defmodule Evision.CpuDispatchTest do
  use ExUnit.Case, async: false

  setup do
    mode = Evision.Backend.cpu_dispatch_info().mode
    on_exit(fn -> Evision.Backend.set_cpu_dispatch(mode) end)
    :ok
  end

  test "baseline override and automatic dispatch report their actual implementation" do
    baseline = Evision.Backend.set_cpu_dispatch(:baseline)
    assert baseline.mode == :baseline
    assert baseline.select == :baseline

    automatic = Evision.Backend.set_cpu_dispatch(:auto)

    expected =
      cond do
        automatic.compiled_implementations.avx2 and automatic.available_features.avx2 -> :avx2
        automatic.compiled_implementations.neon and automatic.available_features.neon -> :neon
        true -> :baseline
      end

    assert automatic.select == expected
    assert Map.has_key?(automatic.available_features, :neon_fp16)
    assert Map.has_key?(automatic.available_features, :neon_dotprod)
    assert Map.has_key?(automatic.available_features, :neon_bf16)
    assert {:error, _} = :evision_nif.mat_set_cpu_dispatch(mode: "unsupported")
    assert Evision.Backend.cpu_dispatch_info().mode == :auto
  end

  test "select preserves every input byte under both dispatch modes" do
    for mode <- [:baseline, :auto] do
      Evision.Backend.set_cpu_dispatch(mode)

      for {kind, bits} <- [{"u", 8}, {"s", 16}, {"f", 16}, {"bf", 16}, {"f", 32}, {"f", 64}],
          count <- [1, 7, 8, 9, 31, 65, 65_539] do
        width = div(bits, 8)
        mask = for i <- 0..(count - 1), into: <<>>, do: <<Enum.at([0, 1, 128, 255], rem(i, 4))>>
        a = for i <- 0..(count * width - 1), into: <<>>, do: <<rem(i * 37, 256)>>
        b = for i <- 0..(count * width - 1), into: <<>>, do: <<rem(i * 19 + 173, 256)>>

        make = fn binary, type, size ->
          :evision_nif.mat_from_binary_by_shape(binary: binary, shape: [count], t: type, l: size)
        end

        %{ref: mask_ref} = make.(mask, "u", 8)
        %{ref: a_ref} = make.(a, kind, bits)
        %{ref: b_ref} = make.(b, kind, bits)
        %{ref: result} = :evision_nif.mat_select(mask: mask_ref, on_true: a_ref, on_false: b_ref)

        expected =
          for i <- 0..(count - 1), into: <<>> do
            binary_part(if(:binary.at(mask, i) == 0, do: b, else: a), i * width, width)
          end

        assert :evision_nif.mat_to_binary(img: result) == expected
        assert :evision_nif.mat_to_binary(img: a_ref) == a
        assert :evision_nif.mat_to_binary(img: b_ref) == b
      end
    end
  end

  test "automatic dispatch respects OpenCV's enabled feature set" do
    enabled = Evision.useOptimized()

    try do
      Evision.Backend.set_cpu_dispatch(:auto)
      Evision.setUseOptimized(false)
      info = Evision.Backend.cpu_dispatch_info()
      assert info.select == :baseline
      refute info.available_features.neon
      refute info.available_features.avx2
    after
      Evision.setUseOptimized(enabled)
    end
  end

  test "Nx.select matches BinaryBackend in both modes and through broadcast fallbacks" do
    for mode <- [:baseline, :auto] do
      Evision.Backend.set_cpu_dispatch(mode)

      for shape <- [{3, 17}, {129, 513}], broadcast <- [false, true] do
        a = Nx.iota(shape, type: :f32)
        b = if broadcast, do: Nx.tensor(-2.0), else: Nx.negate(a)
        mask = Nx.remainder(Nx.iota(shape), 3) |> Nx.as_type(:u8)
        expected = Nx.select(mask, a, b) |> Nx.to_binary()
        [mask, a, b] = Enum.map([mask, a, b], &Nx.backend_copy(&1, Evision.Backend))
        assert Nx.select(mask, a, b) |> Nx.to_binary() == expected
      end
    end
  end
end
