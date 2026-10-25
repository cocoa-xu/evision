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

  test "unaligned inputs and selected output survive creator exit and GC with parallel tails" do
    previous_threads = Evision.getNumThreads()
    on_exit(fn -> Evision.setNumThreads(previous_threads) end)

    for mode <- [:baseline, :auto], threads <- [1, 4] do
      Evision.Backend.set_cpu_dispatch(mode)
      Evision.setNumThreads(threads)

      for {kind, bits} <- [{"u", 8}, {"s", 16}, {"f", 16}, {"bf", 16}, {"f", 32}, {"f", 64}],
          storage <- [:binary, :opencv] do
        count = 65_539
        width = div(bits, 8)
        result = select_after_creator_exit(count, kind, bits, storage)
        :erlang.garbage_collect()

        expected =
          for i <- 0..(count - 1), byte <- 0..(width - 1), into: <<>> do
            offset = i * width + byte

            value =
              if rem(i, 4) == 0, do: rem(offset * 19 + 173, 256), else: rem(offset * 37, 256)

            <<value>>
          end

        assert :evision_nif.mat_to_binary(img: result) == expected
      end
    end
  end

  defp select_after_creator_exit(count, kind, bits, storage) do
    owner = self()
    tag = make_ref()

    {pid, monitor} =
      spawn_monitor(fn ->
        width = div(bits, 8)
        mask = for i <- 0..(count - 1), into: <<>>, do: <<Enum.at([0, 1, 128, 255], rem(i, 4))>>
        a = for i <- 0..(count * width - 1), into: <<>>, do: <<rem(i * 37, 256)>>
        b = for i <- 0..(count * width - 1), into: <<>>, do: <<rem(i * 19 + 173, 256)>>

        make = fn binary, type, size, offset ->
          parent = :binary.copy(<<0xCC>>, offset) <> binary <> :binary.copy(<<0xDD>>, 64)
          unaligned = binary_part(parent, offset, byte_size(binary))

          %{ref: ref} =
            :evision_nif.mat_from_binary_by_shape(
              binary: unaligned,
              shape: [count],
              t: type,
              l: size
            )

          if storage == :opencv do
            %{ref: copied} = :evision_nif.mat_clone(img: ref)
            copied
          else
            ref
          end
        end

        refs = {make.(mask, "u", 8, 1), make.(a, kind, bits, 3), make.(b, kind, bits, 5)}
        send(owner, {tag, refs})
      end)

    assert_receive {^tag, {mask, a, b}}, 10_000
    assert_receive {:DOWN, ^monitor, :process, ^pid, :normal}, 10_000
    :erlang.garbage_collect()
    original = Enum.map([mask, a, b], &:evision_nif.mat_to_binary(img: &1))
    %{ref: result} = :evision_nif.mat_select(mask: mask, on_true: a, on_false: b)
    assert Enum.map([mask, a, b], &:evision_nif.mat_to_binary(img: &1)) == original
    result
  end
end
