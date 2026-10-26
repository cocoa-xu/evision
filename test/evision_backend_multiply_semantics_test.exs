defmodule Evision.Backend.MultiplySemanticsTest do
  use ExUnit.Case, async: true

  setup do
    Nx.default_backend(Nx.BinaryBackend)
    :ok
  end

  defp ev(tensor), do: Nx.backend_copy(tensor, Evision.Backend)
  defp bytes(tensor), do: tensor |> Nx.devectorize() |> Nx.to_binary()

  defp outcome(fun) do
    case fun.() do
      %Nx.Tensor{} = tensor -> {:ok, Nx.type(tensor), bytes(tensor)}
      %Evision.Mat{} = mat -> {:ok, mat.type, Evision.Mat.to_binary(mat)}
      {:error, message} -> raise RuntimeError, message
    end
  rescue
    error -> {:error, error.__struct__, Exception.message(error)}
  end

  defp native_operand(tensor) do
    mat = Evision.Mat.from_nx(tensor)
    if Nx.size(tensor) == 1, do: Evision.Mat.as_type(mat, :f64), else: mat
  end

  test "multiply is elementwise for matrices and vectors" do
    for type <- [:u8, :s16, :f32, :f64],
        {a, b, expected} <- [
          {[[1, 2], [3, 4]], [[2, 3], [4, 5]], [[2, 6], [12, 20]]},
          {[1, 2, 3], [4, 5, 6], [4, 10, 18]}
        ] do
      left = Nx.tensor(a, type: type) |> ev()
      right = Nx.tensor(b, type: type) |> ev()
      result = Nx.multiply(left, right)
      expected = Nx.tensor(expected, type: type)
      assert bytes(result) == bytes(expected)
      assert {result.type, result.shape} == {expected.type, expected.shape}
      assert bytes(left) == bytes(Nx.tensor(a, type: type))
      assert bytes(right) == bytes(Nx.tensor(b, type: type))
    end
  end

  test "integer saturation and wide precision follow direct OpenCV multiplication" do
    for {type, values} <- [
          {:u8, [128, 255]},
          {:s8, [-128, 127]},
          {:u16, [32768, 65535]},
          {:s16, [-32768, 32767]},
          {:u32, [2_147_483_648, 4_294_967_295]},
          {:s32, [-2_147_483_648, 2_147_483_647]},
          {:u64, [9_007_199_254_740_993, 18_446_744_073_709_551_615]},
          {:s64, [-9_223_372_036_854_775_808, 9_007_199_254_740_993]}
        ],
        factor <- [Nx.tensor(2, type: type), Nx.tensor([2, 3], type: type)] do
      array = Nx.tensor(values, type: type)

      for {a, b} <- [{array, factor}, {factor, array}] do
        expected =
          outcome(fn ->
            Evision.Mat.multiply(native_operand(a), native_operand(b), Nx.type(array))
          end)

        left = ev(a)
        right = ev(b)
        assert outcome(fn -> Nx.multiply(left, right) end) == expected
        assert bytes(left) == bytes(a)
        assert bytes(right) == bytes(b)
      end
    end

    array = Nx.tensor([128, 255], type: :u8) |> ev()
    assert bytes(Nx.multiply(array, 2)) == <<255, 255>>
    assert bytes(Nx.multiply(2, array)) == <<255, 255>>
  end

  test "half multiplication preserves the native result or error" do
    for type <- [:f16, :bf16],
        factor <- [Nx.tensor(3.0, type: type), Nx.tensor([3.0, 4.0], type: type)] do
      array = Nx.tensor([1.5, -2.0], type: type)

      for {a, b} <- [{array, factor}, {factor, array}] do
        expected =
          outcome(fn ->
            Evision.Mat.multiply(native_operand(a), native_operand(b), Nx.type(array))
          end)

        assert outcome(fn -> Nx.multiply(ev(a), ev(b)) end) == expected
      end
    end
  end

  test "broadcasting preserves OpenCV saturation and output names" do
    left = Nx.tensor([[128], [255]], type: :u8, names: [:row, nil]) |> ev()
    right = Nx.tensor([2, 3], type: :u8, names: [:column]) |> ev()
    result = Nx.multiply(left, right)
    assert bytes(result) == <<255, 255, 255, 255>>
    assert result.shape == {2, 2}
    assert result.names == [:row, :column]
  end

  test "singleton factors preserve vectorization and expanded native layout" do
    for type <- [:u8, :f32, :f64], axes <- [[], [:batch], [:batch, :inner]] do
      source = Nx.tensor([[[1, 2], [3, 4]], [[5, 6], [7, 8]]], type: type)

      expected =
        Nx.tensor([[[2, 4], [6, 8]], [[10, 12], [14, 16]]], type: type) |> Nx.vectorize(axes)

      input = source |> Nx.vectorize(axes) |> ev()

      for result <- [Nx.multiply(input, 2), Nx.multiply(2, input)] do
        assert bytes(result) == bytes(expected)
        assert result.type == expected.type
        assert result.shape == expected.shape
        assert result.vectorized_axes == expected.vectorized_axes
      end

      assert bytes(input) == bytes(source)
    end

    scalar = Nx.tensor([[[2]]], type: :s32) |> ev()

    for source <- [Nx.tensor(1.25, type: :f32), Nx.tensor([1.0, 2.0, 3.0], type: :f32)] do
      input = ev(source)

      for result <- [Nx.multiply(input, scalar), Nx.multiply(scalar, input)] do
        expected =
          if source.shape == {},
            do: Nx.tensor([[[2.5]]], type: :f32),
            else: Nx.tensor([[[2.0, 4.0, 6.0]]], type: :f32)

        assert bytes(result) == bytes(expected)
        assert result.shape == expected.shape
        assert Evision.Mat.shape(Evision.Backend.from_nx(result)) == result.shape
      end
    end
  end

  test "floating special values and wide integer factors retain native bytes" do
    for {type, bits, words} <- [
          {:f32, 32,
           [0, 0x80000000, 1, 0x80000001, 0x7F800000, 0xFF800000, 0x7FC00123, 0xFFC00456]},
          {:f64, 64,
           [
             0,
             0x8000000000000000,
             1,
             0x8000000000000001,
             0x7FF0000000000000,
             0xFFF0000000000000,
             0x7FF8000000000123,
             0xFFF8000000000456
           ]}
        ],
        factor <- [0, 1, -1, 2, 9_007_199_254_740_993] do
      raw = for word <- words, into: <<>>, do: <<word::unsigned-native-size(bits)>>
      array = Nx.from_binary(raw, type)
      scalar = Nx.tensor(factor, type: :s64)

      for {a, b} <- [{array, scalar}, {scalar, array}] do
        expected =
          outcome(fn ->
            Evision.Mat.multiply(native_operand(a), native_operand(b), Nx.type(array))
          end)

        assert outcome(fn -> Nx.multiply(ev(a), ev(b)) end) == expected
      end
    end
  end

  test "finite f64 overflow retains native infinity for wide integer factors" do
    raw = :binary.copy(<<0x7FEFFFFFFFFFFFFF::unsigned-native-64>>, 4)
    scalar = Nx.tensor(9_007_199_254_740_993, type: :s64) |> ev()
    expected = :binary.copy(<<0x7FF0000000000000::unsigned-native-64>>, 4)
    flat = Nx.from_binary(raw, :f64) |> ev()
    nested = flat |> Nx.reshape({2, 2}) |> Nx.vectorize([:batch, :inner])

    for array <- [flat, nested] do
      assert bytes(Nx.multiply(array, scalar)) == expected
      assert bytes(Nx.multiply(scalar, array)) == expected
      assert bytes(array) == raw
    end
  end
end
