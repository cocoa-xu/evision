defmodule Evision.BackendMinMaxTypeTest do
  use ExUnit.Case, async: true

  @types [:u8, :s8, :u16, :s16, :u32, :s32, :u64, :s64, :f16, :bf16, :f32, :f64]

  setup do
    Nx.default_backend(Nx.BinaryBackend)
    :ok
  end

  defp native(tensor, shape, type) do
    tensor
    |> Nx.broadcast(shape)
    |> Evision.Mat.from_nx()
    |> Evision.Mat.as_type(type)
    |> Evision.Mat.reshape(shape)
  end

  defp compare(left, right, shape) do
    type = Nx.Type.merge(left.type, right.type)
    a = Nx.backend_copy(left, Evision.Backend)
    b = Nx.backend_copy(right, Evision.Backend)
    originals = {Nx.to_binary(a), Nx.to_binary(b)}

    for op <- [:min, :max] do
      expected = apply(Evision, op, [native(left, shape, type), native(right, shape, type)])
      assert %Evision.Mat{} = expected
      actual = apply(Nx, op, [a, b])
      assert {actual.type, actual.shape, actual.data.ref.shape} == {type, shape, shape}
      assert Nx.to_binary(actual) == Evision.Mat.to_binary(expected)
      assert {Nx.to_binary(a), Nx.to_binary(b)} == originals
    end
  end

  test "mixed signed and unsigned arrays use the requested output type" do
    a = Nx.tensor([255, 0], type: :u8, backend: Evision.Backend)
    b = Nx.tensor([127, -1], type: :s8, backend: Evision.Backend)

    for {left, right} <- [{a, b}, {b, a}] do
      maximum = Nx.max(left, right)
      minimum = Nx.min(left, right)
      assert maximum.type == {:s, 16}
      assert minimum.type == {:s, 16}
      assert Nx.to_binary(maximum) == <<255::signed-native-16, 0::signed-native-16>>
      assert Nx.to_binary(minimum) == <<127::signed-native-16, -1::signed-native-16>>
    end
  end

  test "all real type pairs retain native conversion and selection behavior" do
    for lt <- @types, rt <- @types do
      left = Nx.tensor([0, 1, 3], type: lt)
      right = Nx.tensor([3, 2, 0], type: rt)
      compare(left, right, {3})
      compare(Nx.reshape(left, {3, 1}), Nx.reshape(right, {1, 3}), {3, 3})
      compare(Nx.reshape(left, {1, 1, 3}), Nx.reshape(right, {1, 1, 3}), {1, 1, 3})
    end
  end

  test "integer bounds and floating special values follow OpenCV casts" do
    for bits <- [8, 16, 32, 64] do
      unsigned = Nx.tensor([0, Integer.pow(2, bits) - 1, 1], type: {:u, bits})

      signed =
        Nx.tensor([-Integer.pow(2, bits - 1), Integer.pow(2, bits - 1) - 1, -1], type: {:s, bits})

      compare(unsigned, signed, {3})
      compare(signed, unsigned, {3})
    end

    for lt <- [:f16, :bf16, :f32, :f64], rt <- [:f16, :bf16, :f32, :f64] do
      left = Nx.tensor([:nan, :infinity, :neg_infinity, 0.0, -0.0, 1.006], type: lt)
      right = Nx.tensor([1.0, :neg_infinity, :infinity, -0.0, 0.0, :nan], type: rt)
      compare(left, right, {6})
      compare(right, left, {6})
    end
  end

  test "singleton broadcast operands preserve the requested dtype" do
    left = Nx.tensor([255, 0, 128], type: :u8)
    right = Nx.tensor([127], type: :s8) |> Nx.reshape({1, 1, 1})
    compare(left, right, {1, 1, 3})
    compare(right, left, {1, 1, 3})
  end

  test "named vectorized outputs keep their template" do
    left =
      Nx.tensor([[255, 0], [1, 128]], type: :u8, names: [:batch, :value])
      |> Nx.backend_copy(Evision.Backend)
      |> Nx.vectorize(:batch)

    right =
      Nx.tensor([[127, -1], [-2, 100]], type: :s8, names: [:batch, :value])
      |> Nx.backend_copy(Evision.Backend)
      |> Nx.vectorize(:batch)

    result = Nx.max(left, right)

    assert {result.type, result.shape, result.names, result.vectorized_axes} ==
             {{:s, 16}, {2}, [:value], [batch: 2]}

    assert Nx.devectorize(result).data.ref.shape == {2, 2}

    assert Nx.to_binary(result) ==
             <<255::signed-native-16, 0::signed-native-16, 1::signed-native-16,
               128::signed-native-16>>
  end

  test "vectorized raw numbers use the supplied output type and native casts" do
    for type <- [:u8, :u32, :f16, :bf16, :f32],
        number <- [-1, 2, 300, 3_000_000_000, 1.25],
        op <- [:min, :max] do
      source = Nx.tensor([[0, 255], [1, 128]], type: type, names: [:batch, :value])
      array = source |> Nx.backend_copy(Evision.Backend) |> Nx.vectorize(:batch)
      out_type = Nx.Type.merge_number(source.type, number)
      a = native(source, {2, 2}, out_type)
      b = native(Nx.tensor(number), {2, 2}, out_type)

      for {args, mats} <- [{[array, number], [a, b]}, {[number, array], [b, a]}] do
        actual = apply(Nx, op, args)
        expected = apply(Evision, op, mats)

        assert {actual.type, actual.shape, actual.names, actual.vectorized_axes} ==
                 {out_type, {2}, [:value], [batch: 2]}

        assert Nx.devectorize(actual).data.ref.shape == {2, 2}
        assert Nx.to_binary(actual) == Evision.Mat.to_binary(expected)
      end
    end
  end
end
