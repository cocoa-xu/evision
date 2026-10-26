defmodule Evision.BackendScalarComparisonTest do
  use ExUnit.Case, async: true

  @types [:u8, :s8, :u16, :s16, :u32, :s32, :u64, :s64, :f16, :bf16, :f32, :f64]
  @operations [
    equal: :eq,
    not_equal: :ne,
    less: :lt,
    less_equal: :le,
    greater: :gt,
    greater_equal: :ge
  ]

  setup do
    Nx.default_backend(Nx.BinaryBackend)
    :ok
  end

  defp scalar(tensor, shape) do
    tensor
    |> Nx.to_binary()
    |> Evision.Mat.from_binary_by_shape(tensor.type, shape)
    |> Evision.Backend.to_nx(tensor)
  end

  defp native_operand(tensor, type) do
    mat = tensor.data.ref
    mat = if tensor.type == type, do: mat, else: Evision.Mat.as_type(mat, type)
    if tensor.shape == {}, do: Evision.Mat.reshape(mat, {1}), else: mat
  end

  defp compare(left, right) do
    type = Nx.Type.merge(left.type, right.type)
    shape = if left.shape == {}, do: right.shape, else: left.shape
    physical_shape = if shape == {}, do: {1}, else: shape
    originals = {Nx.to_binary(left), Nx.to_binary(right)}

    for {operation, predicate} <- @operations do
      expected =
        Evision.Mat.cmp(native_operand(left, type), native_operand(right, type), predicate)

      assert %Evision.Mat{} = expected
      actual = apply(Nx, operation, [left, right])
      assert actual.type == {:u, 8}
      assert actual.shape == shape
      assert actual.data.ref.shape == physical_shape
      assert Nx.to_binary(actual) == Evision.Mat.to_binary(expected)
      assert {Nx.to_binary(left), Nx.to_binary(right)} == originals
    end
  end

  test "equal scalars compare across direct and copied construction" do
    direct = Nx.tensor(1, type: :f32, backend: Evision.Backend)
    copied = Nx.tensor(1, type: :f32) |> Nx.backend_copy(Evision.Backend)
    assert direct.data.ref.shape == {1, 1}
    assert copied.data.ref.shape == {1}
    assert Nx.to_binary(Nx.equal(direct, copied)) == <<1>>
    assert Nx.to_binary(Nx.equal(copied, direct)) == <<1>>
    assert Nx.equal(direct, direct).data.ref.shape == {1}
    assert {:error, _} = Evision.Mat.cmp(direct.data.ref, copied.data.ref, :eq)
  end

  test "all scalar header pairs preserve native comparison semantics" do
    for type <- @types, a <- [{1}, {1, 1}, {1, 1, 1}], b <- [{1}, {1, 1}, {1, 1, 1}] do
      compare(scalar(Nx.tensor(1, type: type), a), scalar(Nx.tensor(2, type: type), b))
    end
  end

  test "higher-rank scalar headers compare with arrays in both orders" do
    for type <- @types, shape <- [{6}, {2, 3}, {2, 1, 3}], header <- [{1}, {1, 1}, {1, 1, 1}] do
      array =
        Nx.tensor([0, 1, 2, 3, 2, 1], type: type)
        |> Nx.reshape(shape)
        |> Nx.backend_copy(Evision.Backend)

      singleton = scalar(Nx.tensor(2, type: type), header)
      compare(array, singleton)
      compare(singleton, array)
    end
  end

  test "special values and existing mixed-type conversions retain OpenCV results" do
    for type <- [:f16, :bf16, :f32, :f64], value <- [:nan, :infinity, :neg_infinity, -0.0, 0.0] do
      array =
        Nx.tensor([:nan, :infinity, :neg_infinity, -0.0, 0.0, 1.0], type: type)
        |> Nx.backend_copy(Evision.Backend)

      singleton = scalar(Nx.tensor(value, type: type), {1, 1, 1})
      compare(array, singleton)
      compare(singleton, array)
    end

    for {type, value, scalar_type, scalar_value} <- [
          {:u64, 18_446_744_073_709_551_615, :u64, 9_007_199_254_740_993},
          {:s64, -9_007_199_254_740_993, :s64, -9_007_199_254_740_992},
          {:s32, 16_777_217, :f64, 16_777_216.0},
          {:u32, 16_777_217, :f32, 16_777_216.0}
        ] do
      array = Nx.tensor(List.duplicate(value, 8), type: type) |> Nx.backend_copy(Evision.Backend)
      singleton = scalar(Nx.tensor(scalar_value, type: scalar_type), {1, 1, 1})
      compare(array, singleton)
      compare(singleton, array)
    end
  end

  test "named vectorized comparisons retain metadata and composable masks" do
    array =
      Nx.tensor([[1, 2, 3], [3, 2, 1]], type: :f32, names: [:batch, :value])
      |> Nx.backend_copy(Evision.Backend)
      |> Nx.vectorize(:batch)

    singleton = Nx.tensor(2, type: :f32, backend: Evision.Backend)
    actual = Nx.less(array, singleton)

    assert {actual.type, actual.shape, actual.names, actual.vectorized_axes} ==
             {{:u, 8}, {3}, [:value], [batch: 2]}

    assert Nx.to_binary(actual) == <<1, 0, 0, 0, 0, 1>>
    assert Nx.to_binary(Nx.greater(singleton, array)) == Nx.to_binary(actual)
    mask = Nx.devectorize(actual).data.ref
    assert mask.shape == {2, 3}
    assert %Evision.Mat{} = Evision.Mat.add(mask, mask)
  end
end
