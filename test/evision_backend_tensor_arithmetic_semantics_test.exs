defmodule Evision.Backend.TensorArithmeticSemanticsTest do
  use ExUnit.Case, async: true

  setup do
    Nx.default_backend(Nx.BinaryBackend)
    :ok
  end

  defp ev(tensor), do: Nx.backend_copy(tensor, Evision.Backend)

  defp assert_same(result, reference) do
    assert %Evision.Backend{} = result.data
    assert Nx.type(result) == Nx.type(reference)
    assert Nx.shape(result) == Nx.shape(reference)
    assert Nx.names(result) == Nx.names(reference)
    assert result.vectorized_axes == reference.vectorized_axes
    assert Nx.to_flat_list(result) == Nx.to_flat_list(reference)
  end

  test "multiply is elementwise for matrices, vectors, and broadcast inputs" do
    for type <- [:f32, :f64] do
      cases = [
        {Nx.tensor([[1, 2], [3, 4]], type: type), Nx.tensor([[2, 3], [4, 5]], type: type)},
        {Nx.tensor([1, 2, 3], type: type), Nx.tensor([4, 5, 6], type: type)},
        {Nx.tensor([[1], [2]], type: type), Nx.tensor([3, 4, 5], type: type)},
        {Nx.iota({2, 1, 3}, type: type), Nx.tensor([2, 3, 4], type: type)}
      ]

      for {a, b} <- cases do
        assert_same(Nx.multiply(ev(a), ev(b)), Nx.multiply(a, b))
      end
    end
  end

  test "floating multiply preserves promotion, scalar order, and special values" do
    values = Nx.tensor([0.1, -2.75, 1.0e-20, 1.0e20], type: :f32, names: [:value])

    for scalar_type <- [:f32, :f64], scalar <- [0.1, -1.5] do
      b = Nx.tensor(scalar, type: scalar_type)
      assert_same(Nx.multiply(ev(values), ev(b)), Nx.multiply(values, b))
      assert_same(Nx.multiply(ev(b), ev(values)), Nx.multiply(b, values))
      assert_same(Nx.multiply(ev(b), ev(b)), Nx.multiply(b, b))
    end

    a = Nx.tensor([[0.1], [-2.75]], type: :f32)
    b = Nx.tensor([0.3, 1.0000000000000002], type: :f64)
    assert_same(Nx.multiply(ev(a), ev(b)), Nx.multiply(a, b))

    for type <- [:f32, :f64] do
      a = Nx.tensor([:nan, :infinity, :neg_infinity, 0.0, -0.0], type: type)
      b = Nx.tensor([1.0, 0.0, -1.0, :infinity, -1.0], type: type)
      assert_same(Nx.multiply(ev(a), ev(b)), Nx.multiply(a, b))

      a = Nx.tensor([-0.0, 0.0], type: type)
      b = Nx.tensor(2.0, type: type)
      assert Nx.to_binary(Nx.multiply(ev(a), ev(b))) == Nx.to_binary(Nx.multiply(a, b))
    end
  end

  test "integer multiply wraps without floating point precision loss" do
    cases = [
      {Nx.tensor([128, 255], type: :u8), Nx.tensor(2, type: :u8)},
      {Nx.tensor([-128, 127], type: :s8), Nx.tensor([-1, 2], type: :s8)},
      {Nx.tensor([32768, 65535], type: :u16), Nx.tensor([2, 2], type: :u16)},
      {Nx.tensor([32767, -32768], type: :s16), Nx.tensor([2, -1], type: :s16)},
      {Nx.tensor([4_294_967_295, 2_147_483_648], type: :u32), Nx.tensor([2, 2], type: :u32)},
      {Nx.tensor([2_147_483_647, -2_147_483_648], type: :s32), Nx.tensor([2, -1], type: :s32)},
      {Nx.tensor([9_007_199_254_740_993], type: :s64), Nx.tensor(3, type: :s64)},
      {Nx.tensor([18_446_744_073_709_551_615], type: :u64), Nx.tensor(2, type: :u64)},
      {Nx.tensor([[128], [255]], type: :u8), Nx.tensor([2, 3], type: :u8)}
    ]

    for {a, b} <- cases do
      assert_same(Nx.multiply(ev(a), ev(b)), Nx.multiply(a, b))
      assert_same(Nx.multiply(ev(b), ev(a)), Nx.multiply(b, a))
    end
  end

  test "multiply fallback preserves mixed and half precision types" do
    cases = [
      {Nx.tensor([9_007_199_254_740_993], type: :s64), Nx.tensor(0.1, type: :f64)},
      {Nx.tensor([16_777_217, -16_777_217], type: :s32), Nx.tensor([0.3, 0.7], type: :f32)},
      {Nx.tensor([1.5, -2.0], type: :f16), Nx.tensor(3.0, type: :f16)},
      {Nx.tensor([1.5, -2.0], type: :bf16), Nx.tensor([3.0, 4.0], type: :bf16)}
    ]

    for {a, b} <- cases do
      assert_same(Nx.multiply(ev(a), ev(b)), Nx.multiply(a, b))
    end
  end

  test "dot honors explicit contracting axes and outer products" do
    a = Nx.tensor([[1, 2], [3, 4]], type: :f32)
    assert_same(Nx.dot(ev(a), [0], ev(a), [0]), Nx.dot(a, [0], a, [0]))
    assert_same(Nx.dot(ev(a), [1], ev(a), [1]), Nx.dot(a, [1], a, [1]))

    a = Nx.tensor([1, 2], type: :f32)
    b = Nx.tensor([3, 4], type: :f32)
    assert_same(Nx.dot(ev(a), [], ev(b), []), Nx.dot(a, [], b, []))
    assert_same(Nx.dot(ev(a), ev(b)), Nx.dot(a, b))
  end

  test "dot handles batches, multiple axes, and integer accumulation" do
    cases = [
      {Nx.iota({2, 3, 4}, type: :f32), [2], [0], Nx.iota({2, 4, 5}, type: :f32), [1], [0]},
      {Nx.iota({2, 3, 4}, type: :f64), [0, 2], [], Nx.iota({4, 2, 5}, type: :f64), [1, 0], []},
      {Nx.tensor([[128, 255]], type: :u8), [1], [], Nx.tensor([[2], [2]], type: :u8), [0], []},
      {Nx.tensor([9_007_199_254_740_993, 1], type: :s64), [0], [], Nx.tensor([1, 2], type: :s64),
       [0], []},
      {Nx.tensor([[1.5, -2.0]], type: :f16), [1], [], Nx.tensor([[2.0], [3.0]], type: :f16), [0],
       []}
    ]

    for {a, ca, ba, b, cb, bb} <- cases do
      assert_same(Nx.dot(ev(a), ca, ba, ev(b), cb, bb), Nx.dot(a, ca, ba, b, cb, bb))
    end
  end

  test "conventional matrix dot preserves mixed floating promotion and names" do
    for left_type <- [:f32, :f64], right_type <- [:f32, :f64] do
      a = Nx.tensor([[1, 2, 3], [4, 5, 6]], type: left_type, names: [:row, :contract])
      b = Nx.tensor([[1, 2], [3, 4], [5, 6]], type: right_type, names: [:contract, :column])
      assert_same(Nx.dot(ev(a), ev(b)), Nx.dot(a, b))
    end
  end

  test "multiply and dot preserve vectorized axes" do
    a = Nx.tensor([[1, 2], [3, 4]], type: :s32) |> Nx.vectorize(:batch)
    b = Nx.tensor([[10, 20], [30, 40]], type: :s32) |> Nx.vectorize(:batch)
    assert_same(Nx.multiply(ev(a), ev(b)), Nx.multiply(a, b))
    assert_same(Nx.dot(ev(a), ev(b)), Nx.dot(a, b))
  end
end
