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

  test "multiply fallback preserves weak numeric scalar promotion" do
    cases = [
      {:u8, [128, 255], 2},
      {:s8, [-128, 127], 2},
      {:u16, [32768, 65535], 2},
      {:s16, [-32768, 32767], -1},
      {:u32, [4_294_967_295, 2_147_483_648], 2},
      {:s32, [-2_147_483_648, 2_147_483_647], -1},
      {:u64, [18_446_744_073_709_551_615, 9_007_199_254_740_993], 2},
      {:s64, [-9_223_372_036_854_775_808, 9_007_199_254_740_993], 3},
      {:u8, [128, 255], -1},
      {:s8, [-128, 127], 256},
      {:u8, [1, 2], 9_007_199_254_740_993},
      {:f16, [1.5, -2.0], 0.5},
      {:bf16, [1.5, -2.0], 0.5},
      {:f16, [1.5, -2.0], 3},
      {:bf16, [1.5, -2.0], 3}
    ]

    for {type, values, scalar} <- cases do
      tensor = Nx.tensor(values, type: type, names: [:value])
      assert_same(Nx.multiply(ev(tensor), scalar), Nx.multiply(tensor, scalar))
      assert_same(Nx.multiply(scalar, ev(tensor)), Nx.multiply(scalar, tensor))
    end
  end

  test "floating multiplication checks integer scalar precision at mantissa boundaries" do
    scalars = [
      Nx.tensor(2, type: :u8),
      Nx.tensor(-3, type: :s16),
      Nx.tensor(16_777_215, type: :s32),
      Nx.tensor(16_777_216, type: :s32),
      Nx.tensor(16_777_217, type: :s32),
      Nx.tensor(16_777_218, type: :s32),
      Nx.tensor(-16_777_217, type: :s32),
      Nx.tensor(9_007_199_254_740_991, type: :s64),
      Nx.tensor(9_007_199_254_740_992, type: :s64),
      Nx.tensor(9_007_199_254_740_993, type: :s64),
      Nx.tensor(9_007_199_254_740_994, type: :s64),
      Nx.tensor(-9_223_372_036_854_775_808, type: :s64),
      Nx.tensor(18_446_744_073_709_551_615, type: :u64)
    ]

    for type <- [:f32, :f64], scalar <- scalars do
      tensor = Nx.tensor([3.0, -0.1, 1.0000001192092896], type: type, names: [:value])

      for {a, b} <- [{tensor, scalar}, {scalar, tensor}] do
        result = Nx.multiply(ev(a), ev(b))
        expected = Nx.multiply(a, b)
        assert_same(result, expected)
        assert Nx.to_binary(result) == Nx.to_binary(expected)
      end
    end

    # A non-scalar integer operand must not be rounded to f32 before multiplying.
    integer_array = Nx.tensor([16_777_217], type: :s32)
    float_scalar = Nx.tensor(3.0, type: :f32)

    assert_same(
      Nx.multiply(ev(integer_array), ev(float_scalar)),
      Nx.multiply(integer_array, float_scalar)
    )
  end

  test "raw integer scalar multiplication preserves floating zero signs and special values" do
    for type <- [:f32, :f64], scalar <- [0, 1, -1, 2, -3] do
      tensor = Nx.tensor([0.0, -0.0, :infinity, :neg_infinity, :nan], type: type)
      zeros = Nx.tensor([0.0, -0.0], type: type)

      for fun <- [fn t -> Nx.multiply(t, scalar) end, fn t -> Nx.multiply(scalar, t) end] do
        assert_same(fun.(ev(tensor)), fun.(tensor))
        assert Nx.to_binary(fun.(ev(zeros))) == Nx.to_binary(fun.(zeros))
      end
    end
  end

  test "exact integer scalar identity retains native NaN payloads and subnormal bytes" do
    encodings = [
      {:f32, 32, [0, 0x80000000, 0x7F800000, 0xFF800000, 0x7FC00123, 0xFFC00456, 1, 0x80000001]},
      {:f64, 64,
       [
         0,
         0x8000000000000000,
         0x7FF0000000000000,
         0xFFF0000000000000,
         0x7FF8000000000123,
         0xFFF8000000000456,
         1,
         0x8000000000000001
       ]}
    ]

    for {type, width, words} <- encodings do
      raw = for word <- words, into: <<>>, do: <<word::unsigned-native-size(width)>>
      tensor = Nx.from_binary(raw, type) |> ev()
      assert Nx.to_binary(Nx.multiply(tensor, 1)) == raw
      assert Nx.to_binary(Nx.multiply(1, tensor)) == raw
      assert Nx.to_binary(tensor) == raw
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
