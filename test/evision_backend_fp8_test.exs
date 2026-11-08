defmodule Evision.BackendFP8Test do
  use ExUnit.Case, async: true

  @fp8 {:f8_e4m3fn, 8}
  @arithmetic [:add, :subtract, :multiply, :divide]
  @comparisons [:equal, :not_equal, :less, :less_equal, :greater, :greater_equal]

  setup do
    Nx.default_backend(Nx.BinaryBackend)
    :ok
  end

  defp fp8(bytes, shape) do
    bytes
    |> Nx.from_binary(:f8_e4m3fn, backend: Evision.Backend)
    |> Nx.reshape(shape)
  end

  defp oracle_input(tensor, type) do
    tensor = Nx.backend_copy(tensor, Nx.BinaryBackend)
    tensor = if tensor.type == @fp8, do: Nx.as_type(tensor, :f32), else: tensor
    Nx.as_type(tensor, type)
  end

  defp work_type(tensors, out_type) do
    types = [out_type | Enum.map(tensors, & &1.type)]
    if {:f, 64} in types, do: :f64, else: :f32
  end

  # Both results pass through OpenCV's encoder, then compare as f32. Nx's FP8
  # encoder has different rounding, so it must never encode the expected values.
  defp narrowed_f32(tensor, type) do
    tensor
    |> Nx.backend_copy(Evision.Backend)
    |> Nx.as_type(type)
    |> Nx.as_type(:f32)
    |> Nx.backend_copy(Nx.BinaryBackend)
  end

  defp assert_result(actual, expected, type) do
    assert actual.type == type
    assert actual.data.ref.type == type

    assert {actual.shape, actual.names, actual.vectorized_axes} ==
             {expected.shape, expected.names, expected.vectorized_axes}

    left = actual |> narrowed_f32(type) |> Nx.to_flat_list()
    right = expected |> narrowed_f32(type) |> Nx.to_flat_list()

    for {a, b} <- Enum.zip(left, right) do
      assert_in_delta a, b, 1.0e-6 * max(1.0, abs(b))
    end
  end

  defp assert_binary(operation, left, right) do
    # Merge only to check Nx's requested output, never to choose the compute type.
    type = Nx.Type.merge(left.type, right.type)
    work = work_type([left, right], type)
    original = {Nx.to_binary(left), Nx.to_binary(right)}
    expected = apply(Nx, operation, [oracle_input(left, work), oracle_input(right, work)])
    assert_result(apply(Nx, operation, [left, right]), expected, type)
    assert {Nx.to_binary(left), Nx.to_binary(right)} == original
  end

  test "arithmetic handles arrays and scalars in both orders" do
    left = fp8(<<0x39, 0xC2, 0x44, 0x30>>, {2, 2})
    right = fp8(<<0x30, 0x3C, 0x40, 0x48>>, {2, 2})
    scalar = fp8(<<0x3C>>, {})

    for operation <- @arithmetic,
        {a, b} <- [{left, right}, {left, scalar}, {scalar, left}, {scalar, scalar}] do
      assert_binary(operation, a, b)
    end
  end

  test "arithmetic keeps mixed f32 and f64 operands in both orders" do
    input = fp8(<<0x39, 0xC2, 0x44, 0x30>>, {2, 2})

    for type <- [:f32, :f64] do
      array = Nx.tensor([[0.25, 1.5], [2.0, 4.0]], type: type, backend: Evision.Backend)
      scalar = Nx.tensor(1.25, type: type, backend: Evision.Backend)

      for operation <- @arithmetic,
          {a, b} <- [{input, array}, {array, input}, {input, scalar}, {scalar, input}] do
        assert_binary(operation, a, b)
      end
    end
  end

  test "f64 compute survives cancellation even when Nx requests FP8 output" do
    one = fp8(<<0x38>>, {})
    almost_minus_one = Nx.tensor(-0.9990234275, type: :f64, backend: Evision.Backend)

    assert_binary(:add, one, almost_minus_one)
    assert_binary(:add, almost_minus_one, one)

    out = %{one | type: {:f, 64}}
    expected = Nx.add(oracle_input(one, :f64), oracle_input(one, :f64))
    assert_result(Evision.Backend.add(out, one, one), expected, {:f, 64})
  end

  test "min and max handle arrays, scalars and mixed types" do
    input = fp8(<<0xBC, 0x30, 0x40, 0x44>>, {4})
    other = fp8(<<0x38, 0x00, 0x44, 0x40>>, {4})
    scalar = fp8(<<0x3C>>, {})
    wide = Nx.tensor([0.25, 1.25, 2.25, 3.25], type: :f64, backend: Evision.Backend)
    pairs = [{input, other}, {input, scalar}, {scalar, input}, {input, wide}, {wide, input}]

    for operation <- [:min, :max], {a, b} <- pairs do
      assert_binary(operation, a, b)
    end
  end

  test "all six comparisons widen arrays and either scalar operand" do
    input = fp8(<<0xBC, 0x00, 0x38, 0x40>>, {4})
    other = fp8(<<0xBC, 0x38, 0x00, 0x40>>, {4})
    scalar = fp8(<<0x38>>, {})
    wide = Nx.tensor(1.00000001, type: :f64, backend: Evision.Backend)
    pairs = [{input, other}, {input, scalar}, {scalar, input}, {input, wide}, {wide, input}]

    for operation <- @comparisons, {a, b} <- pairs do
      work = work_type([a, b], {:u, 8})
      expected = apply(Nx, operation, [oracle_input(a, work), oracle_input(b, work)])
      assert_result(apply(Nx, operation, [a, b]), expected, {:u, 8})
    end
  end

  test "clip widens its input and bounds before narrowing" do
    input = fp8(<<0xC4, 0xBC, 0x30, 0x44>>, {4})
    lower = fp8(<<0xB8>>, {})
    upper = fp8(<<0x40>>, {})
    expected = Nx.clip(oracle_input(input, :f32), -1.0, 2.0)
    assert_result(Nx.clip(input, lower, upper), expected, @fp8)

    wide = Nx.tensor([-3.0, -1.5, 0.5, 3.0], type: :f64, backend: Evision.Backend)
    type = Nx.Type.merge(Nx.Type.merge(wide.type, lower.type), upper.type)
    expected = Nx.clip(oracle_input(wide, :f64), -1.0, 2.0)
    assert_result(Nx.clip(wide, lower, upper), expected, type)
  end

  test "abs, negate and sign compute on widened values" do
    input = fp8(<<0xC2, 0xBC, 0x80, 0x00, 0x30, 0x42>>, {6})
    original = Nx.to_binary(input)

    for operation <- [:abs, :negate, :sign] do
      expected = apply(Nx, operation, [oracle_input(input, :f32)])
      assert_result(apply(Nx, operation, [input]), expected, @fp8)
      assert Nx.to_binary(input) == original
    end
  end

  test "ceil, floor and round handle positive and negative half-integers" do
    input = fp8(<<0x3C, 0x42, 0xBC, 0xC2>>, {4})
    original = Nx.to_binary(input)

    for operation <- [:ceil, :floor, :round] do
      expected = apply(Nx, operation, [oracle_input(input, :f32)])
      assert_result(apply(Nx, operation, [input]), expected, @fp8)
      assert Nx.to_binary(input) == original
    end
  end

  test "exp, expm1 and log narrow their widened results" do
    input = fp8(<<0x28, 0x30, 0x38, 0x40>>, {4})

    for operation <- [:exp, :expm1, :log] do
      expected = apply(Nx, operation, [oracle_input(input, :f32)])
      assert_result(apply(Nx, operation, [input]), expected, @fp8)
    end
  end

  test "sum, product and extrema reduce globally and along axes" do
    input = fp8(<<0x38, 0x39, 0x3C, 0x30, 0x40, 0x44>>, {2, 3})

    for operation <- [:sum, :product, :reduce_min, :reduce_max],
        opts <- [[], [axes: [0]], [axes: [1], keep_axes: true], [axes: []]] do
      expected = apply(Nx, operation, [oracle_input(input, :f32), opts])
      assert_result(apply(Nx, operation, [input, opts]), expected, @fp8)
    end
  end

  test "shared low-precision math, scans and predicates accept FP8" do
    input = fp8(<<0x00, 0x30, 0x38, 0x40>>, {4})
    source = oracle_input(input, :f32)

    for operation <- [:sqrt, :cumulative_sum, :cumulative_product] do
      expected = apply(Nx, operation, [source])
      assert_result(apply(Nx, operation, [input]), expected, @fp8)
    end

    assert_binary(:pow, input, fp8(<<0x40>>, {}))

    for operation <- [:is_nan, :is_infinity, :logical_not, :all, :any] do
      expected = apply(Nx, operation, [source])
      assert_result(apply(Nx, operation, [input]), expected, {:u, 8})
    end
  end

  test "dot widens before GEMM and preserves contraction axes" do
    left = fp8(<<0x38, 0x3C, 0x40, 0x30>>, {2, 2})
    right = fp8(<<0x40, 0x38, 0x30, 0x44>>, {2, 2})
    wide = Nx.tensor([[0.25, 1.25], [2.25, 3.25]], type: :f64, backend: Evision.Backend)

    for {a, b} <- [{left, right}, {left, wide}, {wide, left}] do
      assert_binary(:dot, a, b)
    end

    expected = Nx.dot(oracle_input(left, :f32), [0], oracle_input(right, :f32), [1])
    assert_result(Nx.dot(left, [0], right, [1]), expected, @fp8)
  end

  test "sort and argsort retain stable ascending and descending ordering" do
    input = fp8(<<0x40, 0x38, 0x38, 0xB8, 0x44, 0x00>>, {2, 3})

    for direction <- [:asc, :desc] do
      opts = [axis: 1, direction: direction]
      source = oracle_input(input, :f32)
      assert_result(Nx.sort(input, opts), Nx.sort(source, opts), @fp8)
      expected = Nx.argsort(source, opts)
      assert_result(Nx.argsort(input, opts), expected, expected.type)
    end
  end

  test "native argmin and argmax handle global and axis reductions with ties" do
    input = fp8(<<0x40, 0x38, 0x38, 0x40, 0x44, 0x44>>, {2, 3})

    for operation <- [:argmin, :argmax],
        opts <- [[tie_break: :low], [tie_break: :high], [axis: 1, tie_break: :high]] do
      expected = apply(Nx, operation, [oracle_input(input, :f32), opts])
      assert_result(apply(Nx, operation, [input, opts]), expected, expected.type)
    end
  end

  test "data movement preserves both signed zeros and both NaN encodings" do
    bytes = <<0x00, 0x80, 0x7F, 0xFF>>
    input = fp8(bytes, {2, 2})
    flat = Nx.reshape(input, {4})
    padded = fp8(<<0x38, bytes::binary, 0x40>>, {6})

    results = [
      {flat, bytes},
      {Nx.transpose(input), <<0x00, 0x7F, 0x80, 0xFF>>},
      {Nx.slice(padded, [1], [4]), bytes},
      {Nx.concatenate([Nx.slice(flat, [0], [2]), Nx.slice(flat, [2], [2])]), bytes}
    ]

    for {result, expected} <- results do
      assert result.type == @fp8
      assert result.data.ref.type == @fp8
      assert Nx.to_binary(result) == expected
    end

    assert Nx.to_binary(input) == bytes
  end

  test "bitwise operations still reject FP8" do
    input = fp8(<<0x38, 0x40>>, {2})
    assert_raise ArgumentError, fn -> Nx.bitwise_and(input, input) end
  end
end
