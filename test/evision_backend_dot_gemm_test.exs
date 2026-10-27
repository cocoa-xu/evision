defmodule Evision.Backend.DotGemmTest do
  use ExUnit.Case, async: true

  setup do
    Nx.default_backend(Nx.BinaryBackend)
    :ok
  end

  defp ev(t), do: Nx.backend_copy(t, Evision.Backend)

  defp values(shape, type, names \\ nil) do
    Nx.tensor(Enum.map(0..(Nx.size(shape) - 1), &(rem(&1, 13) - 6)), type: type)
    |> Nx.reshape(shape, names: names)
  end

  defp metadata(actual, expected) do
    assert {actual.shape, actual.type, actual.names, actual.vectorized_axes} ==
             {expected.shape, expected.type, expected.names, expected.vectorized_axes}
  end

  defp same(actual, expected) do
    metadata(actual, expected)
    assert Nx.to_flat_list(actual) == Nx.to_flat_list(expected)
  end

  test "matrix products normalize native headers after squeeze" do
    for left_type <- [:f32, :f64], right_type <- [:f32, :f64],
        {left_shape, right_shape} <- [{{2, 3}, {3, 1}}, {{3, 1}, {1, 2}},
                                      {{1, 3}, {3, 2}}, {{1, 1}, {1, 1}}] do
      left = values(left_shape, left_type)
      right = values(right_shape, right_type)
      squeezed = fn tensor ->
        tensor |> Nx.new_axis(2) |> ev() |> Nx.squeeze(axes: [2])
      end

      a = squeezed.(left)
      b = squeezed.(right)
      assert a.data.ref.shape != a.shape or b.data.ref.shape != b.shape
      expected = Nx.dot(left, right)
      native = Evision.Mat.matrix_multiply(
        Evision.Mat.from_nx(Nx.as_type(left, expected.type)),
        Evision.Mat.from_nx(Nx.as_type(right, expected.type)), expected.type)

      actual = Nx.dot(a, b)
      same(actual, expected)
      assert Nx.to_binary(actual) == Evision.Mat.to_binary(native)
      assert Nx.to_binary(a) == Nx.to_binary(left)
      assert Nx.to_binary(b) == Nx.to_binary(right)
    end
  end

  test "ordered contractions, inner and outer products preserve metadata and promotion" do
    cases = [
      {{3}, [0], {3}, [0]},
      {{3}, [], {4}, []},
      {{2, 3}, [0], {2, 4}, [0]},
      {{2, 3}, [1], {4, 3}, [1]},
      {{2, 3, 4}, [2, 0], {2, 4, 5}, [1, 0]},
      {{2, 3, 4}, [0, 2], {4, 2, 5}, [1, 0]},
      {{2, 3, 4}, [0, 1, 2], {4, 2, 3}, [1, 2, 0]},
      {{2, 3, 4}, [], {2, 3}, []},
      {{}, [], {2, 3}, []},
      {{2, 3}, [], {}, []},
      {{}, [], {}, []},
      {{1, 1, 1}, [0, 1, 2], {1, 1, 1}, [2, 0, 1]}
    ]

    for ta <- [:f32, :f64], tb <- [:f32, :f64], {sa, ca, sb, cb} <- cases do
      a = values(sa, ta, Enum.take([:left_a, :left_b, :left_c], tuple_size(sa)))
      b = values(sb, tb, Enum.take([:right_a, :right_b, :right_c], tuple_size(sb)))
      actual = Nx.dot(ev(a), ca, ev(b), cb)
      same(actual, Nx.dot(a, ca, b, cb))
      assert actual.data.ref.shape == if(actual.shape == {}, do: {1, 1}, else: actual.shape)
    end
  end

  defp source(t, storage) do
    binary = Nx.to_binary(t)

    binary =
      case storage do
        {:offset, offset} ->
          parent = :binary.copy(<<31>>, offset) <> binary <> :binary.copy(<<73>>, 128)
          part = binary_part(parent, offset, byte_size(binary))
          assert :binary.referenced_byte_size(part) > byte_size(part)
          part

        _ ->
          binary
      end

    mat = Evision.Mat.from_binary_by_shape(binary, t.type, t.shape)
    mat = if storage in [:owned, :shared], do: Evision.Mat.clone(mat), else: mat
    owner = Evision.Backend.to_nx(mat, t)

    mat =
      if storage == :shared do
        mat |> Evision.Mat.reshape({Nx.size(t)}) |> Evision.Mat.reshape(t.shape)
      else
        mat
      end

    {Evision.Backend.to_nx(mat, t), owner}
  end

  test "flattening and transposes retain immutable binary, owned, shared and offset inputs" do
    for type <- [:f32, :f64],
        storage <- [:binary, :owned, :shared] ++ Enum.map(1..7, &{:offset, &1}) do
      a = values({2, 3, 16}, type)
      b = values({16, 2, 5}, type)
      {left, owner} = source(a, storage)
      {right, other_owner} = source(b, storage)
      same(Nx.dot(left, [0, 2], right, [1, 0]), Nx.dot(a, [0, 2], b, [1, 0]))
      matrix = values({16, 4}, type)
      same(Nx.dot(left, [2], ev(matrix), [0]), Nx.dot(a, [2], matrix, [0]))
      for input <- [left, owner], do: assert(Nx.to_binary(input) == Nx.to_binary(a))
      for input <- [right, other_owner], do: assert(Nx.to_binary(input) == Nx.to_binary(b))
    end
  end

  defp bounded(actual, expected, scale, k) do
    metadata(actual, expected)
    u = if expected.type == {:f, 32}, do: :math.pow(2, -24), else: :math.pow(2, -53)

    for {x, y, sum_abs} <-
          Enum.zip([Nx.to_flat_list(actual), Nx.to_flat_list(expected), Nx.to_flat_list(scale)]) do
      # Allow rounding in both reductions and the final cast, scaled per output.
      bound = 8 * u * k * sum_abs
      assert abs(x - y) <= bound
      assert bound <= 0.001 * sum_abs
    end
  end

  test "fractional finite contractions stay within a per-output rounding bound" do
    for ta <- [:f32, :f64], tb <- [:f32, :f64] do
      a =
        Nx.tensor(Enum.map(0..1541, &(:math.sin(&1 * 0.37) * (1 + rem(&1, 5)) / 7)), type: ta)
        |> Nx.reshape({3, 257, 2})

      b =
        Nx.tensor(Enum.map(0..2055, &(:math.cos(&1 * 0.21) * (1 + rem(&1, 7)) / 9)), type: tb)
        |> Nx.reshape({4, 257, 2})

      expected = Nx.dot(a, [1], b, [1])
      scale = Nx.dot(Nx.abs(a), [1], Nx.abs(b), [1])
      bounded(Nx.dot(ev(a), [1], ev(b), [1]), expected, scale, 257)
    end
  end

  test "cancellation uses an absolute error bound based on the input products" do
    for {type, large} <- [{:f32, 1.0e8}, {:f64, 1.0e16}] do
      a = Nx.tensor(Enum.take(Stream.cycle([large, 1, -large]), 129), type: type)
      b = Nx.broadcast(Nx.tensor(1, type: type), {129})
      bounded(Nx.dot(ev(a), ev(b)), Nx.dot(a, b), Nx.dot(Nx.abs(a), Nx.abs(b)), 129)
    end
  end

  test "special values, overflow, subnormals and zero signs match conventional native GEMM" do
    cases = [
      {[:nan, 1], [1, 1]},
      {[:infinity, :neg_infinity], [1, 1]},
      {[:infinity], [0]},
      {[:neg_infinity], [2]},
      {[-0.0], [1]},
      {[0.0], [-1]}
    ]

    for type <- [:f32, :f64] do
      edge = if type == :f32, do: 3.4e38, else: 1.0e308
      tiny = if type == :f32, do: 1.0e-40, else: 1.0e-310

      for {av, bv} <-
            cases ++ [{[edge, edge], [2, -2]}, {[edge, edge], [2, 2]}, {[tiny, tiny], [0.5, 0.5]}] do
        a = ev(Nx.tensor(av, type: type))
        b = ev(Nx.tensor(bv, type: type))
        actual = Nx.dot(a, b)
        control = Nx.dot(Nx.reshape(a, {1, Nx.size(a)}), Nx.reshape(b, {Nx.size(b), 1}))
        # This control deliberately preserves existing GEMM behavior, including
        # zero signs and finite overflow where BinaryBackend may raise.
        assert Nx.to_binary(actual) == Nx.to_binary(control)
        assert actual.shape == {}
        assert actual.data.ref.shape == {1, 1}
      end
    end
  end

  test "scalar, vector and higher-rank native outputs compose with add and dot" do
    a = values({2, 3, 4}, :f32)
    b = values({4, 5}, :f32)
    result = Nx.dot(ev(a), ev(b))
    reference = Nx.dot(a, b)
    same(Nx.add(result, result), Nx.add(reference, reference))
    weights = values({5, 2}, :f32)
    same(Nx.dot(result, ev(weights)), Nx.dot(reference, weights))
    v = values({4}, :f32)
    vector = Nx.dot(ev(b), [0], ev(v), [0])
    same(Nx.add(vector, 2), Nx.add(Nx.dot(b, [0], v, [0]), 2))
    scalar = Nx.dot(ev(v), ev(v))
    same(Nx.add(scalar, 2), Nx.add(Nx.dot(v, v), 2))
    same(Nx.dot(scalar, [], ev(v), []), Nx.dot(Nx.dot(v, v), [], v, []))
  end

  test "integer operands retain OpenCV's unsupported-type error" do
    for type <- [:u8, :s32, :s64] do
      a = values({2, 3}, type)
      b = values({3, 2}, type)

      {:error, message} =
        Evision.Mat.matrix_multiply(Evision.Mat.from_nx(a), Evision.Mat.from_nx(b), type)

      assert_raise RuntimeError, message, fn -> Nx.dot(ev(a), ev(b)) end
    end
  end

  test "batched dot remains unsupported without a reference fallback" do
    a = values({2, 3, 4}, :f64) |> Nx.vectorize(:batch) |> ev()
    b = values({2, 4, 5}, :f64) |> Nx.vectorize(:batch) |> ev()
    assert_raise FunctionClauseError, fn -> Nx.dot(a, b) end
  end

  test "square matrix contractions use the requested axes" do
    a = Nx.tensor([[1, 2], [3, 4]], type: :f32) |> ev()
    assert Nx.to_flat_list(Nx.dot(a, [0], a, [0])) == [10.0, 14.0, 14.0, 20.0]
    assert Nx.to_flat_list(Nx.dot(a, [1], a, [1])) == [5.0, 11.0, 11.0, 25.0]
  end

  test "result owns its data after the process holding binary inputs exits" do
    parent = self()

    {pid, monitor} =
      spawn_monitor(fn ->
        a = values({2, 3, 4}, :f64) |> ev()
        b = values({4, 5}, :f64) |> ev()
        send(parent, {self(), Nx.dot(a, b)})
      end)

    assert_receive {^pid, result}
    assert_receive {:DOWN, ^monitor, :process, ^pid, :normal}
    :erlang.garbage_collect()
    same(result, Nx.dot(values({2, 3, 4}, :f64), values({4, 5}, :f64)))
  end

  test "2D contractions preserve native special-value bytes and input storage" do
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
        storage <- [:binary, :owned],
        left_axis <- [0, 1],
        right_axis <- [0, 1] do
      raw =
        for word <- words ++ Enum.reverse(words),
            into: <<>>,
            do: <<word::unsigned-native-size(bits)>>

      tensor = Nx.from_binary(raw, type) |> Nx.reshape({4, 4})
      {input, owner} = source(tensor, storage)
      mat = Evision.Backend.from_nx(input)
      transposed = Evision.Mat.transpose(mat, [1, 0], as_shape: {4, 4})
      left = if left_axis == 0, do: transposed, else: mat
      right = if right_axis == 1, do: transposed, else: mat
      expected = Evision.Mat.matrix_multiply(left, right, type) |> Evision.Mat.to_binary()

      assert Nx.to_binary(Nx.dot(input, [left_axis], input, [right_axis])) == expected
      assert Nx.to_binary(input) == raw
      assert Nx.to_binary(owner) == raw
    end
  end
end
