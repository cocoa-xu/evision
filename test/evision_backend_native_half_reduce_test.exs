defmodule Evision.BackendNativeHalfReduceTest do
  use ExUnit.Case, async: true

  @operations [:sum, :reduce_min, :reduce_max]

  setup do
    Nx.default_backend(Nx.BinaryBackend)
    :ok
  end

  defp same(actual, expected) do
    assert {actual.type, actual.shape, actual.names, actual.vectorized_axes} ==
             {expected.type, expected.shape, expected.names, expected.vectorized_axes}

    assert Nx.to_binary(actual) == Nx.to_binary(expected)
  end

  defp native_value(source, :sum), do: elem(Evision.sumElems(source), 0)
  defp native_value(source, :reduce_min), do: elem(Evision.minMaxLoc(source), 0)
  defp native_value(source, :reduce_max), do: elem(Evision.minMaxLoc(source), 1)

  defp native_bytes(value, type) do
    Nx.tensor(value, type: :f64)
    |> Evision.Mat.from_nx()
    |> Evision.Mat.as_type(type)
    |> Evision.Mat.to_binary()
  end

  test "half reductions accept native input and return the requested type" do
    for type <- [:f16, :bf16] do
      source = Nx.tensor([[1, 2, 3], [4, 5, 6]], type: type, backend: Evision.Backend)
      same(Nx.sum(source), Nx.tensor(21, type: type))
      same(Nx.sum(source, axes: [0]), Nx.tensor([5, 7, 9], type: type))
      same(Nx.reduce_min(source, axes: [1]), Nx.tensor([1, 4], type: type))
      same(Nx.reduce_max(source, axes: [0]), Nx.tensor([4, 5, 6], type: type))
    end
  end

  test "leading, trailing and interleaved axes preserve exact values and metadata" do
    # All intermediate sums here are small exact integers in both half formats.
    for type <- [:f16, :bf16],
        axes <- [nil, [], [0], [1], [2], [0, 1], [1, 2], [0, 2]],
        keep <- [false, true],
        op <- @operations do
      source = Nx.tensor(Enum.map(0..23, &(rem(&1, 7) - 3)), type: type)
      source = Nx.reshape(source, {2, 3, 4}, names: [:batch, :row, :column])
      input = Nx.backend_copy(source, Evision.Backend)
      original = Nx.to_binary(input)
      opts = [axes: axes, keep_axes: keep]
      same(apply(Nx, op, [input, opts]), apply(Nx, op, [source, opts]))
      assert Nx.to_binary(input) == original
    end
  end

  test "global rounding and special values follow native OpenCV" do
    cases = [
      [1.0, 0.00390625],
      [1.0, 0.00048828125, 0.000000059604644775390625],
      [1.0078125, 1.0078125, 1.0078125],
      [0.0, -0.0],
      [-0.0, 0.0],
      [0.000000059604644775390625, -0.000000059604644775390625]
    ]

    for type <- [:f16, :bf16], values <- cases, op <- @operations do
      source = Nx.tensor(values, type: type)
      mat = Evision.Mat.from_nx(source)
      value = native_value(mat, op)
      result = apply(Nx, op, [Nx.backend_copy(source, Evision.Backend)])
      assert result.type == Nx.Type.normalize!(type)

      assert Nx.to_binary(result) == native_bytes(value, type)
    end
  end

  test "nonfinite reductions return tensors without encoding a nonfinite BEAM float" do
    for type <- [:f16, :bf16] do
      for values <- [[:nan, 2.0], [2.0, :nan], [:infinity, :neg_infinity]] do
        input = Nx.tensor(values, type: type, backend: Evision.Backend)
        assert Nx.to_number(Nx.sum(input)) == :nan
      end

      input = Nx.tensor([:infinity, 2.0, :neg_infinity], type: type, backend: Evision.Backend)
      assert Nx.to_number(Nx.reduce_min(input)) == :neg_infinity
      assert Nx.to_number(Nx.reduce_max(input)) == :infinity

      input = Nx.tensor([:nan, :nan], type: type, backend: Evision.Backend)
      assert Nx.to_number(Nx.reduce_min(input)) == :nan
      assert Nx.to_number(Nx.reduce_max(input)) == :nan
    end
  end

  test "vectorized batches and named axes keep independent reductions" do
    for type <- [:f16, :bf16], op <- @operations, keep <- [false, true] do
      source = Nx.tensor(Enum.map(0..23, &(rem(&1, 5) - 2)), type: type)
      source = Nx.reshape(source, {2, 3, 4}, names: [:batch, :row, :column])
      input = source |> Nx.backend_copy(Evision.Backend) |> Nx.vectorize(:batch)

      expected =
        source
        |> Nx.vectorize(:batch)
        |> then(&apply(Nx, op, [&1, [axes: [:row], keep_axes: keep]]))

      same(apply(Nx, op, [input, [axes: [:row], keep_axes: keep]]), expected)
    end
  end

  test "owned mats and unaligned sub-binaries retain their inputs" do
    for type <- [:f16, :bf16] do
      source = Nx.tensor(Enum.map(0..255, &(rem(&1, 5) - 2)), type: type)
      bytes = Nx.to_binary(source)
      storage = <<17, bytes::binary, 19>>
      view = binary_part(storage, 1, byte_size(bytes))
      borrowed = Evision.Mat.from_binary(view, type, 16, 16, 1)

      for mat <- [borrowed, Evision.Mat.clone(borrowed)], axis <- [0, 1], op <- @operations do
        input = Evision.Mat.to_nx(mat, Evision.Backend)
        expected = source |> Nx.reshape({16, 16}) |> then(&apply(Nx, op, [&1, [axes: [axis]]]))
        same(apply(Nx, op, [input, [axes: [axis]]]), expected)
        assert Evision.Mat.to_binary(mat) == bytes
        assert storage == <<17, bytes::binary, 19>>
      end
    end
  end

  test "scalar and singleton reductions preserve output layout and compose" do
    for type <- [:f16, :bf16], shape <- [{}, {1}, {1, 1}, {2, 1, 1}], op <- @operations do
      source = Nx.broadcast(Nx.tensor(3, type: type), shape)
      input = Nx.backend_copy(source, Evision.Backend)
      result = apply(Nx, op, [input])
      same(result, apply(Nx, op, [source]))
      same(Nx.add(result, result), Nx.add(apply(Nx, op, [source]), apply(Nx, op, [source])))
      same(apply(Nx, op, [input, [axes: []]]), source)
    end
  end

  test "half product remains outside this native capability change" do
    for type <- [:f16, :bf16] do
      input = Nx.tensor([[1, 2], [3, 4]], type: type, backend: Evision.Backend)
      assert_raise RuntimeError, ~r/mat_reduce: unsupported depth/, fn -> Nx.product(input) end
    end
  end
end
