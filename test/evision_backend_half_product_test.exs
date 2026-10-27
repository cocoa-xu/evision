defmodule Evision.BackendHalfProductTest do
  use ExUnit.Case, async: true
  import Bitwise

  setup do
    Nx.default_backend(Nx.BinaryBackend)
    :ok
  end

  defp same(actual, expected) do
    assert {actual.type, actual.shape, actual.names, actual.vectorized_axes} ==
             {expected.type, expected.shape, expected.names, expected.vectorized_axes}

    assert Nx.to_binary(actual) == Nx.to_binary(expected)
  end

  test "half products support whole tensors and both matrix axes" do
    for type <- [:f16, :bf16] do
      input = Nx.tensor([[1, 2, 3], [-1, 0.5, 4]], type: type, backend: Evision.Backend)
      same(Nx.product(input), Nx.tensor(-12, type: type))
      same(Nx.product(input, axes: [0]), Nx.tensor([-1, 1, 12], type: type))
      same(Nx.product(input, axes: [1]), Nx.tensor([6, -2], type: type))
    end
  end

  test "leading, trailing and interleaved products retain output metadata" do
    for type <- [:f16, :bf16],
        axes <- [nil, [], [0], [1], [2], [0, 1], [1, 2], [0, 2]],
        keep <- [false, true] do
      source = Nx.tensor(List.duplicate([0.5, 2, -1, -1], 6) |> List.flatten(), type: type)
      source = Nx.reshape(source, {2, 3, 4}, names: [:batch, :row, :column])
      input = Nx.backend_copy(source, Evision.Backend)
      opts = [axes: axes, keep_axes: keep]
      # Powers of two make every intermediate and final result exact in these cases.
      same(Nx.product(input, opts), Nx.product(source, opts))
      assert Nx.to_binary(input) == Nx.to_binary(source)
    end
  end

  test "products accumulate in f64 and convert the final output with OpenCV" do
    for {type, value, count, numerator, denominator} <- [
          {:f16, 1.0009765625, 3, 1025, 1024},
          {:bf16, 1.0078125, 5, 129, 128}
        ] do
      input = Nx.tensor([List.duplicate(value, count)], type: type) |> Evision.Mat.from_nx()
      wide = Evision.Mat.reduce_rows(input, 1)
      assert wide.type == {:f, 64}
      exact = Integer.pow(numerator, count) / Integer.pow(denominator, count)
      assert Evision.Mat.to_binary(wide) == <<exact::float-native-64>>
    end

    for type <- [:f16, :bf16] do
      source = Nx.tensor([1.0078125, 1.0078125, 1.0078125, -0.5], type: type)
      input = Nx.backend_copy(source, Evision.Backend)

      expected =
        input
        |> Nx.as_type(:f64)
        |> Nx.product()
        |> Evision.Mat.from_nx()
        |> Evision.Mat.as_type(type)

      assert Nx.to_binary(Nx.product(input)) == Evision.Mat.to_binary(expected)

      # Reassociation would recover a finite value instead of preserving sequential overflow/underflow.
      underflow =
        Nx.tensor(List.duplicate(0.5, 1100) ++ List.duplicate(2.0, 1100),
          type: type,
          backend: Evision.Backend
        )

      overflow =
        Nx.tensor(List.duplicate(2.0, 1100) ++ List.duplicate(0.5, 1100),
          type: type,
          backend: Evision.Backend
        )

      assert Nx.to_binary(Nx.product(underflow)) == <<0::unsigned-native-16>>
      assert Nx.to_number(Nx.product(overflow)) == :infinity
    end
  end

  test "IEEE zeros, infinities and NaNs survive tensor output" do
    for type <- [:f16, :bf16] do
      input =
        Nx.tensor([[-0.0, 2], [-0.0, -2], [0.0, -2], [-0.0, -0.0]],
          type: type,
          backend: Evision.Backend
        )

      assert Nx.to_binary(Nx.product(input, axes: [1])) ==
               <<0x8000::unsigned-native-16, 0::unsigned-native-16, 0x8000::unsigned-native-16,
                 0::unsigned-native-16>>

      for values <- [[:nan, 1.0], [1.0, :nan], [0.0, :infinity], [:infinity, 0.0]] do
        assert Nx.to_number(Nx.product(Nx.tensor(values, type: type, backend: Evision.Backend))) ==
                 :nan
      end

      assert Nx.to_number(
               Nx.product(Nx.tensor([:infinity, -2], type: type, backend: Evision.Backend))
             ) == :neg_infinity
    end
  end

  test "vectorized batches keep independent products and named axes" do
    for type <- [:f16, :bf16], axes <- [nil, [:row], [:column]], keep <- [false, true] do
      source = Nx.tensor(List.duplicate([0.5, 2, -1, -1], 6) |> List.flatten(), type: type)
      source = Nx.reshape(source, {2, 3, 4}, names: [:batch, :row, :column])
      input = source |> Nx.backend_copy(Evision.Backend) |> Nx.vectorize(:batch)
      expected = source |> Nx.vectorize(:batch) |> Nx.product(axes: axes, keep_axes: keep)
      same(Nx.product(input, axes: axes, keep_axes: keep), expected)
    end
  end

  test "owned and offset binary inputs stay unchanged" do
    for type <- [:f16, :bf16] do
      unit = Nx.tensor([0.5, 2, -1, -1], type: type) |> Nx.to_binary()
      bytes = :binary.copy(unit, 64)
      storage = <<17, bytes::binary, 19>>
      view = binary_part(storage, 1, byte_size(bytes))
      borrowed = Evision.Mat.from_binary(view, type, 16, 16, 1)

      for mat <- [borrowed, Evision.Mat.clone(borrowed)], axis <- [0, 1] do
        input = Evision.Mat.to_nx(mat)
        source = Nx.from_binary(bytes, type) |> Nx.reshape({16, 16})
        same(Nx.product(input, axes: [axis]), Nx.product(source, axes: [axis]))
        assert Evision.Mat.to_binary(mat) == bytes
        assert storage == <<17, bytes::binary, 19>>
      end
    end
  end

  test "scalar, singleton and identity products keep their layout" do
    for type <- [:f16, :bf16], shape <- [{}, {1}, {1, 1}, {2, 1, 1}] do
      source = Nx.broadcast(Nx.tensor(-2, type: type), shape)
      input = Nx.backend_copy(source, Evision.Backend)
      result = Nx.product(input)
      same(result, Nx.product(source))
      same(Nx.add(result, result), Nx.add(Nx.product(source), Nx.product(source)))
      same(Nx.product(input, axes: []), source)
    end
  end

  test "every half encoding survives a singleton product or becomes a quiet NaN" do
    bytes = for word <- 0..65_535, into: <<>>, do: <<word::unsigned-native-16>>

    for {type, exponent, fraction, quiet} <- [
          {:f16, 0x7C00, 0x3FF, 0x200},
          {:bf16, 0x7F80, 0x7F, 0x40}
        ] do
      input =
        Nx.from_binary(bytes, type) |> Nx.reshape({65_536, 1}) |> Nx.backend_copy(Evision.Backend)

      result = Nx.product(input, axes: [1]) |> Nx.to_binary()
      words = for <<word::unsigned-native-16 <- result>>, do: word

      for {actual, original} <- Enum.with_index(words) do
        if (original &&& exponent) == exponent and (original &&& fraction) != 0 do
          assert (actual &&& exponent) == exponent
          assert (actual &&& quiet) != 0
        else
          assert actual == original
        end
      end

      assert Nx.to_binary(input) == bytes
    end
  end
end
