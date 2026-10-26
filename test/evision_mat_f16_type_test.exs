defmodule Evision.MatF16TypeTest do
  use ExUnit.Case, async: false

  setup do
    Nx.default_backend(Nx.BinaryBackend)
    :ok
  end

  defp native(fun, args),
    do: apply(:evision_nif, fun, [args]) |> Evision.Internal.Structurise.to_struct()

  defp same(%Evision.Mat{} = actual, %Evision.Mat{} = expected) do
    assert {actual.type, actual.shape, actual.channels, actual.dims} ==
             {expected.type, expected.shape, expected.channels, expected.dims}

    assert Evision.Mat.to_binary(actual) == Evision.Mat.to_binary(expected)
  end

  test "documented f16 type forms reach native scalar construction" do
    for type <- [:f16, {:f, 16}] do
      assert %Evision.Mat{type: {:f, 16}} = mat = Evision.Mat.number(1.0, type)
      assert Evision.Mat.to_binary(mat) == <<0x3C00::unsigned-native-16>>
    end
  end

  test "f16 fills and constructors preserve OpenCV output" do
    for type <- [:f16, {:f, 16}],
        shape <- [{1, 1}, {2, 3}, {2, 3, 4}],
        value <- [
          0.0,
          -0.0,
          1.0,
          -1.0,
          1.00048828125,
          1.006,
          0.000000059604644775390625,
          65504.0,
          65520.0,
          1.0e20
        ] do
      same(
        Evision.Mat.full(shape, value, type),
        native(:mat_full, shape: Tuple.to_list(shape), number: value, t: "f", l: 16)
      )
    end

    for type <- [:f16, {:f, 16}], shape <- [{1, 1}, {2, 3}, {2, 3, 4}], op <- [:zeros, :ones] do
      nif = if op == :zeros, do: :mat_zeros, else: :mat_ones

      same(
        apply(Evision.Mat, op, [shape, type]),
        native(nif, shape: Tuple.to_list(shape), t: "f", l: 16)
      )
    end

    for type <- [:f16, {:f, 16}] do
      same(Evision.Mat.eye(3, type), native(:mat_eye, n: 3, t: "f", l: 16))

      same(
        Evision.Mat.arange(-4, 5, 1, type),
        native(:mat_arange, from: -4, to: 5, step: 1, t: "f", l: 16)
        |> Evision.Mat.reshape({1, 9})
      )
    end
  end

  test "f16 atom and tuple forms preserve raw payloads and native conversions" do
    raw =
      for word <- [0, 0x8000, 1, 0x8001, 0x7C00, 0xFC00, 0x7E13, 0xFE42],
          into: <<>>,
          do: <<word::unsigned-native-16>>

    for type <- [:f16, {:f, 16}] do
      same(
        Evision.Mat.from_binary_by_shape(raw, type, {2, 4}),
        native(:mat_from_binary_by_shape, binary: raw, shape: [2, 4], t: "f", l: 16)
      )

      same(
        Evision.Mat.from_binary(raw, type, 2, 4, 1),
        native(:mat_from_binary, binary: raw, rows: 2, cols: 4, channels: 1, t: "f", l: 16)
      )
    end

    source = Nx.tensor([1.00048828125, 1.006, 65504, 65520], type: :f64) |> Evision.Mat.from_nx()
    original = Evision.Mat.to_binary(source)
    expected = native(:mat_as_type, img: source.ref, t: "f", l: 16)
    for type <- [:f16, {:f, 16}], do: same(Evision.Mat.as_type(source, type), expected)
    assert Evision.Mat.to_binary(source) == original
  end

  test "Nx half constants use OpenCV encoding for both half types" do
    for {type, kind} <- [{:f16, "f"}, {:bf16, "bf"}], value <- [0.0, -0.0, 1.006, 65504.0] do
      expected = native(:mat_full, shape: [1, 1], number: value, t: kind, l: 16)
      actual = Nx.tensor(value, type: type, backend: Evision.Backend)
      assert Nx.to_binary(actual) == Evision.Mat.to_binary(expected)

      out = Nx.template({2, 3}, type, names: [:row, :column])
      actual = Evision.Backend.constant(out, value, [])
      assert {actual.type, actual.shape, actual.names} == {out.type, out.shape, out.names}

      assert Nx.to_binary(actual) ==
               Evision.Mat.to_binary(
                 native(:mat_full, shape: [2, 3], number: value, t: kind, l: 16)
               )
    end
  end

  test "typed f16 arithmetic preserves native results or errors" do
    source = Evision.Mat.full({2, 3}, 2, :f32)

    for {operation, nif} <- [
          add: :mat_add_typed,
          subtract: :mat_subtract_typed,
          multiply: :mat_multiply_typed,
          divide: :mat_divide_typed
        ],
        type <- [:f16, {:f, 16}] do
      expected = native(nif, lhs: source.ref, rhs: source.ref, t: "f", l: 16)
      actual = apply(Evision.Mat, operation, [source, source, type])

      case expected do
        %Evision.Mat{} -> same(actual, expected)
        {:error, _} -> assert actual == expected
      end
    end
  end

  test "f16 is recognized natively before unsupported-type fallbacks" do
    previous = Application.fetch_env(:evision, :unsupported_type_map)

    on_exit(fn ->
      case previous do
        {:ok, value} -> Application.put_env(:evision, :unsupported_type_map, value)
        :error -> Application.delete_env(:evision, :unsupported_type_map)
      end
    end)

    Application.put_env(:evision, :unsupported_type_map, %{{:f, 16} => {:f, 32}, f16: :f32})
    for type <- [:f16, {:f, 16}], do: assert(Evision.Mat.number(1, type).type == {:f, 16})
  end
end
