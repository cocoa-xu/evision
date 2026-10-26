defmodule Evision.BackendMatchingShapeTest do
  use ExUnit.Case, async: true

  @ops [
    :add,
    :subtract,
    :multiply,
    :divide,
    :min,
    :max,
    :equal,
    :not_equal,
    :less,
    :less_equal,
    :greater,
    :greater_equal
  ]
  @storage [:binary, :owned, :shared] ++ Enum.map(1..7, &{:offset, &1})

  setup do
    Nx.default_backend(Nx.BinaryBackend)
    :ok
  end

  defp source(template, storage) do
    binary = Nx.to_binary(template)

    data =
      case storage do
        {:offset, offset} ->
          parent = :binary.copy(<<31>>, offset) <> binary <> :binary.copy(<<73>>, 128)
          part = binary_part(parent, offset, byte_size(binary))
          assert :binary.referenced_byte_size(part) > byte_size(part)
          part

        _ ->
          binary
      end

    mat = Evision.Mat.from_binary_by_shape(data, Nx.type(template), Nx.shape(template))
    mat = if storage in [:owned, :shared], do: Evision.Mat.clone(mat), else: mat
    owner = Evision.Backend.to_nx(mat, template)

    view =
      if storage == :shared do
        mat |> Evision.Mat.reshape([Nx.size(template)]) |> Evision.Mat.reshape(Nx.shape(template))
      else
        mat
      end

    {Evision.Backend.to_nx(view, template), owner}
  end

  defp same(actual, expected) do
    assert {actual.type, actual.shape, actual.names, actual.vectorized_axes} ==
             {expected.type, expected.shape, expected.names, expected.vectorized_axes}

    assert Nx.to_binary(Nx.devectorize(actual)) == Nx.to_binary(Nx.devectorize(expected))
  end

  test "matching float inputs retain immutable binary, owned and shared storage" do
    for type <- [:f32, :f64], storage <- @storage do
      left =
        Nx.tensor(Enum.take(Stream.cycle([1, 2, 4, 8]), 128), type: type)
        |> Nx.reshape({8, 16}, names: [:row, :column])

      right = Nx.divide(left, 2)
      {l, lo} = source(left, storage)
      {r, ro} = source(right, storage)

      for operation <- @ops do
        same(apply(Nx, operation, [l, r]), apply(Nx, operation, [left, right]))
        for input <- [l, lo], do: assert(Nx.to_binary(input) == Nx.to_binary(left))
        for input <- [r, ro], do: assert(Nx.to_binary(input) == Nx.to_binary(right))
      end

      exponent = Nx.broadcast(Nx.tensor(2, type: type), {8, 16})
      same(Nx.pow(l, Nx.backend_copy(exponent, Evision.Backend)), Nx.pow(left, exponent))
      assert Nx.to_binary(lo) == Nx.to_binary(left)
    end
  end

  test "results outlive the process holding borrowed inputs" do
    parent = self()

    {pid, monitor} =
      spawn_monitor(fn ->
        input = Nx.tensor(Enum.to_list(1..128), type: :f64) |> Nx.backend_copy(Evision.Backend)
        send(parent, {self(), Nx.add(input, input)})
      end)

    assert_receive {^pid, result}
    assert_receive {:DOWN, ^monitor, :process, ^pid, :normal}
    :erlang.garbage_collect()
    expected = Nx.tensor(Enum.map(1..128, &(&1 * 2)), type: :f64)
    same(result, expected)
  end

  test "mismatched physical headers still take the repairing reshape path" do
    template = Nx.tensor([[1, 2, 4], [8, 16, 32]], type: :f32, names: [:row, :column])
    flat = Evision.Mat.from_binary_by_shape(Nx.to_binary(template), :f32, {6})
    # Model a backend result with correct logical dimensions and a flat Mat header.
    input = Evision.Backend.to_nx(flat, template)
    other = Nx.backend_copy(template, Evision.Backend)
    assert input.data.ref.shape != template.shape

    for operation <- [:add, :subtract, :less, :greater_equal] do
      actual = apply(Nx, operation, [input, other])
      same(actual, apply(Nx, operation, [template, template]))
      assert actual.data.ref.shape == template.shape
    end

    assert Evision.Mat.to_binary(flat) == Nx.to_binary(template)
  end

  test "foreign backend operands retain the existing conversion path" do
    left = Nx.tensor([[1, 2, 4], [8, 16, 32]], type: :f64)
    right = Nx.divide(left, 2)
    native = Nx.backend_copy(right, Evision.Backend)

    for {l, r, expected} <- [
          {left, native, Nx.add(left, right)},
          {native, left, Nx.add(right, left)}
        ] do
      same(Evision.Backend.add(expected, l, r), expected)
    end

    assert Nx.to_binary(native) == Nx.to_binary(right)
  end

  test "broadcasts, scalar paths and vectorized results compose downstream" do
    left = Nx.tensor([[1], [2]], type: :f32)
    right = Nx.tensor([[2, 4, 8]], type: :f32)

    for operation <- @ops do
      same(
        apply(Nx, operation, [
          Nx.backend_copy(left, Evision.Backend),
          Nx.backend_copy(right, Evision.Backend)
        ]),
        apply(Nx, operation, [left, right])
      )
    end

    source =
      Nx.tensor([[[1, 2, 4]], [[2, 4, 8]]], type: :f32, names: [:batch, :row, :column])
      |> Nx.vectorize(:batch)

    native = Nx.backend_copy(source, Evision.Backend)
    same(Nx.add(native, native), Nx.add(source, source))
    same(Nx.add(native, 2.0), Nx.add(source, 2.0))
    array = Nx.tensor([[1, 2, 4], [2, 4, 8]], type: :f32)
    input = Nx.backend_copy(array, Evision.Backend)
    result = Nx.add(input, input)
    weights = Nx.tensor([[1], [2], [4]], type: :f32)

    same(
      Nx.dot(result, Nx.backend_copy(weights, Evision.Backend)),
      Nx.dot(Nx.add(array, array), weights)
    )
  end
end
