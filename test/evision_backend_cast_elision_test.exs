defmodule Evision.BackendCastElisionTest do
  use ExUnit.Case, async: true

  @storage [:binary, :owned, :alias] ++ Enum.map(1..7, &{:offset, &1})
  @integer_types [:u8, :s8, :u16, :s16, :u32, :s32, :u64, :s64]

  setup do
    Nx.default_backend(Nx.BinaryBackend)
    :ok
  end

  defp input(template, storage) do
    binary = Nx.to_binary(template)

    binary =
      case storage do
        {:offset, offset} ->
          parent = :binary.copy(<<123>>, offset) <> binary <> :binary.copy(<<231>>, 128)
          part = binary_part(parent, offset, byte_size(binary))
          assert :binary.referenced_byte_size(part) > byte_size(part)
          part

        _ ->
          binary
      end

    mat = Evision.Mat.from_binary_by_shape(binary, Nx.type(template), Nx.shape(template))
    mat = if storage in [:owned, :alias], do: Evision.Mat.clone(mat), else: mat
    owner = Evision.Backend.to_nx(mat, template)

    derived =
      if storage == :alias do
        flat = Evision.Mat.reshape(mat, [Nx.size(template)])
        Evision.Mat.reshape(flat, Tuple.to_list(Nx.shape(template)))
      else
        mat
      end

    {Evision.Backend.to_nx(derived, template), owner}
  end

  defp assert_same(actual, expected) do
    assert Nx.type(actual) == Nx.type(expected)
    assert Nx.shape(actual) == Nx.shape(expected)
    assert Nx.to_binary(actual) == Nx.to_binary(expected)
  end

  defp assert_operation(template, storage, fun) do
    {tensor, owner} = input(template, storage)
    expected = fun.(template)
    assert_same(fun.(tensor), expected)
    assert Nx.to_binary(tensor) == Nx.to_binary(template)
    assert Nx.to_binary(owner) == Nx.to_binary(template)
  end

  test "same-type numeric inputs keep alignment and immutable owners" do
    for type <- [:f16, :bf16, :f32, :f64], storage <- @storage do
      source = Nx.tensor(List.duplicate([1, 4, 16, 64], 32), type: type)
      assert_operation(source, storage, &Nx.sqrt/1)
      assert_operation(source, storage, &Nx.rsqrt/1)
    end

    for type <- @integer_types, storage <- @storage do
      source = Nx.tensor(List.duplicate([0, 1, 4, 9], 32), type: type)
      assert_operation(source, storage, &Nx.left_shift(&1, Nx.tensor(1, type: type)))
      assert_operation(source, storage, &Nx.quotient(&1, Nx.tensor(2, type: type)))
    end
  end

  test "writes through same-type pipelines preserve all live input aliases" do
    for type <- [:s32, :f64], storage <- @storage do
      source = Nx.iota({16, 8}, type: type)

      assert_operation(source, storage, fn tensor ->
        Nx.put_slice(tensor, [1, 0], Nx.slice(tensor, [0, 0], [8, 8]))
      end)

      indices = Nx.tensor([[2], [2], [0]], type: :s64)
      updates = Nx.tensor(List.duplicate(List.duplicate(3, 8), 3), type: type)

      for op <- [&Nx.indexed_put/3, &Nx.indexed_add/3] do
        assert_operation(source, storage, &op.(&1, indices, updates))
      end
    end
  end

  test "same-type index buffers remain valid at every sub-binary offset" do
    source = Nx.iota({32, 8}, type: :s32)
    index_template = Nx.tensor(Enum.to_list(15..0//-1), type: :s64)

    for storage <- @storage do
      {indices, owner} = input(index_template, storage)
      {tensor, _} = input(source, :owned)
      assert_same(Nx.take(tensor, indices), Nx.take(source, index_template))
      assert Nx.to_binary(indices) == Nx.to_binary(index_template)
      assert Nx.to_binary(owner) == Nx.to_binary(index_template)
    end
  end

  test "reductions and window outputs retain their data and exact finite results" do
    for type <- [:s32, :f64], storage <- @storage do
      source = Nx.iota({16, 8}, type: type)
      assert_operation(source, storage, &Nx.sum(&1, axes: [1]))
      assert_operation(source, storage, &Nx.reduce_max(&1, axes: [1]))
      assert_operation(source, storage, &Nx.window_sum(&1, {2, 2}))
      assert_operation(source, storage, &Nx.window_max(&1, {2, 2}))
    end
  end

  test "same-type copies preserve zero signs and exceptional float payloads" do
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

    for {type, width, values} <- encodings, storage <- @storage do
      pattern = for value <- values, into: <<>>, do: <<value::unsigned-native-size(width)>>
      raw = :binary.copy(pattern, 16)
      source = Nx.from_binary(raw, type)
      {tensor, owner} = input(source, storage)
      result = Nx.put_slice(tensor, [1], Nx.slice(tensor, [0], [64]))
      bytes = div(width, 8)

      expected =
        binary_part(raw, 0, bytes) <>
          binary_part(raw, 0, 64 * bytes) <>
          binary_part(raw, 65 * bytes, byte_size(raw) - 65 * bytes)

      assert Nx.to_binary(result) == expected
      assert Nx.to_binary(tensor) == raw
      assert Nx.to_binary(owner) == raw
    end
  end

  test "f64 linear algebra outputs outlive the process holding input owners" do
    source = Nx.tensor(List.duplicate([[4.0, 0.0], [0.0, 9.0]], 16), type: :f64)
    expected = Nx.LinAlg.cholesky(source)

    for storage <- @storage do
      result =
        Task.async(fn ->
          {tensor, _owner} = input(source, storage)
          Nx.LinAlg.cholesky(tensor)
        end)
        |> Task.await()

      :erlang.garbage_collect()
      assert_same(result, expected)
    end
  end
end
