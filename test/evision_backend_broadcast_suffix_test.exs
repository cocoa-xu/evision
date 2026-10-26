defmodule Evision.Backend.BroadcastSuffixTest do
  use ExUnit.Case, async: true

  setup do
    Nx.default_backend(Nx.BinaryBackend)
    :ok
  end

  test "broadcast preserves every byte across contiguous suffixes and stretched axes" do
    types = [:u8, :s8, :u16, :s16, :u32, :s32, :u64, :s64, :f16, :bf16, :f32, :f64]

    shapes = [
      {{1, 4, 8}, {3, 4, 8}},
      {{2, 1, 3, 4}, {2, 5, 3, 4}},
      {{2, 1, 1}, {2, 5, 7}},
      {{2, 1, 3, 1}, {2, 5, 3, 7}},
      {{1, 2, 1, 3, 1}, {4, 2, 5, 3, 1}},
      {{}, {3, 4, 5}}
    ]

    for type <- types, {source_shape, target_shape} <- shapes do
      {_, bits} = Nx.Type.normalize!(type)
      size = Nx.size(source_shape) * div(bits, 8)
      bytes = for i <- 0..(size - 1), into: <<>>, do: <<rem(i * 73 + div(i, 17), 256)>>
      source = bytes |> Nx.from_binary(type) |> Nx.reshape(source_shape)
      input = Nx.backend_copy(source, Evision.Backend)
      result = Nx.broadcast(input, target_shape)

      assert Nx.to_binary(result) == Nx.to_binary(Nx.broadcast(source, target_shape))
      assert Nx.to_binary(input) == bytes
      assert result.type == source.type
      assert result.shape == target_shape
    end
  end

  test "explicit axes and vectorized metadata survive coalesced copies" do
    source = Nx.tensor([[1, 2, 3], [4, 5, 6]], type: :s64)
    input = Nx.backend_copy(source, Evision.Backend)

    for {shape, axes} <- [{{2, 4, 3}, [0, 2]}, {{4, 2, 3}, [1, 2]}] do
      reference = Nx.broadcast(source, shape, axes: axes)
      result = Nx.broadcast(input, shape, axes: axes)
      assert Nx.to_binary(result) == Nx.to_binary(reference)
      assert result.names == reference.names
    end

    reference = Nx.broadcast(Nx.vectorize(source, :sample), {4, 3})
    result = Nx.broadcast(Nx.vectorize(input, :sample), {4, 3})
    assert Nx.to_binary(result) == Nx.to_binary(reference)
    assert result.vectorized_axes == reference.vectorized_axes
  end

  test "Mat broadcasting copies whole channel groups" do
    source = Evision.Mat.from_binary_by_shape(<<1, 2, 3, 4, 5, 6>>, :u8, {2, 1, 3})
    source = Evision.Mat.last_dim_as_channel(source)
    result = Evision.Mat.broadcast_to(source, {2, 5}, {2, 1})

    assert result.channels == 3

    assert Evision.Mat.to_binary(result) ==
             :binary.copy(<<1, 2, 3>>, 5) <> :binary.copy(<<4, 5, 6>>, 5)
  end
end
