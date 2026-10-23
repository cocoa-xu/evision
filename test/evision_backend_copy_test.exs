defmodule Evision.BackendCopyTest do
  use ExUnit.Case, async: true

  @types [:u8, :s16, :s32, :s64, :f32, :f64]
  @shapes [[9], [4, 5], [3, 4, 5], [2, 2, 3, 4]]

  defp mat(tensor, storage) do
    result =
      Evision.Mat.from_binary_by_shape(Nx.to_binary(tensor), Nx.type(tensor), Nx.shape(tensor))

    if storage == :owned, do: Evision.Mat.clone(result), else: result
  end

  defp bytes(%{ref: ref}), do: :evision_nif.mat_to_binary(img: ref)

  # Enumerate logical coordinates independently of the native block-copy loop.
  defp coordinates([]), do: [[]]

  defp coordinates([dim | dims]) do
    for value <- 0..(dim - 1), rest <- coordinates(dims), do: [value | rest]
  end

  defp expected_slice(tensor, dims, out, starts, steps) do
    source = Nx.to_binary(tensor)
    width = div(byte_size(source), Nx.size(tensor))

    for coordinate <- coordinates(out), into: <<>> do
      offset =
        Enum.zip([coordinate, starts, steps, dims])
        |> Enum.reduce(0, fn {index, start, step, dim}, acc ->
          acc * dim + start + index * step
        end)

      binary_part(source, offset * width, width)
    end
  end

  test "strided copies preserve bytes across ranks, widths, directions and storage owners" do
    for dims <- @shapes, type <- @types, storage <- [:binary, :owned] do
      tensor = Nx.iota(List.to_tuple(dims), type: type)
      src = mat(tensor, storage)
      zero = Enum.map(dims, fn _ -> 0 end)
      one = Enum.map(dims, fn _ -> 1 end)
      last = Enum.map(dims, &(&1 - 1))
      half = Enum.map(dims, &div(&1 + 1, 2))

      layouts = [
        {dims, zero, one},
        {last, one, one},
        {dims, last, Enum.map(dims, fn _ -> -1 end)},
        {dims, List.replace_at(zero, -1, List.last(last)), List.replace_at(one, -1, -1)},
        {half, zero, Enum.map(dims, fn _ -> 2 end)},
        {half, last, Enum.map(dims, fn _ -> -2 end)},
        {dims, zero, Enum.map(dims, fn _ -> 0 end)},
        {one, zero, Enum.map(dims, fn _ -> 2_147_483_647 end)}
      ]

      for {out, starts, steps} <- layouts do
        result =
          :evision_nif.mat_strided_copy(
            src: src.ref,
            in_dims: dims,
            out_dims: out,
            starts: starts,
            strides: steps
          )

        assert bytes(result) == expected_slice(tensor, dims, out, starts, steps)
      end

      assert bytes(src) == Nx.to_binary(tensor)
    end
  end

  test "put_slice copies partial rows and contiguous suffixes without changing either input" do
    for dims <- @shapes, type <- @types, storage <- [:binary, :owned] do
      tensor = Nx.iota(List.to_tuple(dims), type: type)
      base = mat(tensor, storage)
      zero = Enum.map(dims, fn _ -> 0 end)

      for shape <- [dims, Enum.map(dims, &(&1 - 1)), List.update_at(dims, 0, &(&1 - 1))] do
        update = Nx.iota(List.to_tuple(shape), type: type) |> Nx.add(1) |> Nx.as_type(type)
        slice = mat(update, storage)
        starts = Enum.zip(dims, shape) |> Enum.map(fn {dim, size} -> dim - size end)

        result =
          :evision_nif.mat_put_slice(
            base: base.ref,
            slice: slice.ref,
            base_dims: dims,
            slice_dims: shape,
            starts: starts
          )

        assert bytes(result) == Nx.to_binary(Nx.put_slice(tensor, starts, update))
        assert bytes(slice) == Nx.to_binary(update)
        assert bytes(base) == Nx.to_binary(tensor)
      end

      result =
        :evision_nif.mat_put_slice(
          base: base.ref,
          slice: base.ref,
          base_dims: dims,
          slice_dims: dims,
          starts: zero
        )

      assert bytes(result) == Nx.to_binary(tensor)
      assert bytes(base) == Nx.to_binary(tensor)
    end
  end

  test "indexed_add keeps aliased inputs immutable" do
    for storage <- [:binary, :owned] do
      tensor = Nx.iota({4, 4}, type: :s64) |> Nx.add(1)
      src = mat(tensor, storage)
      indices = mat(Nx.iota({4, 1}, type: :s64), storage)
      dims = mat(Nx.tensor([4], type: :s64), storage)

      result =
        :evision_nif.mat_indexed(
          src: src.ref,
          updates: src.ref,
          indices: indices.ref,
          dims: dims.ref,
          inner: 4,
          op: 1
        )

      assert bytes(result) == Nx.to_binary(Nx.add(tensor, tensor))
      assert bytes(src) == Nx.to_binary(tensor)
    end
  end

  test "scalar copy paths do not require an iteration axis" do
    for storage <- [:binary, :owned] do
      src = mat(Nx.tensor(7, type: :s64), storage)

      sliced =
        :evision_nif.mat_strided_copy(
          src: src.ref,
          in_dims: [],
          out_dims: [],
          starts: [],
          strides: []
        )

      updated =
        :evision_nif.mat_put_slice(
          base: src.ref,
          slice: src.ref,
          base_dims: [],
          slice_dims: [],
          starts: []
        )

      assert bytes(sliced) == <<7::signed-native-64>>
      assert bytes(updated) == bytes(sliced)
      assert bytes(src) == bytes(sliced)
    end
  end

  test "reshaped and repeated batch outputs outlive their input process" do
    for storage <- [:binary, :owned] do
      {shaped, batches} =
        Task.async(fn ->
          src = mat(Nx.iota({4, 4}, type: :s32), storage)
          shaped = :evision_nif.mat_as_shape(img: src.ref, as_shape: [2, 2, 4])

          batches =
            :evision_nif.mat_to_batched(
              img: src.ref,
              as_shape: [4, 4],
              batch_size: 3,
              leftover: "repeat"
            )

          {shaped, batches}
        end)
        |> Task.await()

      :erlang.garbage_collect()
      original = for value <- 0..15, into: <<>>, do: <<value::signed-native-32>>
      assert bytes(shaped) == original

      assert Enum.map(batches, &bytes/1) == [
               binary_part(original, 0, 48),
               binary_part(original, 48, 16) <> binary_part(original, 0, 32)
             ]
    end
  end
  test "broadcast select rejects mismatched native layouts" do
    mask = mat(Nx.tensor([0, 1], type: :u8), :binary)
    value = mat(Nx.tensor([10, 20], type: :s32), :binary)

    for {shapes, out} <- [
          {[[2], [2], [2]], [3]},
          {[[1], [2], [2]], [2]},
          {[[2], [1], [2]], [2]},
          {[[2], [2], [2]], [0]},
          {[[2], [2], [2]], []},
          {[[2], [2], [2]], [2_147_483_647, 2_147_483_647]}
        ] do
      assert {:error, _} = Evision.Mat.select_broadcast(mask, value, value, shapes, out)
    end

    assert {:error, _} = Evision.Mat.select_broadcast(value, value, value, [[2], [2], [2]], [2])
    assert {:error, _} = Evision.Mat.select_broadcast(mask, mask, value, [[2], [2], [2]], [2])
  end

end
