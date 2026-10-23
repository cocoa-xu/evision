defmodule Evision.BackendIndexSafetyTest do
  use ExUnit.Case, async: true

  defp mat(binary, t \\ "u", bits \\ 8) do
    %{ref: ref} =
      :evision_nif.mat_from_binary_by_shape(
        binary: binary,
        shape: [div(byte_size(binary), div(bits, 8))],
        t: t,
        l: bits
      )

    ref
  end

  defp ints(values),
    do: mat(for(value <- values, into: <<>>, do: <<value::signed-native-64>>), "s", 64)

  test "take validates both layout and index representation" do
    src = mat(<<10, 20, 30, 40>>)

    for opts <- [
          [indices: mat(<<0>>), outer: 1, axis_dim: 4, inner: 1],
          [indices: ints([100]), outer: 1, axis_dim: 4096, inner: 1],
          [indices: ints([0]), outer: 1, axis_dim: 4, inner: -1],
          [indices: ints([4]), outer: 1, axis_dim: 4, inner: 1]
        ] do
      assert {:error, _} = :evision_nif.mat_take([src: src] ++ opts)
    end

    %{ref: out} =
      :evision_nif.mat_take(src: src, indices: ints([3, 0]), outer: 1, axis_dim: 4, inner: 1)

    assert :evision_nif.mat_to_binary(img: out) == <<40, 10>>
  end

  test "gather and scatter reject fabricated dimensions and truncated updates" do
    src = mat(<<1, 2, 3, 4>>)

    assert {:error, _} =
             :evision_nif.mat_gather(src: src, indices: ints([100]), dims: ints([4096]), inner: 1)

    assert {:error, _} =
             :evision_nif.mat_gather(src: src, indices: mat(<<0>>), dims: ints([4]), inner: 1)

    assert {:error, _} =
             :evision_nif.mat_gather(src: src, indices: ints([0]), dims: mat(<<4>>), inner: 1)

    assert {:error, _} =
             :evision_nif.mat_indexed(
               src: src,
               indices: ints([100]),
               updates: mat(<<9>>),
               dims: ints([4096]),
               inner: 1,
               op: 0
             )

    assert {:error, _} =
             :evision_nif.mat_indexed(
               src: src,
               indices: ints([0, 1]),
               updates: mat(<<9>>),
               dims: ints([4]),
               inner: 1,
               op: 0
             )

    assert {:error, _} =
             :evision_nif.mat_indexed(
               src: src,
               indices: ints([0]),
               updates: ints([9]),
               dims: ints([4]),
               inner: 1,
               op: 0
             )

    %{ref: out} =
      :evision_nif.mat_indexed(
        src: src,
        indices: ints([3, 0]),
        updates: mat(<<9, 8>>),
        dims: ints([4]),
        inner: 1,
        op: 0
      )

    assert :evision_nif.mat_to_binary(img: out) == <<8, 2, 3, 9>>
    assert :evision_nif.mat_to_binary(img: src) == <<1, 2, 3, 4>>
  end

  test "take_along_axis checks non-axis dimensions and index count" do
    src = mat(<<1, 2, 3, 4>>)

    assert {:error, _} =
             :evision_nif.mat_take_along_axis(
               src: src,
               indices: ints([0, 0, 0, 0, 0, 0]),
               in_dims: [2, 2],
               out_dims: [3, 2],
               axis: 1
             )

    assert {:error, _} =
             :evision_nif.mat_take_along_axis(
               src: src,
               indices: ints([0]),
               in_dims: [2, 2],
               out_dims: [2, 2],
               axis: 1
             )
  end

  test "strided copies validate the backing storage and support reverse" do
    src = mat(<<1, 2, 3, 4>>)

    assert {:error, _} =
             :evision_nif.mat_strided_copy(
               src: src,
               in_dims: [4096],
               out_dims: [1],
               starts: [100],
               strides: [1]
             )

    assert {:error, _} =
             :evision_nif.mat_strided_copy(
               src: src,
               in_dims: [4],
               out_dims: [2_147_483_647, 2],
               starts: [0],
               strides: [1]
             )

    %{ref: out} =
      :evision_nif.mat_strided_copy(
        src: src,
        in_dims: [4],
        out_dims: [4],
        starts: [3],
        strides: [-1]
      )

    assert :evision_nif.mat_to_binary(img: out) == <<4, 3, 2, 1>>
  end

  test "put_slice checks real sizes, types and overflowing end positions" do
    base = mat(<<1, 2, 3, 4>>)
    slice = mat(<<9, 8>>)

    for {base_dims, slice_dims, starts} <- [
          {[4096], [2], [100]},
          {[4], [3], [0]},
          {[4], [2], [2_147_483_647]}
        ] do
      assert {:error, _} =
               :evision_nif.mat_put_slice(
                 base: base,
                 slice: slice,
                 base_dims: base_dims,
                 slice_dims: slice_dims,
                 starts: starts
               )
    end

    assert {:error, _} =
             :evision_nif.mat_put_slice(
               base: base,
               slice: ints([1, 2]),
               base_dims: [4],
               slice_dims: [2],
               starts: [0]
             )

    %{ref: out} =
      :evision_nif.mat_put_slice(
        base: base,
        slice: slice,
        base_dims: [4],
        slice_dims: [2],
        starts: [1]
      )

    assert :evision_nif.mat_to_binary(img: out) == <<1, 9, 8, 4>>
  end

  test "select requires a full mask and equally sized branches of the same type" do
    t = mat(<<1, 2, 3, 4>>)
    assert {:error, _} = :evision_nif.mat_select(mask: mat(<<0>>), on_true: t, on_false: t)

    assert {:error, _} =
             :evision_nif.mat_select(mask: mat(<<0, 0, 0, 0>>), on_true: t, on_false: mat(<<9>>))

    assert {:error, _} =
             :evision_nif.mat_select(mask: ints([0, 0, 0, 0]), on_true: t, on_false: t)

    %{ref: out} =
      :evision_nif.mat_select(
        mask: mat(<<1, 0, 1, 0>>),
        on_true: t,
        on_false: mat(<<5, 6, 7, 8>>)
      )

    assert :evision_nif.mat_to_binary(img: out) == <<1, 6, 3, 8>>
  end
end
