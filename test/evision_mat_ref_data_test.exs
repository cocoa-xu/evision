defmodule Evision.Mat.RefData.Test do
  use ExUnit.Case, async: true

  alias Evision.Mat

  @big {480, 640, 3}
  @big_bytes 480 * 640 * 3

  test "large Mats carry the binary that holds their data" do
    mat = Mat.zeros(@big, :u8)
    assert is_binary(mat.ref_data)
    assert byte_size(mat.ref_data) >= @big_bytes

    img = Mat.last_dim_as_channel(mat)
    rgb = Evision.cvtColor(img, Evision.Constant.cv_COLOR_BGR2RGB())
    assert byte_size(rgb.ref_data) >= @big_bytes
  end

  test "ref_data holds only the data and zeroed padding" do
    ref_data = sevens(:binary.copy(<<7>>, @big_bytes)).ref_data
    counts = for <<byte <- ref_data>>, reduce: %{}, do: (acc -> Map.update(acc, byte, 1, &(&1 + 1)))
    assert counts == %{7 => @big_bytes, 0 => byte_size(ref_data) - @big_bytes}
  end

  test "channel_as_last_dim keeps the struct when the shape does not change" do
    mat = Mat.zeros({1024, 1024}, :u8)
    assert Mat.channel_as_last_dim(mat) == mat
  end

  test "small and empty Mats have no ref_data" do
    assert Mat.zeros({2, 2}, :u8).ref_data == nil
    assert Mat.empty().ref_data == nil
  end

  test "a Mat over an Erlang binary refers to that binary" do
    binary = :binary.copy(<<1>>, @big_bytes)
    assert Mat.from_binary(binary, {:u, 8}, 480, 640, 3).ref_data == binary
    assert Mat.from_binary_by_shape(binary, {:u, 8}, @big).ref_data == binary
  end

  test "a Mat stays valid after its ref_data is dropped" do
    expected = :binary.copy(<<7>>, @big_bytes)
    mat = %{sevens(expected) | ref_data: nil}
    :erlang.garbage_collect()
    assert Mat.to_binary(mat) == expected
  end

  test "ref_data stays valid after its Mat is dropped" do
    ref_data = sevens(:binary.copy(<<7>>, @big_bytes)).ref_data
    :erlang.garbage_collect()
    assert byte_size(ref_data) >= @big_bytes
    assert :binary.match(ref_data, :binary.copy(<<7>>, @big_bytes)) != :nomatch
  end

  defp sevens(binary) do
    zeros = Mat.zeros(@big, :u8) |> Mat.last_dim_as_channel()
    Evision.add(Mat.from_binary(binary, {:u, 8}, 480, 640, 3), zeros)
  end

  test "ref_data is hidden from inspect" do
    refute inspect(Mat.zeros(@big, :u8)) =~ "ref_data"
  end

  test "Mats are compared by ref before ref_data" do
    # Small maps order atom keys by atom creation, and == compares values in that order.
    keys = Map.keys(Mat.zeros(@big, :u8))
    assert Enum.find_index(keys, &(&1 == :ref)) < Enum.find_index(keys, &(&1 == :ref_data))
  end

  test "dropped Mats trigger garbage collection even with a large process heap" do
    parent = self()

    pid =
      :erlang.spawn_opt(
        fn ->
          receive do: (:go -> Enum.each(1..20, fn _ -> Mat.zeros({1024, 1024, 4}, :u8) end))
          send(parent, :done)
        end,
        min_heap_size: 1_000_000
      )

    :erlang.trace(pid, true, [:garbage_collection])
    send(pid, :go)
    assert_receive :done, 60_000
    assert_receive {:trace, ^pid, gc, _} when gc in [:gc_minor_start, :gc_major_start], 5_000
  end
end
