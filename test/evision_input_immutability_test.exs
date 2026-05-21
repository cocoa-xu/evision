defmodule Evision.InputImmutabilityTest do
  use ExUnit.Case

  # Guards the input-sharing optimisation in `evision_to(Mat&)`.
  #
  # Generated bindings hand read-only matrices to OpenCV as const InputArrays,
  # so the converter shares the source buffer instead of deep-copying it. These
  # tests make sure that optimisation never lets an OpenCV call mutate the
  # caller's matrix, for both OpenCV-owned and binary-backed matrices.

  alias Evision.Mat

  # 3x4 u8 matrix with distinct values, so an accidental write is visible.
  @data <<1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12>>

  # A binary-backed (zero-copy) matrix: its data is owned by an Erlang binary,
  # so it stays on the deep-copy path.
  defp zero_copy_mat, do: Mat.from_binary(@data, {:u, 8}, 3, 4, 1)

  # `clone/1` deep-copies, so the result is an OpenCV-owned matrix -- the case
  # eligible for the shared-input fast path.
  defp owned_mat, do: Mat.clone(zero_copy_mat())

  describe "a generated binding never mutates its input matrix" do
    test "OpenCV-owned input (shared fast path)" do
      a = owned_mat()
      before = Mat.to_binary(a)

      _ = Evision.add(a, a)
      _ = Evision.subtract(a, a)
      _ = Evision.add(a, a)

      assert Mat.to_binary(a) == before
    end

    test "binary-backed input (deep-copy path)" do
      a = zero_copy_mat()

      _ = Evision.add(a, a)
      _ = Evision.subtract(a, a)

      assert Mat.to_binary(a) == @data
    end

    test "the same matrix passed twice to one call is left intact" do
      # add(a, a) would corrupt `a` if the input were aliased onto the output.
      a = owned_mat()
      sum = Evision.add(a, a)

      assert Mat.to_binary(a) == @data

      doubled =
        @data |> :binary.bin_to_list() |> Enum.map(&(&1 * 2)) |> :erlang.list_to_binary()

      assert Mat.to_binary(sum) == doubled
    end
  end

  describe "results stay correct across reuse" do
    test "one matrix reused through a chain of ops" do
      a = owned_mat()

      b = Evision.add(a, a)
      c = Evision.subtract(b, a)
      d = Evision.add(c, c)

      assert Mat.to_binary(a) == @data
      assert Mat.to_binary(c) == @data
      assert Mat.to_binary(d) == Mat.to_binary(b)
    end

    test "clone is independent of its source" do
      a = owned_mat()
      copy = Mat.clone(a)

      _ = Evision.add(a, a)

      assert Mat.to_binary(copy) == @data
      assert Mat.to_binary(a) == @data
    end
  end
end
