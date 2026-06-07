defmodule Evision.Backend.Test do
  use ExUnit.Case
  doctest Evision.Backend

  # Move a reference (BinaryBackend) tensor onto Evision.Backend.
  defp ev(t), do: Nx.backend_copy(t, Evision.Backend)

  # Assert an Evision.Backend result equals the BinaryBackend reference exactly.
  defp assert_same(got, want) do
    assert Nx.type(got) == Nx.type(want)
    assert Nx.shape(got) == Nx.shape(want)
    got_list = got |> Nx.backend_copy(Nx.BinaryBackend) |> Nx.to_flat_list()
    assert got_list == Nx.to_flat_list(want)
  end

  defp axis_in_rank?(t, opts) do
    case opts[:axis] do
      nil -> true
      axis -> axis < Nx.rank(t)
    end
  end

  describe "argmax/argmin (index family)" do
    test "match Nx.BinaryBackend across axes, ranks, tie-breaks, and dtypes" do
      tensors = [
        Nx.tensor([3, 1, 2]),
        Nx.tensor([[1, 5, 3], [9, 2, 8]]),
        Nx.tensor([[[1, 2], [3, 4]], [[5, 6], [0, 7]]]),
        Nx.tensor([[1, 1, 1], [2, 2, 2]]),
        Nx.tensor([[1.5, -2.0, 3.25], [0.0, 0.0, 0.0]], type: :f32)
      ]

      opts_list = [
        [],
        [tie_break: :high],
        [axis: 0],
        [axis: 1],
        [axis: 0, keep_axis: true],
        [axis: 1, tie_break: :high]
      ]

      for t <- tensors, fun <- [:argmax, :argmin], opts <- opts_list, axis_in_rank?(t, opts) do
        assert_same(apply(Nx, fun, [ev(t), opts]), apply(Nx, fun, [t, opts]))
      end
    end
  end

  describe "sort/argsort" do
    test "match Nx.BinaryBackend across axes, ranks, directions, ties, and dtypes" do
      base = [
        Nx.tensor([3, 1, 2, 1, 3]),
        Nx.tensor([[3, 1, 2], [1, 3, 2]]),
        Nx.tensor([[[2, 1], [1, 2]], [[3, 0], [0, 3]]]),
        Nx.tensor([[5, 5, 5], [5, 5, 5]]),
        Nx.tensor([[1.5, -2.0, 3.25], [3.25, 1.5, -2.0]], type: :f32)
      ]

      # exercise the wide-int custom NIF path (u32/s64/u64) and the half-float
      # cast path (f16/bf16), with ties to pin stable ordering
      typed = for ty <- [:u32, :s64, :u64, :f16, :bf16], do: Nx.tensor([[3, 1, 2, 1], [4, 0, 5, 0]], type: ty)

      opts_list = [
        [],
        [direction: :desc],
        [axis: 1],
        [axis: 1, direction: :desc],
        [axis: 2],
        [axis: 2, direction: :desc]
      ]

      for t <- base ++ typed, fun <- [:sort, :argsort], opts <- opts_list, axis_in_rank?(t, opts) do
        assert_same(apply(Nx, fun, [ev(t), opts]), apply(Nx, fun, [t, opts]))
      end
    end
  end
end
