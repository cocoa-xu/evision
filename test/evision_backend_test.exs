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

  # For float results, reduction order is implementation-defined; compare approximately.
  defp assert_close(got, want) do
    assert Nx.type(got) == Nx.type(want)
    assert Nx.shape(got) == Nx.shape(want)
    got_b = Nx.backend_copy(got, Nx.BinaryBackend)

    assert Nx.to_number(Nx.all_close(got_b, want, atol: 1.0e-5, rtol: 1.0e-5)) == 1,
           "got #{inspect(Nx.to_flat_list(got_b))} vs #{inspect(Nx.to_flat_list(want))}"
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

  describe "sum/product" do
    test "match Nx.BinaryBackend exactly across axes, keep_axes, and integer dtypes" do
      tensors = for ty <- [:s32, :u8, :s64, :u64, :u32], do: Nx.tensor([[[1, 2], [3, 4]], [[5, 6], [7, 8]]], type: ty)

      opts_list = [
        [],
        [axes: [0]],
        [axes: [1]],
        [axes: [2]],
        [axes: [0, 2]],
        [axes: [1], keep_axes: true],
        [axes: [0, 1, 2]]
      ]

      for t <- tensors, fun <- [:sum, :product], opts <- opts_list do
        assert_same(apply(Nx, fun, [ev(t), opts]), apply(Nx, fun, [t, opts]))
      end
    end

    test "match Nx.BinaryBackend approximately for floats" do
      t32 = Nx.tensor([[1.5, -2.0, 3.25], [0.5, 4.0, -1.0]], type: :f32)

      for t <- [t32, Nx.as_type(t32, :f64)],
          fun <- [:sum, :product],
          opts <- [[], [axes: [0]], [axes: [1]], [axes: [1], keep_axes: true]] do
        assert_close(apply(Nx, fun, [ev(t), opts]), apply(Nx, fun, [t, opts]))
      end
    end
  end

  describe "take" do
    test "matches Nx.BinaryBackend across axes, ranks, dtypes, and index shapes" do
      tensors = [
        Nx.tensor([10, 20, 30, 40]),
        Nx.tensor([[1, 2, 3], [4, 5, 6]]),
        Nx.tensor([[[1, 2], [3, 4]], [[5, 6], [7, 8]]], type: :s64),
        Nx.tensor([[1.5, 2.5, 3.5], [4.5, 5.5, 6.5]], type: :f32)
      ]

      index_sets = [Nx.tensor([1, 0, 1]), Nx.tensor([[0, 1], [1, 0]])]

      for t <- tensors, axis <- 0..(Nx.rank(t) - 1), idx <- index_sets do
        assert_same(Nx.take(ev(t), ev(idx), axis: axis), Nx.take(t, idx, axis: axis))
      end
    end
  end

  describe "gather" do
    test "matches Nx.BinaryBackend across axes, ranks, dtypes, and index shapes" do
      t2 = Nx.tensor([[1, 2], [3, 4]])
      t23 = Nx.tensor([[1, 2, 3], [4, 5, 6]])
      t3 = Nx.tensor([[[1, 2], [11, 12]], [[101, 102], [111, 112]]], type: :s64)
      t23f = Nx.tensor([[1.5, 2.5, 3.5], [4.5, 5.5, 6.5]], type: :f32)
      iota = Nx.iota({2, 1, 3})

      # {tensor, indices, opts} triples valid for Nx.gather (coords in bounds,
      # indices last dim == length(axes)); covers default + explicit axes,
      # contiguous + non-contiguous axes, scalar + subset gathers, int + float.
      cases = [
        {t2, Nx.tensor([[1, 1], [0, 1], [1, 0]]), []},
        {t2, Nx.tensor([[[1, 1], [0, 0]], [[1, 0], [0, 1]]]), []},
        {t3, Nx.tensor([[0, 0, 0], [0, 1, 1], [1, 1, 1]]), []},
        {t23, Nx.tensor([[1], [0]]), []},
        {t23, Nx.tensor([[1], [0], [2], [1]]), [axes: [1]]},
        {iota, Nx.tensor([[[1], [0], [2]]]), [axes: [2]]},
        {t23f, Nx.tensor([[0, 0], [1, 2], [0, 2]]), []},
        {t23f, Nx.tensor([[0], [1]]), [axes: [0]]},
        {t3, Nx.tensor([[0], [1]]), [axes: [0]]},
        {t3, Nx.tensor([[0, 1], [1, 0]]), [axes: [0, 2]]}
      ]

      for {t, idx, opts} <- cases do
        assert_same(Nx.gather(ev(t), ev(idx), opts), Nx.gather(t, idx, opts))
      end
    end
  end
end
