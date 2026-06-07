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

  describe "indexed_add / indexed_put" do
    test "match Nx.BinaryBackend across axes, ranks, dtypes (incl. wide ints), and duplicates" do
      # Integer / same-type results -> exact. Covers default + explicit axes,
      # scalar + block updates, and duplicate coordinates (add sums, put last-wins).
      int_cases = [
        {Nx.tensor([0, 0, 0]), Nx.tensor([[1], [2]]), Nx.tensor([2, 4]), []},
        {Nx.tensor([1, 2, 3]), Nx.tensor([[0], [1], [2], [0]]), Nx.tensor([10, 20, 30, 40]), []},
        {Nx.iota({1, 2, 3}), Nx.tensor([[0, 0, 0], [0, 1, 1], [0, 0, 2]]), Nx.tensor([1, 3, -2]), []},
        {Nx.iota({1, 3, 2}), Nx.tensor([[0, 0], [0, 2]]), Nx.tensor([[0, 10], [40, 50]]), []},
        {Nx.tensor([[1, 2, 3], [4, 5, 6]]), Nx.tensor([[0], [2], [2]]),
         Nx.tensor([[7, 8], [9, 10], [11, 12]]), [axes: [1]]},
        {Nx.tensor([[1, 2], [3, 4]]), Nx.tensor([[1, 0], [0, 1], [1, 0]]), Nx.tensor([5, 6, 7]), []}
      ]

      for op <- [:indexed_add, :indexed_put], {t, i, u, o} <- int_cases do
        assert_same(apply(Nx, op, [ev(t), ev(i), ev(u), o]), apply(Nx, op, [t, i, u, o]))
      end

      # Wide-int dtypes cv::add/cv::reduce reject -> exercises the custom typed path.
      for op <- [:indexed_add, :indexed_put], type <- [{:u, 32}, {:s, 64}, {:u, 64}] do
        t = Nx.tensor([[10, 20, 30], [40, 50, 60]], type: type)
        i = Nx.tensor([[0, 1], [1, 2], [0, 1]])
        u = Nx.tensor([1, 2, 3], type: type)
        assert_same(apply(Nx, op, [ev(t), ev(i), ev(u)]), apply(Nx, op, [t, i, u]))
      end

      # Rank-1 index + scalar update shorthand (relies on scalar from_binary support).
      assert_same(
        Nx.indexed_put(ev(Nx.tensor([[1], [2]])), ev(Nx.tensor([1, 0])), ev(Nx.tensor(10))),
        Nx.indexed_put(Nx.tensor([[1], [2]]), Nx.tensor([1, 0]), Nx.tensor(10))
      )
    end

    test "match Nx.BinaryBackend approximately for float, half, and mixed dtypes" do
      cases = [
        {Nx.tensor([0.0, 0.0, 0.0]), Nx.tensor([[0], [1], [0]]), Nx.tensor([1.5, 2.5, 3.0]), []},
        {Nx.tensor([1.0, 2.0, 3.0], type: :f16), Nx.tensor([[0], [2]]),
         Nx.tensor([4.0, 5.0], type: :f16), []},
        {Nx.tensor([1, 2, 3], type: :s32), Nx.tensor([[0], [1]]),
         Nx.tensor([1.5, 2.5], type: :f32), []}
      ]

      for op <- [:indexed_add, :indexed_put], {t, i, u, o} <- cases do
        assert_close(apply(Nx, op, [ev(t), ev(i), ev(u), o]), apply(Nx, op, [t, i, u, o]))
      end
    end
  end

  describe "scalar (0-dim) tensors" do
    test "round-trip through Evision.Backend.from_binary across dtypes" do
      for t <- [Nx.tensor(10), Nx.tensor(-3, type: :s32), Nx.tensor(2.5, type: :f32), Nx.tensor(7, type: :u64)] do
        assert_same(ev(t), t)
      end
    end
  end
end
