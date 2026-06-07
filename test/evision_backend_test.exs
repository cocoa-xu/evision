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

  defp axes_in_rank?(t, opts) do
    case opts[:axes] do
      nil -> true
      axes -> Enum.all?(axes, &(&1 < Nx.rank(t)))
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

  describe "cumulative_sum / product / min / max" do
    test "match Nx.BinaryBackend exactly for integer dtypes across axes and :reverse" do
      tensors = [
        Nx.tensor([1, 2, 3, 4]),
        Nx.tensor([[1, 2, 3], [4, 5, 6]]),
        Nx.tensor([[[1, 2], [3, 4]], [[5, 6], [7, 8]]]),
        Nx.tensor([[3, 1, 2], [9, 8, 7]], type: :s32),
        Nx.tensor([[10, 20], [30, 40]], type: :u64)
      ]

      opts_list = [[], [reverse: true], [axis: 0], [axis: 1, reverse: true]]

      for op <- [:cumulative_sum, :cumulative_product, :cumulative_min, :cumulative_max],
          t <- tensors, opts <- opts_list, axis_in_rank?(t, opts) do
        assert_same(apply(Nx, op, [ev(t), opts]), apply(Nx, op, [t, opts]))
      end
    end

    test "match Nx.BinaryBackend approximately for float and half dtypes" do
      tensors = [
        Nx.tensor([[1.5, -2.0, 3.25], [0.5, 4.0, -1.0]], type: :f32),
        Nx.tensor([[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]], type: :f16)
      ]

      opts_list = [[], [reverse: true], [axis: 0], [axis: 1, reverse: true]]

      for op <- [:cumulative_sum, :cumulative_product, :cumulative_min, :cumulative_max],
          t <- tensors, opts <- opts_list, axis_in_rank?(t, opts) do
        assert_close(apply(Nx, op, [ev(t), opts]), apply(Nx, op, [t, opts]))
      end
    end
  end

  describe "elementwise unary math" do
    test "match Nx.BinaryBackend approximately (in-domain real inputs)" do
      general = Nx.tensor([0.2, 0.5, 0.9, 1.5, 2.0, 3.0], type: :f32)
      unit = Nx.tensor([-0.9, -0.3, 0.0, 0.3, 0.9], type: :f32)
      ge1 = Nx.tensor([1.0, 1.5, 2.0, 5.0], type: :f32)
      pos = Nx.tensor([0.1, 0.5, 1.0, 2.0, 9.0], type: :f32)

      cases = [
        {:sin, general}, {:cos, general}, {:tan, general},
        {:sinh, general}, {:cosh, general}, {:tanh, general},
        {:asinh, general}, {:atan, general}, {:sigmoid, general},
        {:erf, general}, {:erfc, general}, {:cbrt, general},
        {:asin, unit}, {:acos, unit}, {:atanh, unit},
        {:acosh, ge1},
        {:sqrt, pos}, {:rsqrt, pos}, {:log1p, pos}
      ]

      for {op, t} <- cases do
        assert_close(apply(Nx, op, [ev(t)]), apply(Nx, op, [t]))
      end
    end

    test "cast integer input to float and handle f64/f16" do
      int_t = Nx.tensor([1, 2, 3])
      assert_close(Nx.sin(ev(int_t)), Nx.sin(int_t))

      f64_t = Nx.tensor([0.5, 1.0, 2.0], type: :f64)
      assert_close(Nx.cos(ev(f64_t)), Nx.cos(f64_t))
      assert_close(Nx.sqrt(ev(f64_t)), Nx.sqrt(f64_t))

      f16_t = Nx.tensor([0.5, 1.0, 2.0], type: :f16)
      assert_close(Nx.tanh(ev(f16_t)), Nx.tanh(f16_t))
      assert_close(Nx.sqrt(ev(f16_t)), Nx.sqrt(f16_t))
    end
  end

  describe "reduce_max / reduce_min / all / any" do
    test "reduce_max/min match Nx.BinaryBackend exactly across axes, keep_axes, and dtypes" do
      tensors = [
        Nx.tensor([3, 1, 2]),
        Nx.tensor([[1, 5, 3], [9, 2, 8]]),
        Nx.tensor([[[1, 2], [3, 4]], [[5, 6], [0, 7]]]),
        Nx.tensor([[1.5, -2.0, 3.25], [0.0, 4.0, -1.0]], type: :f32),
        Nx.tensor([[10, 20, 30], [40, 50, 60]], type: :u64)
      ]

      opts_list = [
        [],
        [axes: [0]],
        [axes: [1]],
        [axes: [0], keep_axes: true],
        [axes: [1], keep_axes: true]
      ]

      for op <- [:reduce_max, :reduce_min], t <- tensors, opts <- opts_list, axes_in_rank?(t, opts) do
        assert_same(apply(Nx, op, [ev(t), opts]), apply(Nx, op, [t, opts]))
      end
    end

    test "all/any match Nx.BinaryBackend exactly across axes, keep_axes, and dtypes" do
      tensors = [
        Nx.tensor([1, 0, 2]),
        Nx.tensor([[1, 0, 3], [4, 5, 6]]),
        Nx.tensor([[0.0, 1.0], [2.0, 3.0]], type: :f32),
        Nx.tensor([[1, 1], [1, 0]], type: :u8)
      ]

      opts_list = [[], [axes: [0]], [axes: [1]], [axes: [1], keep_axes: true]]

      for op <- [:all, :any], t <- tensors, opts <- opts_list, axes_in_rank?(t, opts) do
        assert_same(apply(Nx, op, [ev(t), opts]), apply(Nx, op, [t, opts]))
      end
    end
  end

  describe "select" do
    test "match Nx.BinaryBackend exactly across shapes, broadcasting, and dtypes" do
      cases = [
        {Nx.tensor([1, 0, 1]), Nx.tensor([10, 20, 30]), Nx.tensor([-1, -2, -3])},
        {Nx.tensor([[1, 0], [0, 1]]), Nx.tensor([[1, 2], [3, 4]]), Nx.tensor([[5, 6], [7, 8]])},
        # scalar branches broadcast to the predicate shape
        {Nx.tensor([1, 0, 2, 0]), Nx.tensor(7), Nx.tensor(9)},
        # row broadcast of the branches
        {Nx.tensor([[1, 0, 1], [0, 1, 0]]), Nx.tensor([[100, 200, 300]]), Nx.tensor([[1, 2, 3]])},
        # float dtype
        {Nx.tensor([1, 0, 1]), Nx.tensor([1.5, 2.5, 3.5], type: :f32),
         Nx.tensor([0.0, 0.0, 0.0], type: :f32)},
        # mixed dtype -> merged (f32) output
        {Nx.tensor([0, 1, 0]), Nx.tensor([1, 2, 3], type: :s32),
         Nx.tensor([1.5, 2.5, 3.5], type: :f32)},
        # scalar predicate selects a whole branch
        {Nx.tensor(1), Nx.tensor([1, 2, 3]), Nx.tensor([4, 5, 6])},
        {Nx.tensor(0), Nx.tensor([1, 2, 3]), Nx.tensor([4, 5, 6])}
      ]

      for {p, a, b} <- cases do
        assert_same(Nx.select(ev(p), ev(a), ev(b)), Nx.select(p, a, b))
      end
    end
  end

  describe "is_nan / is_infinity / logical_not" do
    test "match Nx.BinaryBackend exactly across float (with nan/inf) and integer dtypes" do
      tensors = [
        Nx.tensor([1.0, 0.0, -2.5], type: :f32),
        Nx.tensor([:nan, :infinity, :neg_infinity, 0.0, 3.0], type: :f32),
        Nx.tensor([:nan, 1.0, :infinity], type: :f64),
        Nx.tensor([0.0, :nan, 2.0], type: :f16),
        Nx.tensor([0, 1, -2, 3]),
        Nx.tensor([0, 5, 0], type: :u8)
      ]

      for op <- [:is_nan, :is_infinity, :logical_not], t <- tensors do
        assert_same(apply(Nx, op, [ev(t)]), apply(Nx, op, [t]))
      end
    end
  end

  describe "count_leading_zeros / population_count / left_shift / right_shift" do
    test "match Nx.BinaryBackend exactly across integer dtypes" do
      unary_tensors = [
        Nx.tensor([0, 1, 2, 255], type: :u8),
        Nx.tensor([0, 1, -1, 127, -128], type: :s8),
        Nx.tensor([1, 256, 65535], type: :u16),
        Nx.tensor([0, 1, 2, 100], type: :s32),
        Nx.tensor([1, 2, 3], type: :u64),
        Nx.tensor([-1, -2], type: :s64)
      ]

      for op <- [:count_leading_zeros, :population_count], t <- unary_tensors do
        assert_same(apply(Nx, op, [ev(t)]), apply(Nx, op, [t]))
      end

      shift_cases = [
        {Nx.tensor([1, 2, 4], type: :u8), Nx.tensor([1, 2, 3], type: :u8)},
        {Nx.tensor([255, 16, 1], type: :u8), Nx.tensor([1, 2, 4], type: :u8)},
        {Nx.tensor([-8, -1, 16], type: :s32), Nx.tensor([1, 1, 2], type: :s32)},
        {Nx.tensor([1, 2, 3], type: :s64), Nx.tensor([10, 20, 30], type: :s64)},
        {Nx.tensor([100, 200], type: :u64), Nx.tensor([2, 3], type: :u64)}
      ]

      for op <- [:left_shift, :right_shift], {l, r} <- shift_cases do
        assert_same(apply(Nx, op, [ev(l), ev(r)]), apply(Nx, op, [l, r]))
      end
    end
  end

  describe "all_close" do
    test "match Nx.BinaryBackend across dtypes, tolerances, and inf/nan" do
      cases = [
        {Nx.tensor([1.0, 2.0, 3.0]), Nx.tensor([1.0, 2.0, 3.0]), []},
        {Nx.tensor([1.0, 2.0, 3.0]), Nx.tensor([1.0, 2.0, 3.001]), []},
        {Nx.tensor([1.0, 2.0]), Nx.tensor([1.05, 2.0]), [atol: 0.1]},
        {Nx.tensor([1.0, 2.0]), Nx.tensor([1.5, 2.0]), [atol: 0.1]},
        {Nx.tensor([[1.0, 2.0], [3.0, 4.0]]), Nx.tensor([[1.0, 2.0], [3.0, 4.0]]), []},
        {Nx.tensor([1, 2, 3]), Nx.tensor([1, 2, 3]), []},
        {Nx.tensor([1, 2, 3]), Nx.tensor([1, 2, 4]), []},
        {Nx.tensor([:infinity, 1.0]), Nx.tensor([:infinity, 1.0]), []},
        {Nx.tensor([:infinity, 1.0]), Nx.tensor([:neg_infinity, 1.0]), []},
        {Nx.tensor([:nan, 1.0]), Nx.tensor([:nan, 1.0]), []},
        {Nx.tensor([:nan, 1.0]), Nx.tensor([:nan, 1.0]), [equal_nan: true]},
        {Nx.tensor([1.0, 2.0]), Nx.tensor([1.0, 2.5]), [rtol: 0.0, atol: 0.0]}
      ]

      for {a, b, opts} <- cases do
        assert_same(Nx.all_close(ev(a), ev(b), opts), Nx.all_close(a, b, opts))
      end
    end
  end
end
