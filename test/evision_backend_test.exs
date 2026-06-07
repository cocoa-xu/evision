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

  # Move a tensor onto BinaryBackend (factor reconstruction is done there, since
  # Evision.Backend.multiply does matrix mult for two non-scalar operands).
  defp b(t), do: Nx.backend_copy(t, Nx.BinaryBackend)

  # Assert two tensors are close at a given tolerance (both moved to BinaryBackend first).
  # Reconstruction of cv-exact factors uses a tight tol; comparing canonical values to Nx's
  # iterative SVD/eigh reference uses a looser one (the reference is only ~1e-4 accurate).
  defp close_at(x, y, tol) do
    xb = b(x)
    yb = b(y)
    assert Nx.shape(xb) == Nx.shape(yb)

    assert Nx.to_number(Nx.all_close(xb, yb, atol: tol, rtol: tol)) == 1,
           "#{inspect(Nx.to_flat_list(xb))} vs #{inspect(Nx.to_flat_list(yb))}"
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

  describe "input immutability (shared-buffer safety)" do
    # Read-only NIFs share a cv-owned source buffer instead of deep-copying it, so a
    # reshape (or any internal pipeline that reshapes) can alias its input. These
    # assert that operating on a derived tensor never writes back into the original.
    for type <- [:f32, :s32] do
      test "ops on a reshape do not mutate the original (#{type})" do
        a = Nx.iota({32, 32}, type: unquote(type)) |> Nx.add(1) |> ev()
        a0 = Nx.to_binary(a)

        b = Nx.reshape(a, {1024})
        b0 = Nx.to_binary(b)

        _ = Nx.add(b, 7)
        _ = Nx.negate(b)
        _ = Nx.sum(b)
        _ = Nx.sort(Nx.reshape(b, {32, 32}), axis: 1)

        assert Nx.to_binary(a) == a0
        assert Nx.to_binary(b) == b0
      end

      test "reduction/scan/sort pipelines leave their input intact (#{type})" do
        a = Nx.iota({24, 48}, type: unquote(type)) |> Nx.add(1) |> ev()
        a0 = Nx.to_binary(a)

        _ = Nx.sum(a, axes: [1])
        _ = Nx.reduce_max(a, axes: [0])
        _ = Nx.cumulative_sum(a, axis: 1)
        _ = Nx.sort(a, axis: 1)

        assert Nx.to_binary(a) == a0
      end
    end

    test "multiple live aliases stay independent under GC" do
      a = Nx.iota({64, 64}, type: :f32) |> Nx.add(1.0) |> ev()
      a0 = Nx.to_binary(a)

      Enum.each(1..100, fn i ->
        view = Nx.reshape(a, {4096})
        _ = Nx.add(view, i * 1.0)
        _ = Nx.sum(view)
        if rem(i, 10) == 0, do: :erlang.garbage_collect()
      end)

      :erlang.garbage_collect()
      assert Nx.to_binary(a) == a0
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

    test "sort and argsort match Nx.BinaryBackend NaN ordering" do
      for t <- [
            Nx.tensor([1.0, :nan, 2.0, -1.0], type: :f32),
            Nx.tensor([[1.0, :nan, 2.0], [:nan, -1.0, 0.0]], type: :f64),
            Nx.tensor([1.0, :nan, 2.0, -1.0], type: :f16)
          ],
          fun <- [:sort, :argsort],
          opts <- [[], [direction: :desc], [axis: max(Nx.rank(t) - 1, 0)]] do
        assert_same(apply(Nx, fun, [ev(t), opts]), apply(Nx, fun, [t, opts]))
      end
    end

    test "match Nx.BinaryBackend for larger row-wise inputs" do
      t =
        Nx.iota({96, 512}, type: :s64)
        |> Nx.multiply(17)
        |> Nx.remainder(1009)
        |> Nx.as_type(:f32)

      for fun <- [:sort, :argsort], opts <- [[axis: 1], [axis: 1, direction: :desc]] do
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

    test "sum matches Nx.BinaryBackend for larger row-wise inputs" do
      t =
        Nx.iota({96, 512}, type: :f32)
        |> Nx.multiply(0.25)
        |> Nx.subtract(64.0)

      close_at(Nx.sum(ev(t), axes: [1]), Nx.sum(t, axes: [1]), 1.0e-4)
      close_at(Nx.sum(ev(t), axes: [0]), Nx.sum(t, axes: [0]), 1.0e-4)
    end

    test "cumulative max/min propagate NaNs like Nx.BinaryBackend" do
      t = Nx.tensor([1.0, :nan, 2.0], type: :f32)

      for op <- [:cumulative_max, :cumulative_min], opts <- [[], [reverse: true]] do
        assert_same(apply(Nx, op, [ev(t), opts]), apply(Nx, op, [t, opts]))
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

    test "matches Nx.BinaryBackend for larger copied blocks" do
      t = Nx.iota({32, 64, 8}, type: :s32)
      idx = Nx.remainder(Nx.iota({512}, type: :s64), 64)

      assert_same(Nx.take(ev(t), ev(idx), axis: 1), Nx.take(t, idx, axis: 1))
    end

    test "raises on out-of-bounds indices" do
      t = Nx.tensor([10, 20, 30])
      idx = Nx.tensor([0, 3])

      assert_raise RuntimeError, ~r/take: index out of bounds/, fn ->
        Nx.take(ev(t), ev(idx), axis: 0)
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

    test "matches Nx.BinaryBackend for larger leading-axis gathers" do
      t = Nx.iota({32, 32, 4}, type: :s32)
      g = Nx.iota({8192}, type: :s64)
      idx = Nx.stack([Nx.remainder(g, 32), Nx.remainder(Nx.quotient(g, 32), 32)], axis: 1)

      assert_same(Nx.gather(ev(t), ev(idx), axes: [0, 1]), Nx.gather(t, idx, axes: [0, 1]))
    end

    test "raises on out-of-bounds coordinates" do
      t = Nx.tensor([[1, 2], [3, 4]])
      idx = Nx.tensor([[0, 0], [2, 1]])

      assert_raise RuntimeError, ~r/gather: index out of bounds/, fn ->
        Nx.gather(ev(t), ev(idx))
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

  describe "multiply / divide by a scalar" do
    # Regression: multiplying or dividing by an integer scalar used to raise
    # "no function clause matching in Evision.Backend.to_nx/2". The per-element
    # branch handed a 1x1 integer scalar Mat to cv::multiply, whose scalar
    # fast-path rejects non-float scalars; broadcasting the scalar to the output
    # shape first turns it into a supported array-op-array call.
    test "integer-tensor multiply by an integer scalar matches Nx.BinaryBackend exactly" do
      for t <- [Nx.tensor([1, 2, 3], type: :s32), Nx.tensor([4, 9, 12], type: :s64)],
          s <- [2, -3, 0] do
        assert_same(Nx.multiply(ev(t), s), Nx.multiply(t, s))
        assert_same(Nx.multiply(s, ev(t)), Nx.multiply(s, t))
      end
    end

    test "float-tensor multiply by integer and float scalars matches Nx.BinaryBackend" do
      for t <- [Nx.tensor([1.0, 2.0, 3.0], type: :f32), Nx.tensor([[1.0, 2.0], [3.0, 4.0]], type: :f64)],
          s <- [2, -3, 0.5, -1.5] do
        assert_close(Nx.multiply(ev(t), s), Nx.multiply(t, s))
        assert_close(Nx.multiply(s, ev(t)), Nx.multiply(s, t))
      end
    end

    test "divide by integer and float scalars matches Nx.BinaryBackend" do
      for t <- [Nx.tensor([4, 9, 12], type: :s32), Nx.tensor([1.0, 2.0, 3.0], type: :f32)],
          s <- [2, -4, 0.5] do
        assert_close(Nx.divide(ev(t), s), Nx.divide(t, s))
        assert_close(Nx.divide(s, ev(t)), Nx.divide(s, t))
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

    test "match Nx.BinaryBackend for larger row-wise scans" do
      t =
        Nx.iota({96, 512}, type: :f32)
        |> Nx.multiply(0.25)
        |> Nx.subtract(64.0)

      close_at(Nx.cumulative_sum(ev(t), axis: 1), Nx.cumulative_sum(t, axis: 1), 1.0e-3)

      close_at(
        Nx.cumulative_max(ev(t), axis: 0, reverse: true),
        Nx.cumulative_max(t, axis: 0, reverse: true),
        1.0e-5
      )
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

    test "match Nx.BinaryBackend for larger elementwise inputs" do
      t =
        Nx.iota({256, 256}, type: :f32)
        |> Nx.divide(100.0)

      assert_close(Nx.sin(ev(t)), Nx.sin(t))
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

    test "reduce_max/min propagate NaNs like Nx.BinaryBackend" do
      t = Nx.tensor([[1.0, :nan, 2.0], [3.0, 4.0, 5.0]], type: :f32)

      for op <- [:reduce_max, :reduce_min], opts <- [[], [axes: [1]], [axes: [1], keep_axes: true]] do
        assert_same(apply(Nx, op, [ev(t), opts]), apply(Nx, op, [t, opts]))
      end
    end

    test "reduce_max/min match Nx.BinaryBackend for larger row-wise inputs" do
      t =
        Nx.iota({96, 512}, type: :f32)
        |> Nx.multiply(-3.0)
        |> Nx.add(7.0)

      for op <- [:reduce_max, :reduce_min], opts <- [[axes: [1]], [axes: [0]]] do
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

  describe "atan2 / pow / quotient / remainder" do
    test "atan2 matches approximately (float out, incl. integer inputs)" do
      for {l, r} <- [
            {Nx.tensor([1.0, -1.0, 0.0, 2.5]), Nx.tensor([1.0, 1.0, -1.0, 0.5])},
            {Nx.tensor([1, 2, 3]), Nx.tensor([4, 5, 6])}
          ] do
        assert_close(Nx.atan2(ev(l), ev(r)), Nx.atan2(l, r))
      end
    end

    test "pow matches (integer modular, float approximate)" do
      for {l, r} <- [
            {Nx.tensor([2, 3, 4]), Nx.tensor([3, 2, 1])},
            {Nx.tensor([2, 5], type: :u8), Nx.tensor([10, 2], type: :u8)},
            {Nx.tensor([-2, 3], type: :s32), Nx.tensor([3, 3], type: :s32)}
          ] do
        assert_same(Nx.pow(ev(l), ev(r)), Nx.pow(l, r))
      end

      for {l, r} <- [{Nx.tensor([2.0, 9.0, 4.0]), Nx.tensor([0.5, 0.5, 2.0])}] do
        assert_close(Nx.pow(ev(l), ev(r)), Nx.pow(l, r))
      end
    end

    test "quotient/remainder match exactly across integer signs" do
      for {l, r} <- [
            {Nx.tensor([7, -7, 8, -8]), Nx.tensor([2, 2, 3, 3])},
            {Nx.tensor([10, 20], type: :u8), Nx.tensor([3, 7], type: :u8)}
          ] do
        assert_same(Nx.quotient(ev(l), ev(r)), Nx.quotient(l, r))
        assert_same(Nx.remainder(ev(l), ev(r)), Nx.remainder(l, r))
      end
    end

    test "remainder matches approximately for floats" do
      for {l, r} <- [{Nx.tensor([5.5, -5.5, 7.0]), Nx.tensor([2.0, 2.0, 3.0])}] do
        assert_close(Nx.remainder(ev(l), ev(r)), Nx.remainder(l, r))
      end
    end
  end

  describe "slice / reverse" do
    test "slice matches across starts, lengths, strides, ranks, dtypes" do
      t = Nx.tensor([[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12]])

      cases = [
        {[0, 0], [2, 2], [1, 1]},
        {[1, 1], [2, 2], [1, 1]},
        {[0, 0], [3, 2], [1, 2]},
        {[0, 1], [2, 2], [2, 1]},
        {[1, 0], [1, 4], [1, 1]},
        {[5, 5], [2, 2], [1, 1]}
      ]

      for {s, l, st} <- cases do
        assert_same(Nx.slice(ev(t), s, l, strides: st), Nx.slice(t, s, l, strides: st))
      end

      t3 = Nx.iota({2, 3, 4}, type: :f32)
      assert_same(
        Nx.slice(ev(t3), [0, 1, 0], [2, 2, 4], strides: [1, 1, 2]),
        Nx.slice(t3, [0, 1, 0], [2, 2, 4], strides: [1, 1, 2])
      )
    end

    test "reverse matches across axes, ranks, and dtypes" do
      cases = [
        {Nx.tensor([1, 2, 3, 4]), [0]},
        {Nx.tensor([[1, 2, 3], [4, 5, 6]]), [0]},
        {Nx.tensor([[1, 2, 3], [4, 5, 6]]), [1]},
        {Nx.tensor([[1, 2, 3], [4, 5, 6]]), [0, 1]},
        {Nx.iota({2, 2, 2}, type: :f32), [0, 2]},
        {Nx.iota({2, 3, 4}, type: :s32), [1]}
      ]

      for {t, axes} <- cases do
        assert_same(Nx.reverse(ev(t), axes: axes), Nx.reverse(t, axes: axes))
      end
    end
  end

  describe "put_slice / concatenate" do
    test "put_slice matches across positions, clamping, ranks, dtypes" do
      t = Nx.tensor([[1, 2, 3], [4, 5, 6], [7, 8, 9]])

      cases = [
        {[0, 0], Nx.tensor([[10, 11], [12, 13]])},
        {[1, 1], Nx.tensor([[0]])},
        {[2, 2], Nx.tensor([[99, 99]])},
        {[0, 0], Nx.tensor([[1, 1, 1]])}
      ]

      for {start, slice} <- cases do
        assert_same(Nx.put_slice(ev(t), start, ev(slice)), Nx.put_slice(t, start, slice))
      end

      assert_same(
        Nx.put_slice(ev(Nx.tensor([1.0, 2.0, 3.0, 4.0])), [1], ev(Nx.tensor([9.0, 9.0]))),
        Nx.put_slice(Nx.tensor([1.0, 2.0, 3.0, 4.0]), [1], Nx.tensor([9.0, 9.0]))
      )
    end

    test "concatenate matches across axes, arities, sizes, and dtypes" do
      a = Nx.tensor([[1, 2], [3, 4]])
      b = Nx.tensor([[5, 6], [7, 8]])
      c = Nx.tensor([[9, 10], [11, 12]])

      assert_same(Nx.concatenate([ev(a), ev(b)], axis: 0), Nx.concatenate([a, b], axis: 0))
      assert_same(Nx.concatenate([ev(a), ev(b)], axis: 1), Nx.concatenate([a, b], axis: 1))
      assert_same(Nx.concatenate([ev(a), ev(b), ev(c)], axis: 0), Nx.concatenate([a, b, c], axis: 0))

      d = Nx.tensor([[1, 2, 3]])
      e = Nx.tensor([[4, 5, 6], [7, 8, 9]])
      assert_same(Nx.concatenate([ev(d), ev(e)], axis: 0), Nx.concatenate([d, e], axis: 0))

      assert_same(
        Nx.concatenate([ev(Nx.tensor([1, 2])), ev(Nx.tensor([3.0, 4.0]))]),
        Nx.concatenate([Nx.tensor([1, 2]), Nx.tensor([3.0, 4.0])])
      )
    end
  end

  describe "take_along_axis / top_k" do
    test "take_along_axis matches across axes, ranks, dtypes" do
      t = Nx.tensor([[1, 2, 3], [4, 5, 6]])

      cases = [
        {t, Nx.tensor([[0, 1, 1], [1, 0, 0]]), 0},
        {t, Nx.tensor([[0, 2, 1, 0], [2, 1, 2, 0]]), 1},
        {Nx.tensor([10, 20, 30, 40]), Nx.tensor([3, 0, 0, 2, 1]), 0},
        {Nx.iota({2, 3}, type: :f32), Nx.tensor([[0, 0, 0]]), 0}
      ]

      for {tt, idx, axis} <- cases do
        assert_same(
          Nx.take_along_axis(ev(tt), ev(idx), axis: axis),
          Nx.take_along_axis(tt, idx, axis: axis)
        )
      end
    end

    test "take_along_axis matches Nx.BinaryBackend for larger axis choices" do
      t = Nx.iota({64, 512}, type: :s32)
      axis0_idx = Nx.remainder(Nx.iota({64, 512}, type: :s64), 64)
      axis1_idx = Nx.remainder(Nx.iota({64, 512}, type: :s64), 512)

      assert_same(
        Nx.take_along_axis(ev(t), ev(axis0_idx), axis: 0),
        Nx.take_along_axis(t, axis0_idx, axis: 0)
      )

      assert_same(
        Nx.take_along_axis(ev(t), ev(axis1_idx), axis: 1),
        Nx.take_along_axis(t, axis1_idx, axis: 1)
      )
    end

    test "take_along_axis raises on out-of-bounds indices" do
      t = Nx.tensor([[1, 2, 3], [4, 5, 6]])
      idx = Nx.tensor([[0, 3, 1], [1, 0, 2]])

      assert_raise RuntimeError, ~r/take_along_axis: index out of bounds/, fn ->
        Nx.take_along_axis(ev(t), ev(idx), axis: 1)
      end
    end

    test "top_k matches Nx.BinaryBackend (values and indices)" do
      for {t, k} <- [
            {Nx.tensor([3, 1, 2, 5]), 2},
            {Nx.tensor([[3, 1, 2, 5], [9, 8, 7, 6]]), 2},
            {Nx.tensor([[1.5, 4.5, 2.5, 0.5]], type: :f32), 3}
          ] do
        {gv, gi} = Nx.top_k(ev(t), k: k)
        {wv, wi} = Nx.top_k(t, k: k)
        assert_same(gv, wv)
        assert_same(gi, wi)
      end
    end
  end

  describe "determinant" do
    test "matches Nx.BinaryBackend across sizes, dtypes, and batches" do
      tensors = [
        Nx.tensor([[2.0, 1.0], [1.0, 3.0]], type: :f64),
        Nx.tensor([[1.0, 2.0, 3.0], [0.0, 1.0, 4.0], [5.0, 6.0, 0.0]], type: :f64),
        Nx.tensor([[4.0, 3.0], [6.0, 3.0]], type: :f32),
        # integer input -> f32 output
        Nx.tensor([[1, 2], [3, 4]]),
        # batched {2, 2, 2}
        Nx.tensor([[[2.0, 0.0], [0.0, 3.0]], [[1.0, 2.0], [3.0, 4.0]]], type: :f64)
      ]

      for t <- tensors do
        assert_close(Nx.LinAlg.determinant(ev(t)), Nx.LinAlg.determinant(t))
      end
    end
  end

  describe "solve" do
    test "matches Nx.BinaryBackend for vector/matrix rhs, dtypes, and batches" do
      a = Nx.tensor([[1.0, 0.0, 1.0], [1.0, 1.0, 0.0], [1.0, 1.0, 1.0]], type: :f64)

      cases = [
        {a, Nx.tensor([0.0, 2.0, 1.0], type: :f64)},
        {a, Nx.tensor([[2.0, 2.0, 3.0], [2.0, 2.0, 4.0], [2.0, 0.0, 1.0]], type: :f64)},
        {Nx.tensor([[2.0, 1.0], [1.0, 3.0]], type: :f32), Nx.tensor([3.0, 5.0], type: :f32)},
        # integer input -> f32 solution
        {Nx.tensor([[3, 2], [1, 2]]), Nx.tensor([7, 5])}
      ]

      for {aa, bb} <- cases do
        assert_close(Nx.LinAlg.solve(ev(aa), ev(bb)), Nx.LinAlg.solve(aa, bb))
      end

      # batched: a {2, 2, 2}, b {2, 2} (a per-batch vector rhs)
      ab = Nx.tensor([[[14.0, 10.0], [9.0, 9.0]], [[4.0, 11.0], [2.0, 3.0]]], type: :f64)
      bv = Nx.tensor([[2.0, 3.0], [1.0, -3.0]], type: :f64)
      assert_close(Nx.LinAlg.solve(ev(ab), ev(bv)), Nx.LinAlg.solve(ab, bv))
    end
  end

  describe "svd" do
    test "reconstructs square A and matches singular values" do
      a = Nx.tensor([[1.0, 2.0, 0.0], [2.0, 3.0, 1.0], [0.0, 1.0, 2.0]], type: :f64)
      {u, s, vt} = Nx.LinAlg.svd(ev(a))
      a_rec = Nx.dot(Nx.multiply(b(u), Nx.reshape(b(s), {1, 3})), b(vt))
      close_at(a_rec, a, 1.0e-4)
      close_at(s, elem(Nx.LinAlg.svd(a), 1), 1.0e-3)
    end

    test "rectangular full/reduced shapes and singular values" do
      a = Nx.tensor([[1.0, 2.0], [3.0, 4.0], [5.0, 6.0]], type: :f64)

      {uf, sf, vtf} = Nx.LinAlg.svd(ev(a), full_matrices?: true)
      assert {Nx.shape(uf), Nx.shape(sf), Nx.shape(vtf)} == {{3, 3}, {2}, {2, 2}}

      {ur, sr, vtr} = Nx.LinAlg.svd(ev(a), full_matrices?: false)
      assert {Nx.shape(ur), Nx.shape(sr), Nx.shape(vtr)} == {{3, 2}, {2}, {2, 2}}

      close_at(sf, elem(Nx.LinAlg.svd(a, full_matrices?: true), 1), 1.0e-3)
      a_rec = Nx.dot(Nx.multiply(b(ur), Nx.reshape(b(sr), {1, 2})), b(vtr))
      close_at(a_rec, a, 1.0e-4)
    end
  end

  describe "eigh" do
    test "reconstructs symmetric A and matches eigenvalues as a set" do
      tensors = [
        Nx.tensor([[2.0, 1.0], [1.0, 2.0]], type: :f64),
        Nx.tensor([[2.0, 0.0, 0.0], [0.0, 3.0, 0.0], [0.0, 0.0, 5.0]], type: :f64),
        Nx.tensor([[4.0, 1.0, -2.0], [1.0, 2.0, 0.0], [-2.0, 0.0, 3.0]], type: :f64)
      ]

      for a <- tensors do
        n = elem(Nx.shape(a), 0)
        {vals, vecs} = Nx.LinAlg.eigh(ev(a))
        vb = b(vecs)
        a_rec = Nx.dot(Nx.multiply(vb, Nx.reshape(b(vals), {1, n})), Nx.transpose(vb))
        close_at(a_rec, a, 1.0e-4)

        # order/sign conventions differ from Nx, so compare eigenvalues as a sorted set
        {vals_ref, _} = Nx.LinAlg.eigh(a)
        close_at(Nx.sort(vals), Nx.sort(vals_ref), 1.0e-3)
      end
    end
  end

  describe "cholesky" do
    test "lower factor matches reference and reconstructs A" do
      tensors = [
        Nx.tensor([[4.0, 0.0], [0.0, 9.0]], type: :f64),
        Nx.tensor([[2.0, 1.0], [1.0, 2.0]], type: :f64),
        Nx.tensor([[4.0, 2.0, -2.0], [2.0, 10.0, 2.0], [-2.0, 2.0, 5.0]], type: :f64)
      ]

      for a <- tensors do
        l = Nx.LinAlg.cholesky(ev(a))
        # the lower factor with positive diagonal is unique -> matches BinaryBackend
        close_at(l, Nx.LinAlg.cholesky(a), 1.0e-4)
        lb = b(l)
        close_at(Nx.dot(lb, Nx.transpose(lb)), a, 1.0e-4)
      end
    end
  end

  describe "triangular_solve" do
    test "matches Nx.BinaryBackend across lower/upper, vector/matrix rhs, and transpose" do
      lower = Nx.tensor([[1.0, 0.0, 0.0], [2.0, 3.0, 0.0], [4.0, 5.0, 6.0]], type: :f64)
      upper = Nx.tensor([[2.0, 1.0, 1.0], [0.0, 3.0, 2.0], [0.0, 0.0, 4.0]], type: :f64)
      bvec = Nx.tensor([1.0, 2.0, 3.0], type: :f64)
      bmat = Nx.tensor([[1.0, 4.0], [2.0, 5.0], [3.0, 6.0]], type: :f64)

      cases = [
        {lower, bvec, []},
        {lower, bmat, []},
        {upper, bvec, [lower: false]},
        {upper, bmat, [lower: false]},
        {lower, bvec, [transform_a: :transpose]},
        {upper, bmat, [lower: false, transform_a: :transpose]}
      ]

      for {aa, bb, opts} <- cases do
        close_at(
          Nx.LinAlg.triangular_solve(ev(aa), ev(bb), opts),
          Nx.LinAlg.triangular_solve(aa, bb, opts),
          1.0e-4
        )
      end
    end

    test "left_side: false solves X*A = B" do
      a = Nx.tensor([[1.0, 0.0], [2.0, 3.0]], type: :f64)
      bb = Nx.tensor([[1.0, 2.0], [3.0, 4.0]], type: :f64)
      opts = [left_side: false]

      close_at(
        Nx.LinAlg.triangular_solve(ev(a), ev(bb), opts),
        Nx.LinAlg.triangular_solve(a, bb, opts),
        1.0e-4
      )
    end
  end

  describe "qr" do
    test "reduced and complete reconstruct A with orthonormal Q" do
      tensors = [
        Nx.tensor([[12.0, -51.0, 4.0], [6.0, 167.0, -68.0], [-4.0, 24.0, -41.0]], type: :f64),
        # tall 3x2
        Nx.tensor([[1.0, 2.0], [3.0, 4.0], [5.0, 6.0]], type: :f64),
        # wide 2x3
        Nx.tensor([[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]], type: :f64)
      ]

      for a <- tensors do
        {m, n} = Nx.shape(a)
        k = min(m, n)

        {q, r} = Nx.LinAlg.qr(ev(a), mode: :reduced)
        assert {Nx.shape(q), Nx.shape(r)} == {{m, k}, {k, n}}
        qb = b(q)
        close_at(Nx.dot(qb, b(r)), a, 1.0e-4)
        close_at(Nx.dot(Nx.transpose(qb), qb), Nx.eye(k, type: :f64), 1.0e-4)

        {q, r} = Nx.LinAlg.qr(ev(a), mode: :complete)
        assert {Nx.shape(q), Nx.shape(r)} == {{m, m}, {m, n}}
        qb = b(q)
        close_at(Nx.dot(qb, b(r)), a, 1.0e-4)
        close_at(Nx.dot(Nx.transpose(qb), qb), Nx.eye(m, type: :f64), 1.0e-4)
      end
    end
  end

  describe "lu" do
    test "P*L*U reconstructs A; P keeps the input dtype" do
      tensors = [
        Nx.tensor([[1.0, 2.0, 3.0], [4.0, 5.0, 6.0], [7.0, 8.0, 10.0]], type: :f64),
        Nx.tensor([[2.0, 1.0], [1.0, 3.0]], type: :f64),
        Nx.tensor([[4.0, 3.0], [6.0, 3.0]], type: :f64)
      ]

      for a <- tensors do
        {p, l, u} = Nx.LinAlg.lu(ev(a))
        close_at(Nx.dot(Nx.dot(b(p), b(l)), b(u)), a, 1.0e-4)
      end

      # integer input -> P keeps the input dtype, L/U are float
      ai = Nx.tensor([[1, 2, 3], [4, 5, 6], [7, 8, 10]])
      {p, l, u} = Nx.LinAlg.lu(ev(ai))
      assert Nx.type(p) == Nx.type(ai)
      assert match?({:f, _}, Nx.type(l)) and match?({:f, _}, Nx.type(u))
      close_at(Nx.dot(Nx.dot(b(p), b(l)), b(u)), Nx.as_type(ai, :f64), 1.0e-3)
    end
  end

  describe "window_sum / window_max / window_min / window_product" do
    test "match Nx.BinaryBackend exactly across windows, strides, padding, dilations, dtypes" do
      t3 = Nx.tensor([[[1, 2, 3], [4, 5, 6], [7, 8, 9]], [[10, 11, 12], [13, 14, 15], [16, 17, 18]]])
      t2 = Nx.tensor([[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12]], type: :s32)
      t1 = Nx.tensor([1, 2, 3, 4, 5], type: :u8)

      cases = [
        {t3, {1, 2, 1}, []},
        {t3, {2, 2, 1}, [strides: [1, 2, 3], padding: [{0, 1}, {2, 0}, {1, 1}]]},
        {t2, {2, 2}, [strides: [1, 2]]},
        {t2, {2, 2}, [window_dilations: [1, 2]]},
        {t2, {2, 3}, [padding: :same]},
        {t1, {2}, []},
        {t1, {2}, [strides: [2]]}
      ]

      for op <- [:window_sum, :window_max, :window_min, :window_product], {t, w, o} <- cases do
        assert_same(apply(Nx, op, [ev(t), w, o]), apply(Nx, op, [t, w, o]))
      end
    end

    test "match Nx.BinaryBackend approximately for floats" do
      t = Nx.tensor([[1.5, -2.0, 3.25, 0.5], [4.0, -1.0, 2.0, 6.0], [0.0, 3.0, -4.0, 1.0]], type: :f32)

      for op <- [:window_sum, :window_max, :window_min, :window_product],
          o <- [[], [strides: [2, 1]], [padding: :same], [window_dilations: [1, 2]]] do
        assert_close(apply(Nx, op, [ev(t), {2, 2}, o]), apply(Nx, op, [t, {2, 2}, o]))
      end
    end

    test "window max/min propagate NaNs like Nx.BinaryBackend" do
      t = Nx.tensor([1.0, :nan, 2.0], type: :f32)

      for op <- [:window_max, :window_min] do
        assert_same(apply(Nx, op, [ev(t), {2}]), apply(Nx, op, [t, {2}]))
      end
    end

    test "match Nx.BinaryBackend for larger padded windows" do
      t =
        Nx.iota({64, 64}, type: :f32)
        |> Nx.divide(10.0)

      opts = [padding: :same, window_dilations: [1, 2]]

      close_at(Nx.window_sum(ev(t), {3, 3}, opts), Nx.window_sum(t, {3, 3}, opts), 1.0e-4)
      assert_same(Nx.window_max(ev(t), {3, 3}, opts), Nx.window_max(t, {3, 3}, opts))
    end
  end

  describe "window_scatter_max / window_scatter_min" do
    test "match Nx.BinaryBackend across windows, strides, and duplicate targets" do
      t = Nx.tensor([[1, 3, 2, 7, 5, 4], [8, 6, 9, 2, 1, 0], [4, 5, 3, 1, 2, 6], [7, 0, 8, 3, 9, 2]])

      cases = [
        # non-overlapping {2,3} windows -> grid {2,2}
        {t, {2, 3}, [strides: [2, 3]], Nx.tensor([[10, 20], [30, 40]])},
        # overlapping (stride 1) -> grid {3,5}; exercises duplicate-target accumulation
        {t, {2, 2}, [strides: [1, 1]], Nx.add(Nx.iota({3, 5}, type: :s64), 1)},
        # stride {2,2} -> grid {2,3}
        {t, {2, 2}, [strides: [2, 2]], Nx.tensor([[1, 2, 3], [4, 5, 6]])}
      ]

      for op <- [:window_scatter_max, :window_scatter_min], {tt, w, o, src} <- cases do
        assert_same(apply(Nx, op, [ev(tt), ev(src), 0, w, o]), apply(Nx, op, [tt, src, 0, w, o]))
      end
    end

    test "match Nx.BinaryBackend for floats" do
      t = Nx.tensor([[1.0, 5.0, 2.0, 8.0], [3.0, 9.0, 4.0, 6.0], [7.0, 0.0, 5.0, 1.0]], type: :f32)
      src = Nx.tensor([[10.0, 20.0]], type: :f32)

      for op <- [:window_scatter_max, :window_scatter_min] do
        assert_close(
          apply(Nx, op, [ev(t), ev(src), 0.0, {3, 2}, [strides: [1, 2]]]),
          apply(Nx, op, [t, src, 0.0, {3, 2}, [strides: [1, 2]]])
        )
      end
    end
  end

  describe "conv" do
    test "matches Nx.BinaryBackend across strides, padding, dilations, groups, ranks" do
      img = Nx.iota({1, 1, 4, 4}, type: :f32)
      k11 = Nx.iota({1, 1, 2, 2}, type: :f32)
      img2 = Nx.iota({2, 3, 5, 5}, type: :f32)
      k2 = Nx.iota({4, 3, 2, 2}, type: :f32)
      imgd = Nx.iota({1, 4, 5, 5}, type: :f32)
      kd = Nx.iota({4, 1, 3, 3}, type: :f32)
      img1 = Nx.iota({1, 2, 8}, type: :f32)
      k1 = Nx.iota({3, 2, 3}, type: :f32)

      cases = [
        {img, k11, [strides: [1, 1]]},
        {img, k11, [strides: [2, 2]]},
        {img, k11, [padding: :same]},
        {img, k11, [padding: [{1, 1}, {0, 1}]]},
        {img, k11, [kernel_dilation: [2, 1]]},
        {img, k11, [input_dilation: [2, 2]]},
        {img2, k2, [strides: [2, 1]]},
        {img2, k2, [padding: :same]},
        # depthwise (feature groups)
        {imgd, kd, [feature_group_size: 4]},
        {img1, k1, [strides: [2]]},
        {img1, k1, [padding: :same, kernel_dilation: [2]]}
      ]

      for {i, k, o} <- cases do
        assert_close(Nx.conv(ev(i), ev(k), o), Nx.conv(i, k, o))
      end
    end

    test "matches Nx.BinaryBackend with permutations and batch groups" do
      # input given as NHWC, permuted to canonical NCHW
      img = Nx.iota({1, 5, 5, 3}, type: :f32)
      k = Nx.iota({4, 3, 2, 2}, type: :f32)
      o = [input_permutation: [0, 3, 1, 2]]
      assert_close(Nx.conv(ev(img), ev(k), o), Nx.conv(img, k, o))

      bimg = Nx.iota({4, 2, 4, 4}, type: :f32)
      bk = Nx.iota({2, 2, 2, 2}, type: :f32)
      bo = [batch_group_size: 2]
      assert_close(Nx.conv(ev(bimg), ev(bk), bo), Nx.conv(bimg, bk, bo))
    end

    test "matches Nx.BinaryBackend for larger grouped convolution" do
      img =
        Nx.iota({2, 4, 16, 16}, type: :f32)
        |> Nx.divide(100.0)

      kernel =
        Nx.iota({4, 2, 3, 3}, type: :f32)
        |> Nx.divide(50.0)

      opts = [feature_group_size: 2, padding: :same, strides: [1, 2], kernel_dilation: [1, 2]]

      assert_close(Nx.conv(ev(img), ev(kernel), opts), Nx.conv(img, kernel, opts))
    end

    test "im2row fast path tiles correctly across tile and batch boundaries" do
      # A tiny memory budget forces the im2row path into many tiles (here ~1-4 output
      # rows each), so tiles split mid-batch and the last tile is partial -- exercising
      # the boundary arithmetic that a single-tile conv never hits. The override is
      # process-local, so it never leaks into other (parallel) tests.
      Process.put(:evision_conv_im2row_budget_bytes, 256)

      img2 = Nx.iota({2, 3, 5, 5}, type: :f32)
      k2 = Nx.iota({4, 3, 2, 2}, type: :f32)
      imgd = Nx.iota({1, 4, 5, 5}, type: :f32)
      kd = Nx.iota({4, 1, 3, 3}, type: :f32)
      fimg = Nx.iota({2, 4, 16, 16}, type: :f32) |> Nx.divide(100.0)
      fk = Nx.iota({4, 2, 3, 3}, type: :f32) |> Nx.divide(50.0)

      cases = [
        {img2, k2, [strides: [2, 1]]},
        {img2, k2, [padding: :same]},
        {img2, k2, [padding: :same, kernel_dilation: [2, 1]]},
        {imgd, kd, [feature_group_size: 4]},
        {fimg, fk, [feature_group_size: 2, padding: :same, strides: [1, 2], kernel_dilation: [1, 2]]}
      ]

      for {i, k, o} <- cases do
        assert_close(Nx.conv(ev(i), ev(k), o), Nx.conv(i, k, o))
      end
    end
  end
end
