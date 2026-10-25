# Backend performance review

Reviewed from `c433cfe4f83096a4e739090f66094800e5e30b33` on 2026-10-25. No select experiments were repeated. The shared NIF was neither rebuilt nor replaced by this workstream.

## Result: remove redundant same-type casts in the Nx backend

Implemented one additional clause in `lib/evision/backend.ex`, in `as_mat_type/2` near line 852:

```elixir
# Keep the existing owner when no conversion is needed. Mutating NIFs still
# make their own private copy; borrowing NIFs still align binary-backed inputs.
defp as_mat_type(%Evision.Mat{type: type} = mat, type), do: mat
defp as_mat_type(mat, type), do: reject_error(Evision.Mat.as_type(mat, type))
```

Before this change, the helper always called `Evision.Mat.as_type/2`. Its native implementation, `c_src/modules/evision_mat.h:101-130`, calls `img.convertTo(ret, type)` with a fresh destination even when the depth already matches. That entails a NIF call and full copy. The helper is used before unary math, both operands of custom binary math and shifts, index buffers, scatter and window operations, and after reductions and linear algebra. `Evision.Mat.type/1` reads a struct field for an `Evision.Mat`, so matching that field does not require another NIF call.

The change is confined to the internal Nx helper. The public `Evision.Mat.as_type/2` API and converter flags remain unchanged. Actual type conversions still use the original path. Returning the existing immutable Mat resource retains its owner and does not change floating-point arithmetic, integer arithmetic, reduction order, or payload bytes.

The production change was made only after correctness probes and comparative timings. Root authorized this helper region and the new regression file. Other concurrent changes to `backend.ex` belong to their respective workstreams.

## Measured complete Nx calls

Hardware and runtime: Apple M6, macOS arm64, existing OpenCV 5 NIF, Elixir 1.20.4, OTP 29, Nx 0.12.1, 12 BEAM schedulers, and 12 OpenCV threads. Timings were collected during a coordinated window with other research probes paused. They are local workload measurements, not claims about every platform or tensor shape.

Both backend variants were compiled into separate in-memory modules from the baseline source. The candidate differed only in the helper clause above. This avoided changing the shared BEAM files or NIF and avoided relying on an unrelated loaded backend module. Each measured invocation included the complete `Nx` operation and `Nx.to_binary(result) |> byte_size()`. The native backend is eager; contiguous output materialization returns a resource binary without an extra full-buffer copy. Input preparation and backend transfer were outside the timed region.

For every case and storage owner: 10 warmups per variant, then 30 paired samples with order alternated AB/BA. A sample contained 20 invocations for 32 and 4,096 elements, or 2 invocations for 1,048,576 elements. The invoking process was garbage-collected before each pair. Reported values are median per-invocation microseconds. No memory bandwidth counter, peak RSS measurement, or cross-platform speed claim is implied.

At 1,048,576 input elements:

| Operation | Binary-backed baseline, us | Binary-backed candidate, us | Ratio | cv-owned baseline, us | cv-owned candidate, us | Ratio |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| `Nx.sqrt`, f32 | 162.77 | 159.69 | 1.02x | 174.29 | 119.21 | 1.46x |
| `Nx.rsqrt`, f32 | 142.75 | 90.77 | 1.57x | 151.92 | 89.38 | 1.70x |
| `Nx.left_shift(t, 1)`, s32 | 197.50 | 143.06 | 1.38x | 195.44 | 141.48 | 1.38x |
| `Nx.quotient(t, 2)`, s32 | 208.31 | 154.67 | 1.35x | 206.73 | 152.58 | 1.35x |
| `Nx.reduce_max`, f32, rows of 8 | 197.38 | 188.90 | 1.04x | 147.63 | 139.77 | 1.06x |
| `Nx.sum`, f64, rows of 8 | 217.52 | 199.92 | 1.09x | 101.96 | 85.31 | 1.20x |
| `Nx.put_slice` including source `Nx.slice`, f32 | 213.71 | 115.46 | 1.85x | 214.46 | 121.42 | 1.77x |

Representative p10-p90 sample ranges, in microseconds: owned sqrt 168.35-184.85 versus 117.31-122.65; owned quotient 196.21-218.40 versus 148.23-160.08; owned put_slice 209.67-227.56 versus 103.83-129.65. The smaller reduction improvements had overlapping ranges: owned max 142.77-165.67 versus 134.27-158.83. Treat those smaller ratios cautiously.

The nearly neutral binary-backed sqrt result is expected: the generated sqrt binding uses `INPUT_ONLY`, so its converter still copies binary-backed input to preserve the ownership policy. The candidate removes the earlier redundant copy but does not remove this native safety copy. Custom rsqrt uses `BORROWED_INPUT`, so an aligned binary can be read directly for the duration of the call. This distinction should remain documented if converter policy changes later.

Small and medium cv-owned inputs also benefit from avoiding a NIF call and resource allocation:

| Operation | 32 elements baseline/candidate, us | 4,096 elements baseline/candidate, us |
| --- | ---: | ---: |
| sqrt f32 | 7.01 / 4.07 | 15.15 / 10.14 |
| rsqrt f32 | 6.97 / 4.68 | 15.43 / 10.21 |
| left shift s32 | 14.11 / 9.28 | 29.15 / 18.77 |
| quotient s32 | 28.55 / 17.89 | 31.39 / 18.63 |
| max, rows of 8 | 26.66 / 21.54 | 26.22 / 21.03 |
| sum f64, rows of 8 | 26.18 / 20.89 | 26.86 / 21.03 |
| put_slice including slice | 18.48 / 13.41 | 20.13 / 16.40 |

Sub-millisecond VM timings are noisy. These tables support eliminating an unnecessary operation; they do not establish a universal speedup or a new threading threshold.

## Correctness and ownership evidence

The initial probe explicitly selected `Nx.BinaryBackend` for references. It checked exact output bytes for finite f32/f64 sqrt and max reductions, f16/bf16 sqrt, and all eight integer widths for shifts, quotient, and put_slice. Both binary-backed and cv-owned inputs stayed unchanged. Separate f32/f64 inputs contained positive and negative zero, infinities, positive and negative NaNs with distinct payloads, and positive and negative subnormals. Baseline and candidate math outputs matched byte-for-byte; copy operations also matched an independent byte-splice oracle.

The measured workload shapes were then checked in both storage modes. Baseline-candidate comparison was exact for every operation. BinaryBackend was an exact reference for finite sqrt, shift, quotient, reductions, and small/medium put_slice. For rsqrt, the BinaryBackend comparison used absolute and relative tolerances of `1.0e-6`; baseline-candidate bytes were still exact.

The BinaryBackend reference for the million-element put_slice was slow and was stopped before the shared build window. An independent byte-splice oracle then closed that gap: for a 4 MiB f32 input, copying 524,288 elements from index 0 to index 1 yielded exactly `input[0:4] <> input[0:2097152] <> input[2097156:]`. Both variants and both owner types matched these bytes, and the original inputs were unchanged. The correctness-only replacement probe completed in approximately 0.6 seconds including VM startup and compilation.

The checked-in `test/evision_backend_cast_elision_test.exs` adds six tests covering:

- Finite float math including f16/bf16, and all signed/unsigned integer widths.
- Binary-backed storage, cv-owned storage, cv-owned reshape aliases, and retained sub-binaries at offsets 1 through 7. The test verifies the offset binary retains a larger parent allocation.
- Mutation safety for overlapping put_slice, indexed_put, and indexed_add, with live original aliases.
- Same-type s64 index buffers, reductions, and window operations.
- Exact bytes for signed zeros, infinity signs, NaN payloads, and subnormals in copy operations, using an independent binary-splice oracle.
- Batched f64 Cholesky outputs after the creating process exits and garbage collection runs.

These six tests passed when the current backend source was compiled in an isolated VM against the existing NIF. Root owns subsequent full-suite and sanitizer validation. Command used for the targeted source-level check:

```sh
MIX_ENV=test mix run --no-compile --no-deps-check -e 'Code.compiler_options(ignore_module_conflict: true); Code.compile_file("lib/evision/backend.ex"); ExUnit.start(); Code.require_file("test/evision_backend_cast_elision_test.exs")'
```

An initial broader probe also found that f16/bf16 reduce_max fails in the baseline with `mat_reduce: unsupported depth`. Those reductions were excluded from performance claims and reported to the semantics workstream. This optimization does not address that pre-existing behavior.

### Audit of every helper consumer

All uses of `as_mat_type/2` in the reviewed backend were inspected, including calls through `maybe_narrow/3`:

| Consumer | Ownership and mutation assessment |
| --- | --- |
| Half-float sort, argsort, scan, predicates, and truthiness mask | The actual widen/narrow conversions remain necessary and are unchanged. Sort/scan/predicate kernels produce fresh results; sort writes into its copied output row. |
| Unary math and sqrt | Custom unary kernels allocate fresh destination Mats and borrow only during the NIF call. Generated sqrt uses `INPUT_ONLY`, including its existing binary ownership copy. |
| Custom binary math and shifts | Both operands use `BORROWED_INPUT`; output is separately allocated. Non-contiguous inputs are made continuous when needed. |
| put_slice | Base uses the writable converter `ArgInfo("base", 0)`, which retains its private copy. Slice is read-only. Removing an earlier cast cannot turn the write into an input mutation. |
| take, take_along_axis, gather | Indices and data are borrowed read-only; the output is fresh. Alignment and bounds validation remain in native code. |
| indexed_put and indexed_add | Target uses `ArgInfo("src", 0)` and is copied before updates. Indices and updates are borrowed read-only. |
| Aggregate reductions | The helper is applied to a newly allocated reduction result. Skipping a matching output cast leaves the arithmetic and accumulation order unchanged. |
| window reductions and window scatter | Sources are borrowed; native code allocates a fresh destination. Required half-float widening/narrowing is unchanged. |
| determinant, split_into, and stack_f64 | Binary conversion and reconstruction retain owners. Contiguous `to_binary` returns a resource binary; `from_binary_by_shape` retains that binary in an NIF environment. Generated decompositions use `INPUT_ONLY`; custom decompositions keep their existing converter policy. |
| Final narrowing | If the type differs, conversion still runs. If it matches, the original resource remains valid and `to_nx` retains it. |

Relevant converter policy is in `c_src/evision.cpp`, the `evision_to(..., Mat&, const ArgInfo&)` overload near line 488, and `c_src/ArgInfo.hpp`. In particular, binary-backed views cannot be allowed to escape merely by changing `INPUT_ONLY` into `BORROWED_INPUT`. `reshape.h` intentionally keeps `INPUT_ONLY`. LU and QR intentionally mutate a private converter copy. Those eager copies are safety requirements, not additional opportunities to remove blindly.

## Further candidates, not measured or implemented

### Broadcast contiguous suffixes

`c_src/modules/evision_backend/broadcast.h:28-83` computes zero source strides for stretched dimensions, but copies only the final axis as a block (`last` at line 61). For every output row it recomputes the base offset across all preceding dimensions. The comment describes a trailing contiguous run, but the code does not coalesce multiple matching trailing dimensions.

Candidate: coalesce the largest matching contiguous suffix, then copy or repeat complete suffix blocks and update outer offsets incrementally. Useful test shapes include `[1, 256, 256] -> [16, 256, 256]` and `[32, 1, 64, 64] -> [32, 16, 64, 64]`. The scalar case already uses exponential repeat copying and should be retained.

This is a byte-copy transformation with no arithmetic. Validate against an independent coordinate-to-source-offset oracle across ranks, source singleton placement, explicit Nx broadcast axes, element widths, exceptional floating encodings, and non-contiguous native input. Benchmark complete `Nx.broadcast` separately from the copy loop, including thin suffixes where coalescing may not help. Do not claim a benefit before measuring.

### Scan non-trailing axes without two full transposes

`along_axis/4` in `lib/evision/backend.ex` moves the selected axis last, invokes the row kernel, and transposes the full result back. `cumulative/4` uses this path; the native implementation in `c_src/modules/evision_backend/cumulative.h` only scans rows. A non-trailing scan therefore materializes two full transposes around the scan.

Candidate: a native `[outer, axis_length, inner]` scan that processes independent inner lanes while preserving each lane's original left-to-right or right-to-left recurrence. This can remove both transposes without reassociating additions or products. Floating-point accumulation order, per-step rounding, NaN selection, and signed-zero tie behavior must remain unchanged. Parallelize independent lanes, not one lane's prefix arithmetic.

Benchmark leading, middle, and trailing axes with inner widths 1, 8, 64, and 1,024; include both long/narrow and short/wide tensors. Check exact baseline bytes plus independent scalar recurrences, integer wrapping, reverse scans, ties, and NaN payloads. Interleaved-axis reductions are a related opportunity in `reduce_aggregate/5`, but a strided reduction can lose locality compared with the existing transpose. Neither path is assumed universally faster.

### Narrow read-only borrowing in linear algebra

`c_src/modules/evision_backend/linalg.h` currently uses writable converters for Cholesky's `a` near line 32 and triangular solve's `a` and `b` near lines 232-233. Their bodies only read these inputs and allocate fresh results. They could use `BORROWED_INPUT` with the existing non-contiguous clone checks. That would remove one input copy for aligned contiguous storage without changing arithmetic.

This is unmeasured. Large Cholesky is dominated by arithmetic, and tiny batched matrices may be dominated by dispatch. Measure single and batched matrices of sizes 8, 16, 64, and 256, including vector and multiple-column triangular right-hand sides. Validate exact baseline results, independent residuals, ignored-triangle behavior, singular errors, owner lifetimes, sub-binary alignment, and unchanged inputs. LU and QR must retain their writable converter copies because they modify their working matrix in place.

### Native batching and non-contiguous export, lower priority

`split_into/3` and `stack_f64/2` near the end of `backend.ex` create per-batch resources and NIF calls, and stack outputs through `IO.iodata_to_binary`. Do not describe every split as a data copy: contiguous `to_binary` and `from_binary_by_shape` retain resource/binary storage, while concatenation actually materializes the combined buffer. The adopted same-type helper already removes avoidable f64 casts around this pipeline. A native batched operation could reduce dispatch and resource allocations, but requires its own profiling and must preserve batch order and failure behavior.

`c_src/modules/evision_backend/to_binary.h:45-52` clones an entire non-contiguous matrix and then copies the requested binary prefix. A stride-aware copy directly into the requested binary could avoid both the full clone and unnecessary work for small limits. Most backend outputs are contiguous and several public ROI paths already clone, so first establish that this path matters in actual workloads. Test limits that end mid-row, channel widths, arbitrary dimensions, and exact logical byte order before benchmarking.

### GEMM deferred behind semantic fixes

The semantics workstream reproduced incorrect multiply dispatch and ignored contracting axes in dot. GEMM performance work should use a corrected shape/type contract and independently checked results first. The earlier Accelerate probe already demonstrated that speed alone was insufficient because signed-zero and NaN payload behavior changed. No BLAS/GEMM replacement or reduction reassociation was attempted here.

## Reproducing the comparison

The following standalone scratch harness recreates the measured variants and timing procedure. Run it with the existing test build using `MIX_ENV=test mix run --no-compile --no-deps-check /path/to/probe.exs`. It does not overwrite shared artifacts. Run correctness checks before timing and coordinate an idle measurement window. The committed regression file and the independent splice formula above specify the correctness oracles.

```elixir
Nx.default_backend(Nx.BinaryBackend)
{source, 0} = System.cmd("git", ["show", "c433cfe4f83096a4e739090f66094800e5e30b33:lib/evision/backend.ex"])
old = "  defp as_mat_type(mat, type), do: reject_error(Evision.Mat.as_type(mat, type))"
new = "  defp as_mat_type(%Evision.Mat{type: type} = mat, type), do: mat\n" <> old
Code.compiler_options(ignore_module_conflict: true, no_warn_undefined: :all)
Code.compile_string(String.replace(source, "Evision.Backend", "Evision.ResearchBaseline"))
Code.compile_string(source |> String.replace(old, new) |> String.replace("Evision.Backend", "Evision.ResearchCandidate"))

defmodule CastProbe do
  def copy(input, backend, storage) do
    t = Nx.backend_copy(input, backend)
    if storage == :owned, do: Nx.backend_copy(t, backend), else: t
  end

  def median(v), do: Enum.at(Enum.sort(v), div(length(v), 2))
  def percentile(v, p), do: Enum.at(Enum.sort(v), trunc((length(v) - 1) * p))

  def sample(fun, reps) do
    start = System.monotonic_time(:nanosecond)
    for _ <- 1..reps, do: fun.()
    (System.monotonic_time(:nanosecond) - start) / reps / 1000
  end

  def bench(name, input, fun, storage, reps) do
    a = copy(input, Evision.ResearchBaseline, storage)
    b = copy(input, Evision.ResearchCandidate, storage)
    fa = fn -> fun.(a) |> Nx.to_binary() |> byte_size() end
    fb = fn -> fun.(b) |> Nx.to_binary() |> byte_size() end
    for _ <- 1..10, do: (fa.(); fb.())
    {ta, tb} = Enum.reduce(1..30, {[], []}, fn round, {as, bs} ->
      :erlang.garbage_collect()
      {x, y} = if rem(round, 2) == 0 do
        x = sample(fa, reps)
        y = sample(fb, reps)
        {x, y}
      else
        y = sample(fb, reps)
        x = sample(fa, reps)
        {x, y}
      end
      {[x | as], [y | bs]}
    end)
    IO.inspect({name, storage, Nx.size(input), median(ta), median(tb),
      median(ta) / median(tb), percentile(ta, 0.1), percentile(ta, 0.9),
      percentile(tb, 0.1), percentile(tb, 0.9)})
  end
end

for count <- [32, 4096, 1_048_576] do
  f32 = Nx.from_binary(:binary.copy(<<0::float-native-32, 1::float-native-32,
    4::float-native-32, 9::float-native-32>>, div(count, 4)), :f32)
  s32 = Nx.from_binary(:binary.copy(<<0::signed-native-32, 1::signed-native-32,
    4::signed-native-32, 9::signed-native-32>>, div(count, 4)), :s32)
  f64 = Nx.as_type(f32, :f64)
  cases = [
    {"sqrt_f32", f32, &Nx.sqrt/1},
    {"rsqrt_f32", Nx.add(f32, 1), &Nx.rsqrt/1},
    {"shift_s32", s32, &Nx.left_shift(&1, 1)},
    {"quotient_s32", s32, &Nx.quotient(&1, 2)},
    {"reduce_max_f32", Nx.reshape(f32, {div(count, 8), 8}), &Nx.reduce_max(&1, axes: [1])},
    {"sum_f64", Nx.reshape(f64, {div(count, 8), 8}), &Nx.sum(&1, axes: [1])},
    {"put_slice_f32", f32, &Nx.put_slice(&1, [1], Nx.slice(&1, [0], [div(count, 2)]))}
  ]
  for {name, input, fun} <- cases, storage <- [:binary, :owned] do
    CastProbe.bench(name, input, fun, storage, if(count > 4096, do: 2, else: 20))
  end
end
```

## Checkpoint and handoff

The intended profile was identified and live `/status` was inspected before substantial work and at research checkpoints, using the separate `Weekly limit` field. The requested reset-to-100% stop condition was not observed. No account identity or quota snapshot is stored in this project report.

Source and tests are frozen for root's integration and sanitizer runs. No branch changes, commits, pushes, or shared NIF builds were performed by this workstream. Remaining candidate ideas are explicitly unmeasured. The timing harness is preserved above and its syntax was checked. Temporary scripts and logs under `_build/research-backend-performance` were removed after verification; the status-only CLI was closed.
