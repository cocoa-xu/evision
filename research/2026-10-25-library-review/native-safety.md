# Native binding and memory-safety review

Date: 2026-10-25. Base: `c433cfe4f83096a4e739090f66094800e5e30b33`.

This first pass confirmed two public correctness bugs and prepared focused fixes
with five regression tests. It did not reproduce a new out-of-bounds access,
use-after-free, or parallel data race. The root agent owns the coordinated NIF
rebuild, complete suite, and sanitizer validation; this agent did not replace or
rebuild the shared NIF.

## Runtime verification

- Required status checks completed; the requested reset stop condition was not reached.
  The inspection CLI was closed after the audit checkpoint.
- Probes used `MIX_ENV=test mix run --no-compile --no-deps-check`.
  `:code.which(:evision_nif)` resolved to this checkout's
  `_build/test/lib/evision/ebin/evision_nif.beam`; `:code.priv_dir(:evision)`
  resolved to `_build/test/lib/evision/priv`; OpenCV reported `5.0.0`.
- Scratch was isolated under `_build/research-native-safety` and removed after
  the results were recorded.

## Confirmed: moments output loses all calculated values

Location: `c_src/evision.cpp:2257`, specifically the successful return at line
2293. The converter creates an empty map `ret`, successively inserts the 24
moment values into `iter`, and returned the original empty `ret`.

Minimal reproducer:

```elixir
image = Evision.Mat.ones({2, 2}, :u8)
Evision.moments(image)
```

Observed before rebuilding: `%{}`. Expected: the 24 raw, central, and normalized
moments, including `m00: 4.0`, `m10: 2.0`, `m01: 2.0`, `mu20: 1.0`, and
`nu20: 0.0625`. This affects `Evision.moments/1`, `Evision.moments/2`, direct NIF
callers, and consumers that pass these values to Hu moments or compute centroids.
The computation itself succeeds; the native-to-BEAM conversion drops its result.

Authorized fix: return `iter`. There is no ownership or allocation change.

Regression coverage in `test/evision_moments_test.exs`:

1. Assert the complete 24-value map for a 2 by 2 matrix of ones.
2. Check the difference between weighted moments and `binaryImage: true` for
   `[[0, 1], [2, 3]]`, and verify the input bytes remain unchanged.

Pre-rebuild result: both new tests failed, with actual `%{}`. No claim of a
post-fix pass was made before the root agent's rebuild.

## Confirmed: channel reinterpretation rejects four supported scalar types

Location: `c_src/modules/evision_mat.h:974`, in
`evision_cv_mat_last_dim_as_channel`.

The previous guard enumerated eight older OpenCV type constants. It rejected
single-channel `u32`, `s64`, `u64`, and `bf16` matrices even though the binary
constructors support those scalar types. The error incorrectly said the matrix
already had channel information.

Minimal reproducer:

```elixir
Evision.Mat.ones({2, 2, 3}, :u32)
|> Evision.Mat.last_dim_as_channel()
```

Observed before rebuilding: `{:error, "image already has channel info"}`.
Expected: a `u32` Mat with `dims: 2`, `channels: 3`, logical shape `{2, 2, 3}`,
and unchanged bytes. The same failure was reproduced for `s64`, `u64`, and
`bf16`; `f32` succeeded as the control.

This also affects `Evision.Mat.update_roi/3` on images of these types. The Elixir
wrapper flattens the channels, applies the update, then calls
`last_dim_as_channel/1` to restore the image layout. A valid ROI update therefore
ends in this unrelated error.

Authorized fix: check `src.channels() == 1` instead of an enumerated dtype
whitelist. The existing dimension, channel-count, continuity, and owned-buffer
handling remain in place. The condition uses the longstanding OpenCV Mat API
and introduces no OpenCV 5-only symbol, preserving the older API compatibility
of this particular guard.

Regression coverage in `test/evision_mat_channel_test.exs`:

1. Reinterpret and round-trip all 12 supported scalar types, checking dtype,
   dimensions, channels, shape, and exact bytes, for both binary-backed and
   OpenCV-owned inputs.
2. Reject inputs already containing multiple channels for every type.
3. Update an actual three-channel image ROI for every type, checking restored
   channels, exact output bytes, and unchanged source and patch buffers.

Pre-rebuild result: round-trip and ROI tests failed at `u32`; the multichannel
rejection test passed. Root coordinates the post-fix result.

## Additional boundary review and runtime probe

Targeted reads covered the shared shape validators; Mat and tensor-map
conversion; `to_binary`, batching, broadcasting, transpose and reshape;
concatenation, strided copy and slice insertion; gather, take, take-along-axis
and indexed updates; reduction, cumulative operations, convolution and window
coordinate arithmetic; and the NIF exception dispatcher.

Observations supporting the current implementation:

- Flat backend tensor validation bounds counts by `INT_MAX` and validates byte
  extents. Coordinate helpers compare caller metadata with actual Mat storage.
- Gather and take-along-axis use atomic error flags and avoid a copy for an
  invalid coordinate. The parallel take path validates indices before its copy
  pass. Writable indexed operations use a private source copy.
- Copy kernels either materialize continuous storage or use strides/row
  pointers. Convolution shape validation bounds intermediate element products;
  coordinate expressions widen before multiplying dimensions and dilations.
- The NIF registration dispatcher catches OpenCV and standard C++ exceptions,
  including allocations outside the inner `ERRWRAP2` blocks.

An isolated parallel error-path probe exercised `mat_take`, `mat_gather`, and
`mat_take_along_axis` with `262_145` indices. All indices were zero except the
last value, which was `2` for a two-element source. This count exceeds the
applicable native parallel thresholds. Eight calls per operation were run with
four concurrent BEAM tasks.

Observed: all 24 calls returned an `index out of bounds` error, source and index
binaries remained unchanged, and a subsequent valid take returned `<<20, 10>>`.
This is evidence for the tested error paths, not a general race-freedom or
sanitizer result.

Reproduction outline:

```elixir
count = 262_145
source = Evision.Mat.from_binary_by_shape(<<10, 20>>, :u8, {1, 2}).ref
binary = :binary.copy(<<0::signed-native-64>>, count - 1) <> <<2::signed-native-64>>
indices = Evision.Mat.from_binary_by_shape(binary, :s64, {1, count}).ref
dims = Evision.Mat.from_binary_by_shape(<<2::signed-native-64>>, :s64, {1}).ref

:evision_nif.mat_take(src: source, indices: indices, outer: 1, axis_dim: 2, inner: 1)
:evision_nif.mat_gather(src: source, indices: indices, dims: dims, inner: 1)
:evision_nif.mat_take_along_axis(
  src: source, indices: indices, in_dims: [1, 2], out_dims: [1, count], axis: 1
)
```

The previous select exact-boundary, unaligned-input, GC ownership, and f16 ROI
audits were not repeated.

## Deferred converter issue, no established current public surface

`c_src/evision.cpp:2489` specializes conversion to
`std::vector<std::vector<int>>`. It calls `value.resize(n)` at line 2507 and then
`value.push_back(inner_val)` at line 2538. For nonempty input this leaves `n`
initial empty vectors before the `n` parsed vectors.

This is a definite internal logic defect, but the current OpenCV 5 generated
bindings do not expose a confirmed caller of this specialization. In particular,
`vector_MatShape` is now a vector of the distinct `cv::MatShape` struct, not a
vector of integer vectors. The handwritten concatenate implementation parses
its nested shape list itself. Do not attribute the defect to current DNN
methods without additional evidence.

Suggested follow-up: identify an actual supported generated caller, including
older OpenCV compatibility builds, and add a converter-level regression before
changing it. The likely local repair is assignment to `value[i]` rather than
appending after resize. No production change was made for this issue.

## Handoff

Prepared production changes are limited to the moments return and the channel
guard. Their new test files are owned by this audit. Source and tests were
frozen when the root agent began the coordinated build. The root agent should
record normal and ASan/UBSan suite results in the shared review summary and
handle commits and cleanup of its build artifacts.
