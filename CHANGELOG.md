# Changelog

## v1.1.0-rc.0 (2026-10-27)

First release candidate for v1.1.0. Changes below are relative to v1.0.0 and
include additions first shipped in v1.0.1-rc.0. OpenCV remains at 5.0.0; the Nx
requirement remains `~> 0.12.1`.

### Compatibility

- Numerical operations and conversions use OpenCV behavior, including rounding,
  overflow and nonfinite values. Results are not guaranteed to match
  `Nx.BinaryBackend` bit for bit.
- Array `Nx.min`/`Nx.max` now honor the requested output type. This fixes incorrect
  types and values for mixed inputs, including vectorized numeric scalars. For
  example, `max(u8[255], s8[127])` now returns `s16[255]` instead of `s8[127]`.
  These corrections can change successful old results, including floating-point
  rounding when the output type changes ([#358](https://github.com/cocoa-xu/evision/pull/358)).
- `:f16` and `{:f, 16}` are now recognized as native types. Custom
  `unsupported_type_map` entries for those types no longer apply. Request `:f32`
  explicitly if f32 storage is required. Previously unsupported half reductions
  can now succeed; native operations without half support still report errors
  ([#356](https://github.com/cocoa-xu/evision/pull/356),
  [#359](https://github.com/cocoa-xu/evision/pull/359),
  [#360](https://github.com/cocoa-xu/evision/pull/360)).
- The legacy Erlang `evision_nif:cv_16SU1/0` spelling is retained as an alias of
  the corrected `cv_16UC1/0` helper.

### Added

- Runtime NEON/AVX2 dispatch for dense Nx `select`, with
  `Evision.Backend.cpu_dispatch_info/0` and `set_cpu_dispatch/1`.
  `EVISION_CPU_DISPATCH=baseline` disables the optional Evision kernels. It does
  not disable OpenCV's own dispatch or compiler-generated baseline SIMD
  ([#346](https://github.com/cocoa-xu/evision/pull/346)).
- f16/bf16 sum, minimum, maximum and product reductions, including axis subsets
  and vectorized inputs. Sum/extrema use OpenCV; products use ordered f64
  accumulation and native output conversion (#359, #360).
- OpenCV.js correspondence metadata and a CSP-compatible runtime build.
  `Evision.JS.opencv_js_url/0,1` and `opencv_js_path/0` expose its URL and optional
  embedded file, with matching Erlang APIs. Embedding is opt-in via
  `config :evision, Evision.JS, embed_js: true` or `EVISION_EMBED_JS=true`
  ([#328](https://github.com/cocoa-xu/evision/pull/328),
  [#332](https://github.com/cocoa-xu/evision/pull/332)).
- BEAM type-code helpers for OpenCV 5.0's `CV_64S`, `CV_64U`, `CV_32U`, `CV_16BF`
  and `CV_Bool` depths, including channel-count variants.

### Fixed

- Validate native shapes, binary extents, indices, allocation sizes and ROI
  bounds before memory access; contain C++ exceptions at registered NIF
  boundaries. Define integer overflow in custom reductions/window arithmetic
  and reject invalid floating-to-integer initial values
  ([#338](https://github.com/cocoa-xu/evision/pull/338),
  [#345](https://github.com/cocoa-xu/evision/pull/345)).
- Correct f16/bf16 decoding and cover native byte order, including big-endian
  s390x. Strengthen borrowed/offset-storage regression tests (#345,
  [#362](https://github.com/cocoa-xu/evision/pull/362)).
- Use native elementwise multiplication and honor nonbatched f32/f64 dot
  contraction axes through GEMM, including matrix inputs produced by `squeeze`.
  Normalize scalar comparison headers for OpenCV
  ([#352](https://github.com/cocoa-xu/evision/pull/352),
  [#353](https://github.com/cocoa-xu/evision/pull/353),
  [#357](https://github.com/cocoa-xu/evision/pull/357)).
- Return populated image moments, validate channel count when reinterpreting
  dimensions, and unwrap generated plain-struct setter arguments correctly
  ([#337](https://github.com/cocoa-xu/evision/pull/337),
  [#350](https://github.com/cocoa-xu/evision/pull/350),
  [#351](https://github.com/cocoa-xu/evision/pull/351)).
- Keep benchmark reference inputs on `Nx.BinaryBackend` regardless of the
  caller's default backend ([#349](https://github.com/cocoa-xu/evision/pull/349)).
- Include required native helper headers in Hex source packages, resolve installed
  OpenCV FLANN headers, handle HTTP response headers up to 64 KiB, and retain
  prerelease suffixes in the Hex release script
  ([#334](https://github.com/cocoa-xu/evision/pull/334),
  [#347](https://github.com/cocoa-xu/evision/pull/347),
  [#361](https://github.com/cocoa-xu/evision/pull/361)).
- Address Elixir 1.20 compile warnings, simplify precompiled deployment, and retry
  transient release-asset upload failures.

### Performance

- Reuse compatible read-only Mat inputs, reduce redundant copies and casts,
  concatenate into one output allocation, fuse select mask/broadcast reads,
  coalesce contiguous broadcast copies, and write unary results directly.
  Matching min/max matrix/vector headers avoid extra reshape calls
  (#339-344, #354-355, [#364](https://github.com/cocoa-xu/evision/pull/364)).
- For #364 alone, measured complete f32 min calls (including `Nx.to_binary`) on
  Apple M6 improve from 8.28 to 3.85 microseconds for 64 owned elements, and from
  196.87 to 182.15 microseconds for 1,048,576 binary-backed elements. Tested
  outputs retain exact bytes; these are workload-specific measurements, not
  speedup guarantees for the entire release or other platforms.

### Known limitations

- Mixed-type array comparisons still have lossy promotion and unsupported half
  combinations. Batched dot is not newly supported in this release.

## v1.0.1-rc.0 (2026-07-20)

- Add OpenCV.js correspondence metadata and a CSP-compatible runtime build (#328).
- Address Elixir 1.20 compile warnings and simplify precompiled deployment
  (#329-331).
- Include additional Python generator directories in source packages.

## v1.0.0 (2026-06-24)

Stable release of the OpenCV 5.0 and Nx backend work described in v1.0.0-rc.0.
Changes since that candidate:

- Update precompiled CUDA variants to CUDA 12.9 and CUDA 13; remove CUDA 11
  variants. CUDA requirements are separate from the CPU-only builds.
- Fix 64-bit CUDA type handling on Windows/MSVC and MLAS cross-compilation for
  s390x/riscv64.
- Add selective Linux GNU precompile dispatch and improve release download retries.

## v1.0.0-rc.0 (2026-06-08)

This is the first release candidate for evision **v1.0.0**, the first major
version. Two headline changes land together:

- evision now targets **OpenCV 5.0.0**. This is a breaking upgrade (C++17, the
  legacy C API removed, modules reorganised, new core element types, and a
  rewritten DNN engine); see OpenCV's release notes for the upstream details.
- **`Evision.Backend` is now a working Nx backend**, implementing the majority
  of the `Nx.Backend` behaviour with results verified against
  `Nx.BinaryBackend`.

### Added

- **`Evision.Backend` is now a working Nx backend
  ([#48](https://github.com/cocoa-xu/evision/issues/48)).** 72 of the 85
  `Nx.Backend` callbacks are implemented, each verified element-for-element
  against `Nx.BinaryBackend` across dtypes, ranks, axes, and broadcasting, so
  `cv::Mat` can back `Nx` tensors for most workflows. Newly implemented
  callbacks include:
  - Reductions: `sum`, `product`, `reduce_max`, `reduce_min`, `all`, `any`, and
    `argmax`/`argmin` (backed by `cv::reduceArgMax`/`reduceArgMin`, with Nx
    tie-break semantics).
  - Cumulative ops: `cumulative_sum`, `cumulative_product`, `cumulative_min`,
    `cumulative_max`.
  - Sorting: `sort` and `argsort` along any axis, stable in both directions,
    including the wide-integer depths (`u32`/`s64`/`u64`) that `cv::sort`
    rejects.
  - Indexing: `take`, `gather`, `indexed_add`, and `indexed_put`, with
    Nx-compatible bounds checking.
  - Elementwise unary math and `select`.
  - `init/1` (required by the `Nx.Backend` behaviour since Nx 0.7) and scalar
    (0-dimensional) tensors in `from_binary`.
- OpenCV 5.0's new native depths are mapped to Nx dtypes: `CV_64S` to `{:s, 64}`,
  `CV_32U` to `{:u, 32}`, `CV_64U` to `{:u, 64}`, and `CV_16BF` to `{:bf, 16}`.
  64-bit values now round-trip through `cv::Mat` losslessly (the old s64-to-s32
  downcast that truncated values above 2^31 is gone), and `Evision.Mat.at/2`
  returns full-width 64-bit values.
- Haar/HOG parity: `Evision.CascadeClassifier` and `Evision.HOGDescriptor` build
  again via the contrib `xobjdetect` module, where OpenCV 5.0 moved them.
- `mix evision.backend.bench`, a benchmark task for the Evision backend with an
  optional Torchx comparison.

### Changed

- Uses OpenCV 5.0.0.
- Requires Nx `~> 0.12.1`.
- Module reorganisation follows OpenCV 5.0. Most classes keep their `Evision.*`
  names, but a few feature detectors moved to the contrib `xfeatures2d` module
  and are now under `Evision.XFeatures2D.*`: `AKAZE`, `KAZE`,
  `AgastFeatureDetector`, and `BRISK`.
- Multi-channel `raw_type` codes changed. OpenCV 5.0 bumped `CV_CN_SHIFT` from 3
  to 5, so a multi-channel `Mat`'s integer type code differs (for example
  `cv_8UC3` is now 64, was 16). `Evision.Constant.cv_8UC3/0` and friends compute
  the correct 5.0 values; code that hardcoded these numbers must be updated.
- `Evision.VideoWriter.write/2` now returns a boolean. OpenCV 5.0 changed
  `cv::VideoWriter::write` to return a success flag instead of `void`, so the
  call no longer returns the writer; write to the same writer handle in a loop.

### Removed

- OpenCV 4.x support and the multi-version selection mechanism. evision targets
  OpenCV 5.0.x only.
- The DNN Darknet and Caffe importers (removed upstream in 5.0). Use
  `Evision.DNN.readNetFromONNX/1` or ONNX-converted models instead.
- The DNN Halide backend (removed upstream in 5.0).

### Fixed

- `Evision.Backend` N-dimensional broadcasting now matches Nx semantics for
  rank-differing and multi-axis broadcasts (e.g. `{3, 4}` to `{2, 3, 4}`,
  `{2, 1, 4}` to `{2, 3, 4}`) and honours Nx's explicit `:axes`, so non
  right-aligned broadcasts are correct. Previously every elementwise binary op
  (`add`/`subtract`/`divide`/`min`/`max`/comparisons) could silently disagree
  with `Nx.BinaryBackend`, and on AArch64 a divide-by-zero in the tiling path
  returned garbage instead of trapping.
- `Evision.Backend` `logical_and`/`logical_or`/`logical_xor` now use truthiness
  semantics, so they are correct for non-boolean inputs (`logical_and(2, 1)` is
  `1`, not `0`) and `logical_xor` no longer raises on non-`u8` types.
- `Evision.Backend` integer scalar operands in `multiply`/`divide` no longer
  raise a `to_nx/2` clause error (OpenCV only treats a 1x1 operand as a scalar
  when it is `f64`).
- Building from source no longer re-runs the OpenCV install on every
  `mix compile`; the cmake-config gate now matches OpenCV 5.0's install path.
- 32-bit ARMv8 Nerves targets (rpi3/rpi3a/rpi0_2, Cortex-A53 built as armv7hf)
  compile again. OpenCV 5.0.0's `v_floor` NEON fast path uses the AArch64-only
  `vcvtmq_s32_f32` intrinsic under `#if __ARM_ARCH > 7`, which GCC's AArch32
  `arm_neon.h` does not provide; the fast path is now gated on `__aarch64__` so
  AArch32 keeps the portable floor fallback.
- Building without contrib modules compiles again against OpenCV 5.0.0. The
  hand-written `cv::stereo::MatchQuasiDense` vector converter was gated on
  `HAVE_OPENCV_STEREO`, but in OpenCV 5.0 `stereo` is a new main module (so that
  macro is always defined) while the quasi-dense stereo types moved to the
  contrib `xstereo` module. The converter is now gated on `HAVE_OPENCV_XSTEREO`,
  so a build without contrib modules no longer references the absent
  `cv::stereo` namespace.
- Nerves ARM targets build again with OpenCV 5.0.0. Nerves cross toolchains set
  the C/C++ compiler but not the ASM compiler, so cmake's `enable_language(ASM)`
  falls back to the host x86 assembler and hands it OpenCV's bundled MLAS AArch64
  `.S` GEMM kernels, which it cannot assemble (`no such instruction: 'fmla
  v5.4s...'`). This broke both the 32-bit boards (which report an arm64 processor
  name) and the 64-bit rpi5. MLAS is now skipped when it needs ASM and the build
  is cross-compiling, and the DNN module falls back to its built-in SGEMM.
- Building on FreeBSD compiles again with OpenCV 5.0.0. Intel IPP ICV's
  vendored safestring header declares a 3-argument `memset_s` that conflicts
  with FreeBSD libc's C11 4-argument `memset_s`, so the optional IPP
  accelerator is now disabled on FreeBSD.

### Performance

- `Evision.Backend` elementwise loops are parallelised with `cv::parallel_for_`,
  with stripe counts sized to the thread pool rather than the range length (a
  naive port dispatched one block per element and ran some ops slower in
  parallel than serially). Read-only NIF inputs are marked `INPUT_ONLY` so a
  cv-owned source `Mat` is shared instead of deep-copied; a no-op reshape of a
  2 MB tensor drops from ~110us to ~3us.
- Reductions read their input in its native dtype and promote per element to the
  wide accumulator, dropping a separate cast pass, and a new leading-axis path
  avoids transposing first: `reduce_max` over axis 0 of a 512x1024 tensor drops
  from ~3.2ms to ~0.25ms.
- Conv gains an im2row + GEMM fast path for the common 2-D case (single batch
  group, no input dilation, `f32`/`f64`), reaching parity with the libtorch
  backend; other shapes fall back to the general N-d kernel.
- Scalar-operand elementwise ops (`add`/`subtract`/`multiply`/`divide`/`min`/
  `max`/comparisons/`pow`/`atan2`/`quotient`/`remainder`/shifts) take a fast
  path that casts the scalar to a single element instead of materialising a full
  broadcast array: scalar `add` on a 2048x2048 tensor drops from ~20ms to ~1.6ms.

Change logs for `v0.2.x` are in
[CHANGELOG.v0.2.md](https://github.com/cocoa-xu/evision/blob/main/CHANGELOG.v0.2.md);
`v0.1.x` is in
[CHANGELOG.v0.1.md](https://github.com/cocoa-xu/evision/blob/main/CHANGELOG.v0.1.md).
