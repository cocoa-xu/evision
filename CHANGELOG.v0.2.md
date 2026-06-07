# Changelog

## v0.2.17 (2026-05-27)
[Browse the Repository](https://github.com/cocoa-xu/evision/tree/v0.2.17) | [Released Assets](https://github.com/cocoa-xu/evision/releases/tag/v0.2.17)

### Fixed

- `Evision.Mat.clip/3`: upper bound no longer collapses to the lower bound (typo where both branches compared against `lower`).
- `Evision.Mat.dot/2`: now actually computes a dot product. The previous implementation called `cv::Mat::cross` despite being named `dot`, so it only worked for 3-element vectors and returned a `Mat`. See **Breaking changes** below.
- `Evision.Mat.arange/4,5`: range count is now computed with ceiling division, so the final stepped value is included when the interval is not evenly divisible (e.g. `arange(0, 5, 2)` now returns `[0, 2, 4]` instead of `[0, 2]`). `step == 0` and direction mismatches are rejected explicitly.
- `Evision.Mat.negate/1`, `Evision.Mat.ceil/1`, `Evision.Mat.floor/1`, `Evision.Mat.round/1`: now traverse every scalar in multi-channel matrices instead of only the first channel of each pixel.
- `Evision.Mat.ceil/1`, `Evision.Mat.floor/1`, `Evision.Mat.round/1`: added support for `CV_16F` (half-precision float) inputs.
- `Evision.Mat.at/2`: added bounds checking (was previously undefined behaviour past the end) and a clone-on-non-continuous fallback so the scalar index lands on the right element. `CV_16F` is now supported.
- `Evision.Mat.from_binary_by_shape/3`: rejects binaries whose byte size does not match the declared shape (previously created a `Mat` that read past the binary).
- `Evision.Mat.from_binary/5`: the declared `rows × cols × channels × bytes_per_element` is now computed with overflow checks.
- `Evision.Mat.to_binary/1,2`:
  - The `limit` parameter is now honored on the NIF path (was previously ignored).
  - Non-continuous matrices are cloned before serialisation so the returned binary contains contiguous element data rather than the underlying buffer's interleaved padding.
  - The `nx_tensor` branch in Elixir now interprets `limit` as the number of elements (multiplying by `elem_size`), matching the NIF path and the `Backend.inspect/2` caller.
- `Evision.Mat.to_batched/3` (3-arg): now works on multi-channel matrices (previously the `shape(mat)` it derived internally always failed the size check). The 3-arg wrapper also no longer passes a tuple where a list is expected.
- `Evision.Mat.to_batched/4` repeat mode: no longer reads past the source matrix when `batch_size × slice_size` exceeds the source size; the source is now safely repeated as many times as needed.
- `Evision.Mat.update_roi/3`: the caller's `Mat` data is no longer mutated in place — `update_roi` always returns a new `Mat` and leaves the input untouched. Previously the underlying buffer was modified through the shared reference, silently mutating any other reference to the same `Mat`.
- `Evision.Mat.transpose/3` with `as_shape:`: rejects `as_shape` whose total byte size does not match the source matrix, preventing out-of-bounds reads from `cv::transposeND`.
- `Evision.Mat.expm1/1`: removed a dead/buggy fallback that called `cv::exp` a second time without exception protection after the wrapped call had already failed.
- `Evision.Mat.last_dim_as_channel/1`: now uses `src.depth()` (instead of the full type) when re-tagging the channels and explicitly rejects channel counts outside `[1, CV_CN_MAX]`.
- `Evision.CUDA.GpuMat.from_pointer/3`: the resource is now released on the error path (previously leaked) and the OOM path returns a proper error term.
- `evision::nif::get_tuple` for `std::vector<int64_t>`: now reads each element as `int64_t` (was `int`), so large 64-bit values in tuples are no longer truncated/rejected.
- `evision_highgui` thread initialisation: races between concurrent `start_native_gui` callers are now serialised behind an `EVISION_INITIATING` state, and `evision_main_loop` notifies all waiters (was `notify_one`, which could leave additional waiters stuck).
- `alloc_resource` for `cv::Mat *`: explicitly initialises `tmp->val = nullptr` so a release before assignment does not free uninitialised memory.

### Added

- `Evision.Mat.matmul/2`: convenience wrapper around `cv::Mat::operator*` (matrix multiplication via `cv::gemm`). Both inputs must be 2-D and share a `CV_32F` or `CV_64F` depth. For element-wise dot-product semantics use `Evision.Mat.dot/2`.

### Breaking changes

- `Evision.Mat.dot/2` now returns a `number()` (the scalar dot product) instead of a `%Evision.Mat{}`. This matches the C++ `cv::Mat::dot` definition. The previous behaviour was a bug: the function was implemented with `cv::Mat::cross`, which only accepted 3-element vectors and returned a `Mat`. Callers who actually wanted matrix multiplication should switch to the new `Evision.Mat.matmul/2`.
- `Evision.Mat.to_batched/4` (4-arg, explicit `as_shape`) now requires `as_shape` to include channels as a dimension for multi-channel matrices, and the resulting batches are single-channel `Mat`s with the channel axis at the end. Concretely:
  - Old: `to_batched(cv_8uc3_mat, batch_size, {N, H, W}, ...)` → batches of `CV_8UC3` mats with shape `{batch_size, H, W}`.
  - New: `to_batched(cv_8uc3_mat, batch_size, {N, H, W, 3}, ...)` → batches of `CV_8U` mats with shape `{batch_size, H, W, 3}`.
  - The old `to_batched(cv_8uc3_mat, batch_size, {N, H, W}, ...)` call now returns `{:error, "to_batched failed: cannot treated matrix as the request shape"}`.
  - `Evision.Backend` (Nx) workflows are **not affected**: tensors entering through `from_nx`/`from_binary` are always single-channel `Mat`s with the logical Nx shape, so the old and new size-checks evaluate identically.

## v0.2.14 (2025-08-15)
[Browse the Repository](https://github.com/cocoa-xu/evision/tree/v0.2.14) | [Released Assets](https://github.com/cocoa-xu/evision/releases/tag/v0.2.14)

### Changes

- Added support for newer generation of CUDA architectures, including `sm_100` and `sm_120`.

## v0.2.13 (2025-05-15)
[Browse the Repository](https://github.com/cocoa-xu/evision/tree/v0.2.13) | [Released Assets](https://github.com/cocoa-xu/evision/releases/tag/v0.2.13)

### Fixed

- Allowing Features2D parameters to be `nil` when they have default values or as output parameters.

## v0.2.12 (2025-04-14)
[Browse the Repository](https://github.com/cocoa-xu/evision/tree/v0.2.12) | [Released Assets](https://github.com/cocoa-xu/evision/releases/tag/v0.2.12)

### Added

- Precompiled binaries for `x86_64-unknown-freebsd`.

## v0.2.11 (2025-01-18)
[Browse the Repository](https://github.com/cocoa-xu/evision/tree/v0.2.11) | [Released Assets](https://github.com/cocoa-xu/evision/releases/tag/v0.2.11)

### Changes

- Bump OpenCV to v4.11.0. Some APIs may have changed, please see OpenCV's release note for more information.
- Removed `aarch64-windows-msvc` and `ppc64le-linux-gnu` target from the precompiled binaries.

## v0.2.10 (2024-09-22)
[Browse the Repository](https://github.com/cocoa-xu/evision/tree/v0.2.10) | [Released Assets](https://github.com/cocoa-xu/evision/releases/tag/v0.2.10)

### Changes

- Bump `req` to `0.5` and updated examples accordingly. Thanks @mnishiguchi for PR [#266](https://github.com/cocoa-xu/evision/pull/266)

## v0.2.9 (2024-08-17)
[Browse the Repository](https://github.com/cocoa-xu/evision/tree/v0.2.9) | [Released Assets](https://github.com/cocoa-xu/evision/releases/tag/v0.2.9)

### Fixes

- Include the `c_src/evision_consts.h` in the package.

## v0.2.8 (2024-08-04)
[Browse the Repository](https://github.com/cocoa-xu/evision/tree/v0.2.8) | [Released Assets](https://github.com/cocoa-xu/evision/releases/tag/v0.2.8)

### Fixes

- Fixed inheritance issues for classes that have any of the following base classes:
  - `cv::img_hash::ImgHashBase`
  - `cv::legacy::Tracker`
  - `cv::phase_unwrapping::PhaseUnwrapping`
  - `cv::rapid::Tracker`
  - `cv::reg::Map`
  - `cv::reg::Mapper`
  - `cv::structured_light::StructuredLightPattern`
  - `BackgroundSubtractor`

## v0.2.7 (2024-07-03)
[Browse the Repository](https://github.com/cocoa-xu/evision/tree/v0.2.7) | [Released Assets](https://github.com/cocoa-xu/evision/releases/tag/v0.2.7)

### Breaking changes (compile-time and CUDA users only)
- `EVISION_CUDA_VERSION`, now it should be the major version of CUDA, e.g., `12`.
- `EVISION_CUDNN_VERSION`, it should be the major version of cuDNN, e.g., `9`.

### Changes
- Added precompiled version with CUDA 12.x and cudnn 9.x for aarch64-linux-gnu.

## v0.2.5 (2024-07-01)
[Browse the Repository](https://github.com/cocoa-xu/evision/tree/v0.2.5) | [Released Assets](https://github.com/cocoa-xu/evision/releases/tag/v0.2.5)

### Changes
- Validate possible keyword arguments for functions that accept named arguments.

    ```elixir
    iex> img = Evision.imread("test/testdata/dog.jpg")

    # valid keyword argument
    iex> Evision.applyColorMap(src: img, colormap: Evision.Constant.cv_COLORMAP_AUTUMN)
    %Evision.Mat{
      channels: 3,
      dims: 2,
      type: {:u, 8},
      raw_type: 16,
      shape: {576, 768, 3},
      ref: #Reference<0.97833242.1116078104.245977>
    }
    
    # list all possible keyword arguments if the user provides any invalid ones
    iex> Evision.applyColorMap(src: img, colorMap: Evision.Constant.cv_COLORMAP_AUTUMN)
    ** (ArgumentError) unknown keys [:colorMap] in [src: %Evision.Mat{channels: 3, dims: 2, type: {:u, 8}, raw_type: 16, shape: {576, 768, 3}, ref: #Reference<0.97833242.1116078110.246705>}, colorMap: 0], the allowed keys are: [:dst, :colormap, :userColor, :src]
        (elixir 1.18.0-dev) lib/keyword.ex:362: Keyword.validate!/2
        (evision 0.2.4) lib/generated/evision.ex:4603: Evision.applyColorMap/1
        iex:4: (file)
    ```

- Generated typed enums for OpenCV's `cv::flann`.
- 
  This should include the following enums and place them in the corresponding modules.

  - `flann_algorithm_t`
  - `flann_centers_init_t`
  - `flann_log_level_t`
  - `flann_distance_t`
  - `flann_datatype_t`

  For example, `flann_algorithm_t` will be put in the `Evision.Flann.Algorithm` module:

  ```elixir
  defmodule Evision.Flann.Algorithm do
    @type enum :: integer()
    @doc enum: true
    def cv_FLANN_INDEX_LINEAR, do: 0
    @doc enum: true
    def cv_FLANN_INDEX_KDTREE, do: 1
    @doc enum: true
    def cv_FLANN_INDEX_KMEANS, do: 2
    @doc enum: true
    def cv_FLANN_INDEX_COMPOSITE, do: 3
    @doc enum: true
    def cv_FLANN_INDEX_KDTREE_SINGLE, do: 4
    @doc enum: true
    def cv_FLANN_INDEX_HIERARCHICAL, do: 5
    @doc enum: true
    def cv_FLANN_INDEX_LSH, do: 6
    @doc enum: true
    def cv_FLANN_INDEX_SAVED, do: 254
    @doc enum: true
    def cv_FLANN_INDEX_AUTOTUNED, do: 255
  end
  ```

## v0.2.4 (2024-06-20)
[Browse the Repository](https://github.com/cocoa-xu/evision/tree/v0.2.4) | [Released Assets](https://github.com/cocoa-xu/evision/releases/tag/v0.2.4)

### Changes
- Allow users to use named arguments.

## v0.2.3 (2024-06-17)
[Browse the Repository](https://github.com/cocoa-xu/evision/tree/v0.2.3) | [Released Assets](https://github.com/cocoa-xu/evision/releases/tag/v0.2.3)

### Added
- Experimental support for use shared CUDA memory via `Evision.CUDA.GpuMat.from_pointer/{3,4}`.

## v0.2.2 (2024-06-15)
[Browse the Repository](https://github.com/cocoa-xu/evision/tree/v0.2.2) | [Released Assets](https://github.com/cocoa-xu/evision/releases/tag/v0.2.2)

### Added
- Experimental support for CUDA shared memory via `Evision.CUDA.GpuMat.to_pointer/{1,2}`.
- Experimental support for Gleam. See [gleam_evision_demo](https://github.com/cocoa-xu/gleam_evision_demo) for more information.

### Changes
- General improvements to the typespecs. Enumerators now also have their own modules.
- Experimental support for using Nx.tensor directly without calling helper functions like `Evision.Mat.from_nx/1`, `Evision.Mat.from_nx_2d/1` and `Evision.Mat.last_dim_as_channel/1` .
- Support `/AOS` (arithmetic op src) marks in OpenCV source code.

  This should make evision's behaviour in line with OpenCV (C++ and Python).

  For example, in `Evision.add/2`, the expected behaviour should be different when src1/src2 are single number and they are tuple/array.

  `add(src, X)` where `X` is a number (or `Nx.tensor(X)`), it means `add(src, {X,X,X,X})`

  while `add(src, {X})` (or `add(src, Nx.tensor([X]))`) means `add(src, {X,0,0,0})`.
- Allow a single number to be passed as `Evision.Mat.maybe_mat_in()` and `Evision.scalar()`.
- Allow a n-tuple (containing n numbers) to be passed as `Evision.Mat.maybe_mat_in()` and `Evision.scalar()`.
- `Evision.Mat` now implements `Nx.LazyContainer` protocol.

### Fixes
- Fixed Erlang bindings when converting from Elixir structs to Erlang records and vice versa.

## v0.2.1 (2024-06-04)
[Browse the Repository](https://github.com/cocoa-xu/evision/tree/v0.2.1) | [Released Assets](https://github.com/cocoa-xu/evision/releases/tag/v0.2.1)

### Fixes
- Fixed function specs where type hints for sub-classes were using the type of their parent-classes.

## v0.2.0 (2024-06-03)
[Browse the Repository](https://github.com/cocoa-xu/evision/tree/v0.2.0) | [Released Assets](https://github.com/cocoa-xu/evision/releases/tag/v0.2.0)

### Changes
- Uses OpenCV 4.10.0. Some APIs may have changed, please see OpenCV's release note for more information.

Change logs for `v0.1.x` can be found in the [CHANGELOG.v0.1.md](https://github.com/cocoa-xu/evision/blob/main/CHANGELOG.v0.1.md)
