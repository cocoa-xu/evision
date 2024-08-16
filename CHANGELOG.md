# Changelog

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
