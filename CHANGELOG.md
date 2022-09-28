# Changelog

## v0.1.6-dev
- `Evision.imencode/{2,3}` will now return encoded image as binary instead of a list.

## v0.1.5 (2022-09-27)
- Fixed `Evision.Mat.transpose`: should call `shape!` instead of `shape`. Thanks to @kipcole9 ! #77
- Always use `Evision.Mat.from_binary_by_shape/3` for `Evision.Nx.to_mat`.
- Check `cv::Mat::Mat.type()` when fetching the shape of a Mat.
  The number of channels will be included as the last dim of the shape if and only if `cv::Mat::Mat.type()` did not encode any channel information.
- Added `Evision.Mat.last_dim_as_channel/1`.

  This method convert a tensor-like `Mat` to a "valid 2D image" with its `channels` equals to `3` or `1`.

- Added `Evision.Nx.to_mat/2`.

  This method convert a `Nx.Tensor` to a `Mat`. The second argument indicates the wanted/actual shape of the tensor.

- Added more Mat functions:
  - `Evision.Mat.as_shape/2`.
  - `Evision.Mat.size/1`.
  - `Evision.Mat.channels/1`.
  - `Evision.Mat.depth/1`.
  - `Evision.Mat.raw_type/1`.
  - `Evision.Mat.isSubmatrix/1`.
  - `Evision.Mat.isContinuous/1`.
  - `Evision.Mat.elemSize/1`.
  - `Evision.Mat.elemSize1/1`.
  - `Evision.Mat.total/{1,2,3}`.

- Added OpenCV types:
  - `Evision.cv_cn_shift/0`.
  - `Evision.cv_depth_max/0`.
  - `Evision.cv_mat_depth_mask/0`.
  - `Evision.cv_maketype/2`.
  - `Evision.cv_8U/0`.
  - `Evision.cv_8UC/1`.
  - `Evision.cv_8UC1/0`.
  - `Evision.cv_8UC2/0`.
  - `Evision.cv_8UC3/0`.
  - `Evision.cv_8UC4/0`.
  - `Evision.cv_8S/0`.
  - `Evision.cv_8SC/1`.
  - `Evision.cv_8SC1/0`.
  - `Evision.cv_8SC2/0`.
  - `Evision.cv_8SC3/0`.
  - `Evision.cv_8SC4/0`.
  - `Evision.cv_16U/0`.
  - `Evision.cv_16UC/1`.
  - `Evision.cv_16UC1/0`.
  - `Evision.cv_16UC2/0`.
  - `Evision.cv_16UC3/0`.
  - `Evision.cv_16UC4/0`.
  - `Evision.cv_16S/0`.
  - `Evision.cv_16SC/1`.
  - `Evision.cv_16SC1/0`.
  - `Evision.cv_16SC2/0`.
  - `Evision.cv_16SC3/0`.
  - `Evision.cv_16SC4/0`.
  - `Evision.cv_32S/0`.
  - `Evision.cv_32SC/1`.
  - `Evision.cv_32SC1/0`.
  - `Evision.cv_32SC2/0`.
  - `Evision.cv_32SC3/0`.
  - `Evision.cv_32SC4/0`.
  - `Evision.cv_32F/0`.
  - `Evision.cv_32FC/1`.
  - `Evision.cv_32FC1/0`.
  - `Evision.cv_32FC2/0`.
  - `Evision.cv_32FC3/0`.
  - `Evision.cv_32FC4/0`.
  - `Evision.cv_64F/0`.
  - `Evision.cv_64FC/1`.
  - `Evision.cv_64FC1/0`.
  - `Evision.cv_64FC2/0`.
  - `Evision.cv_64FC3/0`.
  - `Evision.cv_64FC4/0`.
  - `Evision.cv_16F/0`.
  - `Evision.cv_16FC/1`.
  - `Evision.cv_16FC1/0`.
  - `Evision.cv_16FC2/0`.
  - `Evision.cv_16FC3/0`.
  - `Evision.cv_16FC4/0`.

## v0.1.4 (2022-09-10)
- Default to `Evision.Backend` for `Evision.Nx.to_nx/2`.
- Fixed class inheritance issue in `py_src/class_info.py`.
- Fixed missing comma in example livebooks' `Mix.install`. @dbii
- Added decision tree and random forest example.

## v0.1.3 (2022-09-01)
- Fixed issues in restoring files from precompiled package for macOS and Linux.
  - Paths are now quoted. 
  - using `cp -RPf` on macOS while `cp -a` on Linux.
- Fixed `destroyAllWindows` in NIF.
  It was generated as 'erlang:destroyAllWindows/1' but it should be 'erlang:destroyAllWindows/0'.

## v0.1.2 (2022-08-26)
- Added x86_64 musl compilation CI test.
- Fixed transpose.
- Added a few precompilation musl targets:
  - x86_64-linux-musl
  - aarch64-linux-musl
  - armv7l-linux-musleabihf
  - riscv64-linux-musl

## v0.1.1 (2022-08-25)
- Use OpenCV 4.6.0 by default.
- Implemented a few nx callbacks (remaining ones will be implemented in the next release).
- Deprecated the use of the `EVISION_PRECOMPILED_VERSION` environment variable. The version information will be implied by the tag:

    ```elixir
    def deps do
    [
        {:evision, "~> 0.1.1", github: "cocoa-xu/evision", tag: "v0.1.1"}
    ]
    end
    ```
  
  The value of the environment variable `EVISION_PREFER_PRECOMPILED` decides whether the precompiled artefacts will be used or not.

  ~~From the next version (>=0.1.2), `evision` will set `EVISION_PREFER_PRECOMPILED` to `true` by default.~~

## v0.1.0 (2022-07-23)
First release.
