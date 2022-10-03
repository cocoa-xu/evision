# Changelog

## v0.1.7-dev (main)
[Browse the Repository](https://github.com/cocoa-xu/evision)
### Changed
- `cv::VideoCapture` will be wrapped in struct. For example:

  ```elixir
  iex> cap = Evision.VideoCapture.videoCapture!("test/videocapture_test.mp4")
  %Evision.VideoCapture{
    fps: 43.2,
    frame_count: 18.0,
    frame_width: 1920.0,
    frame_height: 1080.0,
    isOpened: true,
    ref: #Reference<0.3650318819.3952214034.37793>
  }
  iex> frame = Evision.VideoCapture.read!(cap)
  %Evision.Mat{
    channels: 3,
    dims: 2,
    type: {:u, 8},
    raw_type: 16,
    shape: {1080, 1920, 3},
    ref: #Reference<0.3650318819.3952214042.38343>
  }
  ```

- `Evision.Mat.empty/0` will also return an `Evision.Mat` struct (was returning `#Reference<some random numbers>`).

  ```elixir
  iex> Evision.Mat.empty!()
  %Evision.Mat{
    channels: 1,
    dims: 0,
    type: {:u, 8},
    raw_type: 0,
    shape: {},
    ref: #Reference<0.2351084001.2568618002.207930>
  }
  ```

### Added
- Added `Evision.Mat.literal/{1,2,3}` to create `Evision.Mat` from list literals.

  Creating `Evision.Mat` from empty list literal (`[]`) is the same as calling `Evision.Mat.empty()`.

  ```elixir
  iex> Evision.Mat.literal!([])
  %Evision.Mat{
    channels: 1,
    dims: 0,
    type: {:u, 8},
    raw_type: 0,
    shape: {},
    ref: #Reference<0.1204050731.2031747092.46781>
  }
  ```

  By default, the shape of the Mat will stay as is.
  ```elixir
  iex> Evision.Mat.literal!([[[1,1,1],[2,2,2],[3,3,3]]], :u8)
  %Evision.Mat{
    channels: 1,
    dims: 3,
    type: {:u, 8},
    raw_type: 0,
    shape: {1, 3, 3},
    ref: #Reference<0.512519210.691404819.106300>
  }
  ```

  `Evision.Mat.literal/3` will return a vaild 2D image if the keyword argment, `as_2d`, is set to `true` and if the list literal can be represented as a 2D image.
  ```elixir
  iex> Evision.Mat.literal!([[[1,1,1],[2,2,2],[3,3,3]]], :u8, as_2d: true)
  %Evision.Mat{
    channels: 3,
    dims: 2,
    type: {:u, 8},
    raw_type: 16,
    shape: {1, 3, 3},
    ref: #Reference<0.512519210.691404820.106293>
  }
  ```

## v0.1.6 (2022-09-29)
[Browse the Repository](https://github.com/cocoa-xu/evision/tree/v0.1.7) | [Released Assets](https://github.com/cocoa-xu/evision/releases/tag/v0.1.6)
### Breaking Changes
- `Evision.imencode/{2,3}` will now return encoded image as binary instead of a list.

- `cv::Mat` will be wrapped in struct. For example:

  ```elixir
  iex> Evision.imread!("path/to/image.png")
  %Evision.Mat{
    channels: 3,
    dims: 2,
    type: {:u, 8},
    raw_type: 16,
    shape: {512, 512, 3},
    ref: #Reference<0.2992585850.4173463580.172624>
  }
  ```

  This should close #76.

## v0.1.5 (2022-09-27)
[Browse the Repository](https://github.com/cocoa-xu/evision/tree/v0.1.5) | [Released Assets](https://github.com/cocoa-xu/evision/releases/tag/v0.1.5)
### Changed
- Always use `Evision.Mat.from_binary_by_shape/3` for `Evision.Nx.to_mat`.
- Check `cv::Mat::Mat.type()` when fetching the shape of a Mat.
  The number of channels will be included as the last dim of the shape if and only if `cv::Mat::Mat.type()` did not encode any channel information.

### Fixed
- Fixed `Evision.Mat.transpose`: should call `shape!` instead of `shape`. Thanks to @kipcole9 ! #77

### Added
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
[Browse the Repository](https://github.com/cocoa-xu/evision/tree/v0.1.4) | [Released Assets](https://github.com/cocoa-xu/evision/releases/tag/v0.1.4)
### Changed
- Default to `Evision.Backend` for `Evision.Nx.to_nx/2`.

### Fixed
- Fixed class inheritance issue in `py_src/class_info.py`.
- Fixed missing comma in example livebooks' `Mix.install`. Thanks to @dbii.

### Added
- Added decision tree and random forest example.

## v0.1.3 (2022-09-01)
[Browse the Repository](https://github.com/cocoa-xu/evision/tree/v0.1.3) | [Released Assets](https://github.com/cocoa-xu/evision/releases/tag/v0.1.3)
### Fixed
- Fixed issues in restoring files from precompiled package for macOS and Linux.
  - Paths are now quoted. 
  - using `cp -RPf` on macOS while `cp -a` on Linux.
- Fixed `destroyAllWindows` in NIF.
  It was generated as 'erlang:destroyAllWindows/1' but it should be 'erlang:destroyAllWindows/0'.

## v0.1.2 (2022-08-26)
[Browse the Repository](https://github.com/cocoa-xu/evision/tree/v0.1.2) | [Released Assets](https://github.com/cocoa-xu/evision/releases/tag/v0.1.2)
### Fixed
- Fixed transpose.

### Added
- Added x86_64 musl compilation CI test.
- Added a few precompilation musl targets:
  - x86_64-linux-musl
  - aarch64-linux-musl
  - armv7l-linux-musleabihf
  - riscv64-linux-musl

## v0.1.1 (2022-08-25)
[Browse the Repository](https://github.com/cocoa-xu/evision/tree/v0.1.1) | [Released Assets](https://github.com/cocoa-xu/evision/releases/tag/v0.1.1)
### Changed
- Use OpenCV 4.6.0 by default.
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

### Added
- Implemented a few nx callbacks (remaining ones will be implemented in the next release).


## v0.1.0 (2022-07-23)
First release.
