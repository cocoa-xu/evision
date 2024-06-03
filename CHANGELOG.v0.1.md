# Changelog

## v0.1.39 (2024-06-01)
[Browse the Repository](https://github.com/cocoa-xu/evision/tree/v0.1.39) | [Released Assets](https://github.com/cocoa-xu/evision/releases/tag/v0.1.39)

### Changes
- Validate allowed keyword arguments.
- List all allowed keyword arguments in the function specs.

## v0.1.38 (2024-03-30)
[Browse the Repository](https://github.com/cocoa-xu/evision/tree/v0.1.38) | [Released Assets](https://github.com/cocoa-xu/evision/releases/tag/v0.1.38)

### Fix
- [videocapture] fixed an issue where invoking `Evision.VideoCapture.waitAny/{1,2}` reports a NIF error `"cv::VideoCapture::waitAny not loaded"`.

## v0.1.37 (2024-02-17)
[Browse the Repository](https://github.com/cocoa-xu/evision/tree/v0.1.37) | [Released Assets](https://github.com/cocoa-xu/evision/releases/tag/v0.1.37)

### Changes
- [nerves-build] use fwup v1.10.1
- [precompiled] support `armv6-linux-gnueabihf` target
- [precompiled] precompiled binaries are now built with Erlang/OTP NIF version 2.16, and they are compatible with NIF version 2.16 and later.
- [model_zoo] use permanent URLs for all models.

### Added
- [experimental] support `aarch64-windows-msvc` target
- [nerves-build] added `rpi5` and `srhub`
- [model_zoo] added PP-OCR V3 text detection models
- [model_zoo] added image classification mobilenet v2 models

## v0.1.36 (2024-02-16)
[Browse the Repository](https://github.com/cocoa-xu/evision/tree/v0.1.36) | [Released Assets](https://github.com/cocoa-xu/evision/releases/tag/v0.1.36)

### Fix
- Allow implicitly cast to `Evision.Feature2D` from the following types:
  - `Evision.AKAZE`
  - `Evision.AffineFeature`
  - `Evision.AgastFeatureDetector`
  - `Evision.BRISK`
  - `Evision.FastFeatureDetector`
  - `Evision.GFTTDetector`
  - `Evision.KAZE`
  - `Evision.MSER`
  - `Evision.ORB`
  - `Evision.SimpleBlobDetector`
  - `Evision.XFeatures2D.BEBLID`
  - `Evision.XFeatures2D.BoostDesc`
  - `Evision.XFeatures2D.BriefDescriptorExtractor`
  - `Evision.XFeatures2D.DAISY`
  - `Evision.XFeatures2D.FREAK`
  - `Evision.XFeatures2D.LATCH`
  - `Evision.XFeatures2D.LUCID`
  - `Evision.XFeatures2D.MSDDetector`
  - `Evision.XFeatures2D.StarDetector`
  - `Evision.XFeatures2D.TBMR`
  - `Evision.XFeatures2D.TEBLID`
  - `Evision.XFeatures2D.VGG`
  
### Added
- [experimental] Support compiling for iOS. Precompiled binaries are also available for iOS, but they are not tested yet, and they require a few more steps to use. Please see [this guide](https://github.com/cocoa-xu/evision/pull/79) for more information.

## v0.1.35 (2024-02-14)
[Browse the Repository](https://github.com/cocoa-xu/evision/tree/v0.1.35) | [Released Assets](https://github.com/cocoa-xu/evision/releases/tag/v0.1.35)

### Changed
- Detect and use env var `HTTP_PROXY` and `HTTPS_PROXY` when downloading precompiled binaries.
- Updated to OpenCV 4.9.0. Some APIs may have changed, please see OpenCV's release note for more information.
- Use embedded `:evision_windows_fix` instead of `:dll_loader_helper`.
- Updated CUDA versions for precompiled binaries. Now we only have `EVISION_CUDA_VERSION=118` and `EVISION_CUDA_VERSION=121`.

## v0.1.34 (2023-11-18)
[Browse the Repository](https://github.com/cocoa-xu/evision/tree/v0.1.34) | [Released Assets](https://github.com/cocoa-xu/evision/releases/tag/v0.1.34)

### Changed
- [compatibilities] Make it compatible with newer versions of Kino. Thanks for the contribution from @jonatanklosko.

## v0.1.32 (2023-08-01)
[Browse the Repository](https://github.com/cocoa-xu/evision/tree/v0.1.32) | [Released Assets](https://github.com/cocoa-xu/evision/releases/tag/v0.1.32)

### Changed
- Updated to OpenCV 4.8.0. Some APIs have changed, please see OpenCV's release note for more information.
- Using manylinux2014 to build precompiled binaries for x86_64-linux-gnu (w/ and w/o contrib, except for the cuda ones). This allows us to only require glibc version to be at least 2.17.

## v0.1.31 (2023-04-17)
[Browse the Repository](https://github.com/cocoa-xu/evision/tree/v0.1.31) | [Released Assets](https://github.com/cocoa-xu/evision/releases/tag/v0.1.31)

### Changed
- [Evision.DNN] Added support for passing `Nx.Tensor` or `Evision.Mat` as the input argument of `bboxes` in 
  - `Evision.DNN.nmsBoxes/{4,5}`

    ```elixir
    iex> Evision.DNN.nmsBoxes([{0,1,2,3}], [1], 0.4, 0.3)
    [0]
    iex> Evision.DNN.nmsBoxes(Nx.tensor([[0,1,2,3]]), [1], 0.4, 0.3)
    [0]
    iex> Evision.DNN.nmsBoxes(Evision.Mat.literal([[0,1,2,3]], :f64), [1], 0.4, 0.3)
    [0]
    ```

  - `Evision.DNN.nmsBoxesBatched/{5,6}`

    ```elixir
    iex> Evision.DNN.nmsBoxesBatched([{0,1,2,3}], [1], [1], 0.4, 0.3)
    [0]
    iex> Evision.DNN.nmsBoxesBatched(Nx.tensor([[0,1,2,3]]), [1], [1], 0.4, 0.3)
    [0]
    iex> Evision.DNN.nmsBoxesBatched(Evision.Mat.literal([[0,1,2,3]], :f64), [1], [1], 0.4, 0.3)
    [0]
    ```

  - `Evision.DNN.softNMSBoxes/{4,5}`

    ```elixir
    iex> Evision.DNN.softNMSBoxes([{0,1,2,3}], [1], 0.4, 0.3)
    {[1.0], [0]}
    iex> Evision.DNN.softNMSBoxes(Nx.tensor([[0,1,2,3]]), [1], 0.4, 0.3)
    {[1.0], [0]}
    iex> Evision.DNN.softNMSBoxes(Evision.Mat.literal([[0,1,2,3]], :s32), [1], 0.4, 0.3)
    {[1.0], [0]}
    ```

## v0.1.30 (2023-03-24)
[Browse the Repository](https://github.com/cocoa-xu/evision/tree/v0.1.30) | [Released Assets](https://github.com/cocoa-xu/evision/releases/tag/v0.1.30)

### Fixed
- [smartcell] Fixed outputBlob is embedded in a list for CRNN and MobileNetV1 models.

## v0.1.29 (2023-03-13)
[Browse the Repository](https://github.com/cocoa-xu/evision/tree/v0.1.29) | [Released Assets](https://github.com/cocoa-xu/evision/releases/tag/v0.1.29)

### Fixed
- [py_src] Fixed incorrect typespec for `Saclar`. Thanks @tusqasi and @Nicd.
- [smartcell] Fixed PPResnet based models.
- [smartcell] Fixed invalid charset URLs as they were removed in the upstream repo. CRNN models URLs to commit 12817b80.

## v0.1.28 (2023-01-25)
[Browse the Repository](https://github.com/cocoa-xu/evision/tree/v0.1.28) | [Released Assets](https://github.com/cocoa-xu/evision/releases/tag/v0.1.28)

### Fixed
- [py_src] `ArgInfo.has_default` is now set to `true` if `a.defval` is `f"{a.tp}()"`. Fixed [#174](https://github.com/cocoa-xu/evision/issues/174).
- [ci-win-precompile-core] Removed the line that deletes the `priv/x64` directory. It should be removed in the last version because we will add `priv/x64` to the DLL search path instead of copying all opencv dll files to the `priv` directory.

## v0.1.27 (2023-01-23)
[Browse the Repository](https://github.com/cocoa-xu/evision/tree/v0.1.27) | [Released Assets](https://github.com/cocoa-xu/evision/releases/tag/v0.1.27)

### Breaking Changes
- [Evision.Constant] Constant values are all relocated to the `Evision.Constant` module. To use them, either do

  ```elixir
  Evision.Constant.cv_IMREAD_ANY()
  ```

  or

  ```elixir
  import Evision.Constant
  cv_IMREAD_ANY()
  ```

- [Evision] `Evision.__enabled_modules__/0` => `Evision.enabled_modules/0`. Result will now be computed using `HAVE_OPENCV_{MODULE_NAME}` macros.

### Fixed
- [Evision.Mat] fixed `Evision.Mat.update_roi/3`.
- [py_src] fix incorrect typespecs.
- [py_src] `VideoCaptureAPIs` should be a single number instead of a list of number.

### Changed
- [c_src] check if we can use existing atom from `enif_make_existing_atom` before calling to `enif_make_atom`.

### Added
- OpenCV contrib modules.

  - For users who prefer using precompiled binaries, almost all modules in opencv_contrib is included in precompiled archives (except for CUDA related ones, please see the next bullet point), and this will be the new default for precompiled users. 
   
    However, we do provide precompiled binaries that only include core modules. Please see the next paragraph.

  - For all users, to use core modules only, please set env var `EVISION_ENABLE_CONTRIB=false`.

- CUDA 11 + cudnn 8 support with precompiled binaries.

  Note that CUDA 11 and cudnn 8 runtime libraries/dll files are not included in the precompiled archive. 

  Please following the installation guide on NVIDIA's website.

  - For precompiled binaries users, please set `EVISION_ENABLE_CUDA` to `true`. Besides that, there are 3 CUDA versions to choose:

    - `EVISION_CUDA_VERSION=111`
    - `EVISION_CUDA_VERSION=114`
    - `EVISION_CUDA_VERSION=118`

    Please choose the one that matches your local CUDA versions, and set this `EVISION_CUDA_VERSION` env var correspondingly.

  - For users who prefer compiling from source, you'll only need to set `EVISION_ENABLE_CUDA` to `true`, and OpenCV will detect and use (if possible) your local CUDA/cudnn runtime.

  Lastly, if `EVISION_ENABLE_CUDA` is `true` while `EVISION_ENABLE_CONTRIB` is `false`, CUDA related modules will not be compiled/downloaded.

## v0.1.26 (2023-01-21)
Please use `v0.1.27` as precompilation binaries for targets `x86_64-windows-msvc-contrib-cuda*` and `x86_64-linux-gnu-contrib-cuda*` were incorrect.

## v0.1.25 (2022-12-18)
[Browse the Repository](https://github.com/cocoa-xu/evision/tree/v0.1.25) | [Released Assets](https://github.com/cocoa-xu/evision/releases/tag/v0.1.25)

### Fixed
- [smartcell] fixed a typo in SFace.

## v0.1.23/v0.1.24 (2022-12-17)
[Browse the Repository](https://github.com/cocoa-xu/evision/tree/v0.1.24) | [Released Assets](https://github.com/cocoa-xu/evision/releases/tag/v0.1.24)

### Fixed
- [smartcell] fixed charset loading when initialising FP16/INT8 CRNN models. [#144](https://github.com/cocoa-xu/evision/issues/144)
- [smartcell] fixed OpenCL target label.

### Changed
- [smartcell] register the model zoo smart cell (`Evision.SmartCell.Zoo`) on starting. Thanks to @josevalim.
- [smartcell] make `:kino` and `:progress_bar` optional dependencies.
- [ci] added one more step to make sure it compiles without optional deps. Thanks to @josevalim.
- [smartcell] hide all FP16 models of CRNN because they were not supported until [opencv/opencv #22337](https://github.com/opencv/opencv/pull/22337), which was after the release date of OpenCV 4.6.0.
  
  See more on [https://github.com/opencv/opencv/issues/18735#issuecomment-1273125970](https://github.com/opencv/opencv/issues/18735#issuecomment-1273125970).

- [smartcell] hide `CRNN CH (INT8)` and `CRNN EN (INT8)` because OpenCV 4.6.0 seemed to have problems loading/parsing them even with the `demo.py` script in the official opencv_zoo repo.

## v0.1.22 (2022-12-16)
[Browse the Repository](https://github.com/cocoa-xu/evision/tree/v0.1.22) | [Released Assets](https://github.com/cocoa-xu/evision/releases/tag/v0.1.22)

### Added
- [smartcell] OpenCV Model Zoo. `Evision.SmartCell.Zoo`

## v0.1.21 (2022-11-25)
[Browse the Repository](https://github.com/cocoa-xu/evision/tree/v0.1.21) | [Released Assets](https://github.com/cocoa-xu/evision/releases/tag/v0.1.21)

### Fixed
- [py_src] fixed functions in dnn that `return *this`.

  For this part, this original code (as in `python-opencv`) would case a new object to be allocated in C++ like

  ```cpp
  TextDetectionModel_DB retval;
  retval = self.setSomeValue(...)
  return pyopencv_from(retval);
  ```

  Noticing the address of the object has changed (because it's a new one) after calling `m.setBinaryThreshold`.
  ```python3
  >>> import cv2'
  >>> m = cv2.dnn_TextDetectionModel_DB("DB_IC15_resnet18.onnx")
  >>> m
  < cv2.dnn.TextDetectionModel_DB 0x1064cf210>
  >>> m.setBinaryThreshold(0.5)
  < cv2.dnn.TextDetectionModel_DB 0x11ecda7f0>
  ```

## v0.1.20 (2022-11-24)
[Browse the Repository](https://github.com/cocoa-xu/evision/tree/v0.1.20) | [Released Assets](https://github.com/cocoa-xu/evision/releases/tag/v0.1.20)

### Fixed
- [Precompiled] fixed incorrect checksum for `x86_64-linux-gnu`.

## v0.1.19 (2022-11-14)
[Browse the Repository](https://github.com/cocoa-xu/evision/tree/v0.1.19) | [Released Assets](https://github.com/cocoa-xu/evision/releases/tag/v0.1.19)

### Changed
- [py_src/c_src] Added `has_default` field to `ArgInfo`.

## v0.1.18 (2022-11-12)
[Browse the Repository](https://github.com/cocoa-xu/evision/tree/v0.1.18) | [Released Assets](https://github.com/cocoa-xu/evision/releases/tag/v0.1.18)

### Fixes
- [precompile] Fixed `Mix.Tasks.Compile.EvisionPrecompiled.read_checksum_map/1`
- [py_src] Fixed code generation for derived classes in namespace `cv::dnn`
- [test] added test for `Evision.DNN.DetectionModel`.

## v0.1.17 (2022-11-11)
[Browse the Repository](https://github.com/cocoa-xu/evision/tree/v0.1.17) | [Released Assets](https://github.com/cocoa-xu/evision/releases/tag/v0.1.17)

### Fixes
- [py_src] Fixed a code generation bug when all the input arguments of a function are optional.

### Changed
- [example] `Req.get!` should only raise on 4xx and 5xx. Thanks @wojtekmach

### Added
- [example] Added two examples: 

  - find and draw contours in an image.
  - extracting sudoku puzzle from an image.

- [erlang] Structurised/recordized all `#reference`s that have their own Erlang module.
- [erlang] Download precompiled binaries using `evision_precompiled.erl`.
- [erlang] Generate typespecs.
 
## v0.1.16 (2022-10-30)
[Browse the Repository](https://github.com/cocoa-xu/evision/tree/v0.1.16) | [Released Assets](https://github.com/cocoa-xu/evision/releases/tag/v0.1.16)

### Fixes
- [deps] `:kino` will be an optional dependency, if we use `if` before `defmodule`. This reverts the changes in in v0.1.15. 
  
  Thanks @josevalim for helping me figuring out why using `if` before `defmodule` would solve the problem. More details can be found [here](https://cocoa-research.works/2022/10/conditional-compliation-with-if-and-use-in-elixir/).

### Changes
- [config.exs] Added configurable parameters related to rendering `Evision.Mat` in Kino. (They are optional and can also be adjusted in runtime)

  - `config :evision, kino_render_image_encoding: :png`
  - `config :evision, kino_render_image_max_size: {8192, 8192}`
  - `config :evision, kino_render_tab_order: [:image, :raw, :numerical]`

### Added
- [Evision.Mat] Added a few functions related to Kino.Render

  | Function | Description |
  |:---------|:------------|
  |`Evision.Mat.kino_render_tab_order/0` | Get preferred order of Kino.Layout tabs for `Evision.Mat` in Livebook. |
  |`Evision.Mat.set_kino_render_tab_order/1` | Set preferred order of Kino.Layout tabs for `Evision.Mat` in Livebook. |
  |`Evision.Mat.kino_render_image_max_size/0` | Get the maximum allowed image size to render in Kino. |
  |`Evision.Mat.set_kino_render_image_max_size/1` | Set the maximum allowed image size to render in Kino. |
  |`Evision.Mat.kino_render_image_encoding/0` | Get preferred image encoding when rendering in Kino. |
  |`Evision.Mat.set_kino_render_image_encoding/1` | Set preferred image encoding when rendering in Kino. |

## v0.1.15 (2022-10-26)
[Browse the Repository](https://github.com/cocoa-xu/evision/tree/v0.1.15) | [Released Assets](https://github.com/cocoa-xu/evision/releases/tag/v0.1.15)
### Changes
- [mix compile] Suppress logs if `evision.so` is already presented when compiling from source.
- [Precompile] Added precompile target `aarch64-windows-msvc`.

### Fixes
- [deps] `:kino` should be a required dependency

## v0.1.14 (2022-10-22)
[Browse the Repository](https://github.com/cocoa-xu/evision/tree/v0.1.14) | [Released Assets](https://github.com/cocoa-xu/evision/releases/tag/v0.1.14)
### Breaking Changes
- [Precompile] Linux: remove GTK support in precompiled binaries. (This change only affects users on Linux.)

  This means functions in the `Evision.HighGui` module will return error if you are using precompiled binaries. This follows the convention in `opencv-python`.

  Workarounds for this:
  1. compile `evision` from source so that OpenCV will try to use the GUI backends they support on your system.
  2. use `Evision.Wx`. still in development, but basic functions like `imshow/2` are available. However, it requires Erlang to be compiled with `wxWidgets`.
  3. use Livebook with `:kino >= 0.7`. `evision` has built-in support for `Kino.Render` which can automatically give a visualised result in Livebook. This requires `:kino >= 0.7`.

- [Evision.Nx] Module `Evision.Nx` is now removed. Functions in `Evision.Nx` were moved to `Evision.Mat` in v0.1.13. Many thanks to @zacky1972 and @josevalim for their contributions to this module in very early days of the development.

  | Old                      | New                         |
  |:------------------------:|:---------------------------:|
  |`Evision.Nx.to_mat/{1,2}` | `Evision.Mat.from_nx/{1,2}` |
  |`Evision.Nx.to_mat/5`     | `Evision.Mat.from_binary/5` |
  |`Evision.Nx.to_mat_2d/1`  | `Evision.Mat.from_nx_2d/1`  |
  |`Evision.Nx.to_nx/1`      | `Evision.Mat.to_nx/1`       |

### Added
- [Evision.Wx] implemented `imshow/2`, `destroyWindow/1` and `destroyAllWindows/0`.
- [SmartCell] Added SmartCells. They are optional and `:kino >= 0.7` will be required to use them.

  If you'd like to use smartcells, please add `:kino` to `deps` in the `mix.exs` file.

  ```elixir
  defp deps do
    [
      # ...
      {:kino, "~> 0.7"},
      # ...
    ]
  end
  ```

  And then please register smartcells to `:kino` by invoking `Evision.SmartCell.register_smartcells()`.

  `Evision.SmartCell.available_smartcells/0` will return all available smartcells.

  (Optional step) It's also possible to add only some of these smartcells, for example,

  ```elixir
  Evision.SmartCell.register_smartcells([
    Evision.SmartCell.ML.TrainData,
    Evision.SmartCell.ML.SVM
  ])
  ```

## v0.1.13 (2022-10-19)
[Browse the Repository](https://github.com/cocoa-xu/evision/tree/v0.1.13) | [Released Assets](https://github.com/cocoa-xu/evision/releases/tag/v0.1.13)
### Fixes
- [c_src] Specialised function `evision_to [with Tp_=cv::UMat]`.
- [Evision.Backend] ensure that an `Evision.Mat` is returned from `reject_error/1`.

### Changed
- [c_src] `parseSequence` will only handle tuples.
- [Evision.Mat] `Evision.Mat.quicklook` will use alternative escaping sequence to avoid having a dedicate function in NIF. Thanks to @akash-akya and @kipcole9 ([vix#68](https://github.com/akash-akya/vix/pull/68)).

  ```
  ST means either BEL (hex code 0x07) or ESC \\.
  ```

- [nx-integration] Functions in `Evision.Nx` are now moved to `Evision.Mat`.

  | Old                      | New                         |
  |:------------------------:|:---------------------------:|
  |`Evision.Nx.to_mat/{1,2}` | `Evision.Mat.from_nx/{1,2}` |
  |`Evision.Nx.to_mat/5`     | `Evision.Mat.from_binary/5` |
  |`Evision.Nx.to_mat_2d/1`  | `Evision.Mat.from_nx_2d/1`  |
  |`Evision.Nx.to_nx/1`      | `Evision.Mat.to_nx/1`       |

As of v0.1.13, calls to these old functions will be forwarded to the corresponding new ones. 

In the next release (v0.1.14), `Evision.Nx` will be removed.

- [Evision.Mat] `Evision.Mat.tranpose` will use `cv::transposeND` if possible.
- [Precompile] Try to compile OpenCV with gtk3 support.

### Added
- [test] Added a test for `Evision.warpPerspective`.
- [example] Added an example for `Evision.warpPerspective`.
- [example] Added some examples for `Evision.warpPolar`.
- [example] Added QRCode encoding and decoding example.
- [docs] Added a cheatsheet.

## v0.1.12 (2022-10-15)
[Browse the Repository](https://github.com/cocoa-xu/evision/tree/v0.1.12) | [Released Assets](https://github.com/cocoa-xu/evision/releases/tag/v0.1.12)

### Breaking Changes
- [Evision.QRCodeEncoder.Params] Renamed `Evision.QRCodeEncoder.Params.qrcodeencoder_params/0` to `Evision.QRCodeEncoder.Params.params/0`.

### Fixes
- Function guard should also allow `Nx.Tensor` when the corresponding input argument is `Evision.Mat.maybe_mat_in()`.
- [Evision.Mat] `Evision.Mat.quicklook/1`  should also check the number of channels is one of `[1, 3, 4]` when `dims == 2`.
- [c_src] `evision_cv_mat_broadcast_to` should call `enif_free((void *)dst_data);` if `void * tmp_data = (void *)enif_alloc(elem_size * count_new_elem);` failed.
- [py_src] Fixed the template of simple call constructor.

### Changed
- [Docs] Example Livebooks is now included in docs as extras.
- [Evision.Mat] `Evision.Mat.roi/{2,3}` now supports Elixir Range.
- [Evision.Mat] Implemented Access behaviour. 

  - `Access.fetch/2` examples:

    ```elixir
    iex> img = Evision.imread("test/qr_detector_test.png")
    %Evision.Mat{
      channels: 3,
      dims: 2,
      type: {:u, 8},
      raw_type: 16,
      shape: {300, 300, 3},
      ref: #Reference<0.809884129.802291734.78316>
    }

    # Same behaviour as Nx. 
    # Also, img[0] gives the same result as img[[0]]
    # For this example, they are both equvilent of img[[0, :all, :all]]
    iex> img[[0]]
    %Evision.Mat{
      channels: 3,
      dims: 2,
      type: {:u, 8},
      raw_type: 16,
      shape: {1, 300, 3},
      ref: #Reference<0.809884129.802291731.77296>
    }

    # same as img[[0..100, 50..200, :all]]
    # however, currently we only support ranges with step size 1
    #
    # **IMPORTANT NOTE**
    #
    # also, please note that we are using Elixir.Range here
    # and Elixir.Range is **inclusive**, i.e, [start, end] 
    # while cv::Range `{integer(), integer()}` is `[start, end)`
    # the difference can be observed in the `shape` field
    iex> img[[0..100, 50..200]]
    %Evision.Mat{
      channels: 3,
      dims: 2,
      type: {:u, 8},
      raw_type: 16,
      shape: {101, 151, 3},
      ref: #Reference<0.809884129.802291731.77297>
    }
    iex> img[[{0, 100}, {50, 200}]]
    %Evision.Mat{
      channels: 3,
      dims: 2,
      type: {:u, 8},
      raw_type: 16,
      shape: {100, 150, 3},
      ref: #Reference<0.809884129.802291731.77297>
    }

    # for this example, the result is the same as `Evision.extractChannel(img, 0)`
    iex> img[[:all, :all, 0]]
    %Evision.Mat{
      channels: 1,
      dims: 2,
      type: {:u, 8},
      raw_type: 0,
      shape: {300, 300},
      ref: #Reference<0.809884129.802291731.77298>
    }
    iex> img[[:all, :all, 0..1]]
    %Evision.Mat{
      channels: 2,
      dims: 2,
      type: {:u, 8},
      raw_type: 8,
      shape: {300, 300, 2},
      ref: #Reference<0.809884129.802291731.77299>
    }

    # when index is out of bounds
    iex> img[[:all, :all, 42]]
    {:error, "index 42 is out of bounds for axis 2 with size 3"}

    # it works the same way for any dimensional Evision.Mat
    iex> mat = Evision.Mat.ones({10, 10, 10, 10, 10}, :u8)
    iex> mat[[1..7, :all, 2..6, 3..9, :all]]
    %Evision.Mat{
      channels: 1,
      dims: 5,
      type: {:u, 8},
      raw_type: 0,
      shape: {7, 10, 5, 7, 10},
      ref: #Reference<0.3015448455.3766878228.259075>
    }
    ```

  - `Access.get_and_update/3` examples:

    ```elixir
    iex> mat = Evision.Mat.zeros({5, 5}, :u8)
    iex> Evision.Nx.to_nx(mat)
    #Nx.Tensor<
      u8[5][5]
      Evision.Backend
      [
        [0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0]
      ]
    >
    iex> {old, new} = Evision.Mat.get_and_update(mat, [1..3, 1..3], fn roi ->
        {roi, Nx.broadcast(Nx.tensor(255, type: roi.type), roi.shape)}
    end)
    iex> Evision.Nx.to_nx(new)
    #Nx.Tensor<
      u8[5][5]
      Evision.Backend
      [
        [0, 0, 0, 0, 0],
        [0, 255, 255, 255, 0],
        [0, 255, 255, 255, 0],
        [0, 255, 255, 255, 0],
        [0, 0, 0, 0, 0]
      ]
    >
    ```

## v0.1.11 (2022-10-13)
[Browse the Repository](https://github.com/cocoa-xu/evision/tree/v0.1.11) | [Released Assets](https://github.com/cocoa-xu/evision/releases/tag/v0.1.11)
### Important Note
In `v0.1.10`, an invalid checksum file was pushed to hex.pm, please read the changelog, especially the breaking changes in `v0.1.10`. [Changelog for `v0.1.10`](https://github.com/cocoa-xu/evision/releases/tag/v0.1.10).

### Fixed
- [Precompile] `Mix.Tasks.Evision.Fetch` should always download and oerwrite existing files.

## v0.1.10 (2022-10-13)
[Browse the Repository](https://github.com/cocoa-xu/evision/tree/v0.1.10) | [Released Assets](https://github.com/cocoa-xu/evision/releases/tag/v0.1.10)
### Important Note
Invalid checksum file was pushed to hex.pm, please use `v0.1.11` instead.

### Breaking Changes
- Say goodbye to the bang(!) version functions!

  Thanks to @josevalim who wrote me this `Errorize` module back in Feb 2022, and in v0.1.10 this module will be removed. There are two main reasons for this:

  - I've managed to structurise all `#references` that have their own modules in [#101](https://github.com/cocoa-xu/evision/pull/101).
  - After generating function specs, dialyzer seems to be really upset about these bang(!) version functions, and would emit a few thousand warnings.
- [Precompile] Include NIF version in precompiled tarball filename.

  ```elixir
  "evision-nif_#{nif_version}-#{target}-#{version}"
  ```

- Return value changed if the first return type of the function is `bool`

  - If the function only returns a `bool`, the updated return value will simple be `true` or `false`.
  
    ```elixir
    # before
    iex> :ok = Evision.imwrite("/path/to/image.png", img)
    iex> :error = Evision.imwrite("/path/to/image.png", invalid_img)
    # after
    iex> true = Evision.imwrite("/path/to/image.png", img)
    iex> false = Evision.imwrite("/path/to/image.png", invalid_img)
    ```

  - If the first return type is `bool`, and there is another value to return:
  
    ```elixir
    # before
    iex> frame = Evision.VideoCapture.read(capture) # has a frame available
    iex> :error = Evision.VideoCapture.read(capture) # cannot read / no more available frames
    # after
    iex> frame = Evision.VideoCapture.read(capture) # has a frame available
    iex> false = Evision.VideoCapture.read(capture) # cannot read / no more available frames
    ```
  
  - If the first return type is `bool`, and there are more than one value to return:

    ```elixir
    # before
    iex> {val1, val2} = Evision.SomeModule.some_function(arg1) # when succeeded
    iex> :error = Evision.SomeModule.some_function(capture) # when failed
    # after
    iex> {val1, val2} = Evision.SomeModule.some_function(arg1) # when succeeded
    iex> false = Evision.SomeModule.some_function(capture) # when failed
    ```

- `std::string` and `cv::String` will be wrapped in a binary term instead of a list.

  For example,

  ```elixir
  # before
  iex> {'detected text', _, _} = Evision.QRCodeDetector.detectAndDecode(qr, img)
  # after
  iex> {"detected text", _, _} = Evision.QRCodeDetector.detectAndDecode(qr, img)
  ```

- Structurised all `#reference`s that have their own module.

  A list of modules that are now wrapped in structs can be found in the appendix section.

- [Evision.DNN] As it's not possible to distinguish `std::vector<uchar>` and `String` in Elixir, `Evision.DNN::readNet*` functions that load a model from in-memoy buffer will be renamed to `Evision.DNN::readNet*Buffer`.

  For example, 

  ```elixir
  @spec readNetFromONNX(binary()) :: Evision.DNN.Net.t() | {:error, String.t()}
  def readNetFromONNX(onnxFile)

  @spec readNetFromONNXBuffer(binary()) :: Evision.DNN.Net.t() | {:error, String.t()}
  def readNetFromONNXBuffer(buffer)
  ```

### Changed
- [Evision.Backend] raise a better error message for callbacks that haven't been implemented in `Evision.Backend`. Thanks to @josevalim

  An example of the updated error message:
  
  ```elixir
  iex> Evision.Backend.slice(1,2,3,4,5)
  ** (RuntimeError) operation slice is not yet supported on Evision.Backend.
  Please use another backend like Nx.BinaryBackend or Torchx.Backend.
    To use Torchx.Backend, :torchx should be added to your app's deps.
    Please see https://github.com/elixir-nx/nx/tree/main/torchx for more information on how to install and use it.
  To convert the tensor to another backend, please use Evision.Nx.to_nx(tensor, Backend.ModuleName)
    for example, Evision.Nx.to_nx(tensor, Nx.BinaryBackend) or Evision.Nx.to_nx(tensor, Torchx.Backend).
  Pull request would be more than welcomed if you'd like to implmenent this function and make contributions.
      (evision 0.1.10-dev) lib/evision/backend.ex:815: Evision.Backend.slice/5
      iex:1: (file)
  ```

- [Docs] Improved cross reference in inline docs. For example,

  #### Before
  ```elixir
  @doc """
  ...
  @see setCVFolds
  ...
  """
  def getCVFolds(self) do
  ```

  #### After
  ```elixir
  @doc """
  ...
  @see `setCVFolds/2`
  ...
  """
  def getCVFolds(self) do
  ```

  In this way, you can navigate to the referenced function in the generated html docs.

### Fixed
- Docs: included `retval` and `self` in the `Return` section.

### Added
- [Spec] Function spec for all Elixir functions, including generated ones.
- [Evision.Mat] Added `Evision.Mat.roi/{2,3}`.

  ```elixir
  iex> img = Evision.imread("test/qr_detector_test.png")
  %Evision.Mat{
    channels: 3,
    dims: 2,
    type: {:u, 8},
    raw_type: 16,
    shape: {300, 300, 3},
    ref: #Reference<0.3957900973.802816029.173984>
  }

  # Mat operator()( const Rect& roi ) const;
  iex> sub_img = Evision.Mat.roi(img, {10, 10, 100, 200})
  %Evision.Mat{
    channels: 3,
    dims: 2,
    type: {:u, 8},
    raw_type: 16,
    shape: {200, 100, 3},
    ref: #Reference<0.3957900973.802816020.173569>
  }

  # Mat operator()( Range rowRange, Range colRange ) const;
  iex> sub_img = Evision.Mat.roi(img, {10, 100}, {20, 200}) 
  %Evision.Mat{
    channels: 3,
    dims: 2,
    type: {:u, 8},
    raw_type: 16,
    shape: {90, 180, 3},
    ref: #Reference<0.3957900973.802816020.173570>
  }
  iex> sub_img = Evision.Mat.roi(img, :all, {20, 200}) 
  %Evision.Mat{
    channels: 3,
    dims: 2,
    type: {:u, 8},
    raw_type: 16,
    shape: {300, 180, 3},
    ref: #Reference<0.3957900973.802816020.173571>
  }

  # Mat operator()(const std::vector<Range>& ranges) const;
  iex> sub_img = Evision.Mat.roi(img, [{10, 100}, {10, 100}])
  %Evision.Mat{
    channels: 3,
    dims: 2,
    type: {:u, 8},
    raw_type: 16,
    shape: {90, 90, 3},
    ref: #Reference<0.3957900973.802816020.173567>
  }
  iex> sub_img = Evision.Mat.roi(img, [{10, 100}, :all])
  %Evision.Mat{
    channels: 3,
    dims: 2,
    type: {:u, 8},
    raw_type: 16,
    shape: {90, 300, 3},
    ref: #Reference<0.3957900973.802816020.173568>
  }
  ```

- [Evision.Mat] Added `Evision.Mat.quicklook/1`.

  This function will check the value of `:display_inline_image_iterm2` in the application config. If is `true`,
  then it will detect if current session is running in `iTerm2` (by checking the environment variable `LC_TERMINAL`).

  If both are `true`, we next check if the image is a 2D image, also if its size is within the limits. The maximum size can be set in the application config, for example,

  ```elixir
  config :evision, display_inline_image_iterm2: true
  config :evision, display_inline_image_max_size: {8192, 8192}
  ```

  If it passes all the checks, then it will be displayed as an inline image in iTerm2.

### Appendix
List of modules that are now wrapped in structs.

  - `Evision.AKAZE`
  - `Evision.AffineFeature`
  - `Evision.AgastFeatureDetector`
  - `Evision.Algorithm`
  - `Evision.AlignExposures`
  - `Evision.AlignMTB`
  - `Evision.AsyncArray`
  - `Evision.BFMatcher`
  - `Evision.BOWImgDescriptorExtractor`
  - `Evision.BOWKMeansTrainer`
  - `Evision.BOWTrainer`
  - `Evision.BRISK`
  - `Evision.BackgroundSubtractor`
  - `Evision.BackgroundSubtractorKNN`
  - `Evision.BackgroundSubtractorMOG2`
  - `Evision.CLAHE`
  - `Evision.CUDA`
  - `Evision.CUDA.BufferPool`
  - `Evision.CUDA.DeviceInfo`
  - `Evision.CUDA.Event`
  - `Evision.CUDA.GpuMat`
  - `Evision.CUDA.HostMem`
  - `Evision.CUDA.Stream`
  - `Evision.CUDA.TargetArchs`
  - `Evision.CalibrateCRF`
  - `Evision.CalibrateDebevec`
  - `Evision.CalibrateRobertson`
  - `Evision.CascadeClassifier`
  - `Evision.CirclesGridFinderParameters`
  - `Evision.DISOpticalFlow`
  - `Evision.DMatch`
  - `Evision.DNN.ClassificationModel`
  - `Evision.DNN.DetectionModel`
  - `Evision.DNN.DictValue`
  - `Evision.DNN.KeypointsModel`
  - `Evision.DNN.Layer`
  - `Evision.DNN.Model`
  - `Evision.DNN.Net`
  - `Evision.DNN.SegmentationModel`
  - `Evision.DNN.TextDetectionModel`
  - `Evision.DNN.TextDetectionModelDB`
  - `Evision.DNN.TextDetectionModelEAST`
  - `Evision.DNN.TextRecognitionModel`
  - `Evision.DenseOpticalFlow`
  - `Evision.DescriptorMatcher`
  - `Evision.Detail.AffineBasedEstimator`
  - `Evision.Detail.AffineBestOf2NearestMatcher`
  - `Evision.Detail.BestOf2NearestMatcher`
  - `Evision.Detail.BestOf2NearestRangeMatcher`
  - `Evision.Detail.Blender`
  - `Evision.Detail.BlocksChannelsCompensator`
  - `Evision.Detail.BlocksCompensator`
  - `Evision.Detail.BlocksGainCompensator`
  - `Evision.Detail.BundleAdjusterAffine`
  - `Evision.Detail.BundleAdjusterAffinePartial`
  - `Evision.Detail.BundleAdjusterBase`
  - `Evision.Detail.BundleAdjusterRay`
  - `Evision.Detail.BundleAdjusterReproj`
  - `Evision.Detail.CameraParams`
  - `Evision.Detail.ChannelsCompensator`
  - `Evision.Detail.DpSeamFinder`
  - `Evision.Detail.Estimator`
  - `Evision.Detail.ExposureCompensator`
  - `Evision.Detail.FeatherBlender`
  - `Evision.Detail.FeaturesMatcher`
  - `Evision.Detail.GainCompensator`
  - `Evision.Detail.GraphCutSeamFinder`
  - `Evision.Detail.HomographyBasedEstimator`
  - `Evision.Detail.ImageFeatures`
  - `Evision.Detail.MatchesInfo`
  - `Evision.Detail.MultiBandBlender`
  - `Evision.Detail.NoBundleAdjuster`
  - `Evision.Detail.NoExposureCompensator`
  - `Evision.Detail.NoSeamFinder`
  - `Evision.Detail.PairwiseSeamFinder`
  - `Evision.Detail.SeamFinder`
  - `Evision.Detail.SphericalProjector`
  - `Evision.Detail.Timelapser`
  - `Evision.Detail.VoronoiSeamFinder`
  - `Evision.FaceDetectorYN`
  - `Evision.FaceRecognizerSF`
  - `Evision.FarnebackOpticalFlow`
  - `Evision.FastFeatureDetector`
  - `Evision.Feature2D`
  - `Evision.FileNode`
  - `Evision.FileStorage`
  - `Evision.Flann.Index`
  - `Evision.FlannBasedMatcher`
  - `Evision.GFTTDetector`
  - `Evision.GeneralizedHough`
  - `Evision.GeneralizedHoughBallard`
  - `Evision.GeneralizedHoughGuil`
  - `Evision.HOGDescriptor`
  - `Evision.KAZE`
  - `Evision.KalmanFilter`
  - `Evision.KeyPoint`
  - `Evision.LineSegmentDetector`
  - `Evision.ML.ANNMLP`
  - `Evision.ML.Boost`
  - `Evision.ML.DTrees`
  - `Evision.ML.EM`
  - `Evision.ML.KNearest`
  - `Evision.ML.LogisticRegression`
  - `Evision.ML.NormalBayesClassifier`
  - `Evision.ML.ParamGrid`
  - `Evision.ML.RTrees`
  - `Evision.ML.SVM`
  - `Evision.ML.SVMSGD`
  - `Evision.ML.StatModel`
  - `Evision.ML.TrainData`
  - `Evision.MSER`
  - `Evision.MergeDebevec`
  - `Evision.MergeExposures`
  - `Evision.MergeMertens`
  - `Evision.MergeRobertson`
  - `Evision.OCL`
  - `Evision.OCL.Device`
  - `Evision.ORB`
  - `Evision.Parallel`
  - `Evision.PyRotationWarper`
  - `Evision.QRCodeDetector`
  - `Evision.QRCodeEncoder`
  - `Evision.QRCodeEncoder.Params`
  - `Evision.SIFT`
  - `Evision.Samples`
  - `Evision.Segmentation.IntelligentScissorsMB`
  - `Evision.SimpleBlobDetector`
  - `Evision.SimpleBlobDetector.Params`
  - `Evision.SparseOpticalFlow`
  - `Evision.SparsePyrLKOpticalFlow`
  - `Evision.StereoBM`
  - `Evision.StereoMatcher`
  - `Evision.StereoSGBM`
  - `Evision.Stitcher`
  - `Evision.Subdiv2D`
  - `Evision.TickMeter`
  - `Evision.Tonemap`
  - `Evision.TonemapDrago`
  - `Evision.TonemapMantiuk`
  - `Evision.TonemapReinhard`
  - `Evision.Tracker`
  - `Evision.TrackerDaSiamRPN`
  - `Evision.TrackerDaSiamRPN.Params`
  - `Evision.TrackerGOTURN`
  - `Evision.TrackerGOTURN.Params`
  - `Evision.TrackerMIL`
  - `Evision.TrackerMIL.Params`
  - `Evision.UMat`
  - `Evision.UsacParams`
  - `Evision.Utils.Nested.OriginalClassName`
  - `Evision.Utils.Nested.OriginalClassName.Params`
  - `Evision.VariationalRefinement`
  - `Evision.VideoCapture`
  - `Evision.VideoWriter`


## v0.1.9 (2022-10-09)
[Browse the Repository](https://github.com/cocoa-xu/evision/tree/v0.1.9) | [Released Assets](https://github.com/cocoa-xu/evision/releases/tag/v0.1.9)
### Bug Fixes
- `Mix.Tasks.Compile.EvisionPrecompiled`: using `File.cp_r/2` instead of calling `cp -a` via `System.cmd/3`.
- Fixed TLS warnings when downloading precompiled tarball file. Thanks to @kipcole9!
- Only include `evision_custom_headers/evision_ml.hpp` if the `HAVE_OPENCV_ML` macro is defined.
- Support parsing `RefWrapper<T> (&value)[N]` from list or tuple. ([#99](https://github.com/cocoa-xu/evision/issues/99))

  See the function in `c_src/evision.cpp`.

  ```cpp
  bool parseSequence(ErlNifEnv *env, ERL_NIF_TERM obj, RefWrapper<T> (&value)[N], const ArgInfo& info)
  ```

  ```elixir
  # `RotatedRect` has to be a tuple, {centre, size, angle}
  Evision.boxPoints!({{224.0, 262.5}, {343.0, 344.0}, 90.0})

  # while `Point`/`Size` can be either a list, `[x, y]`, or a tuple, `{x, y}`
  Evision.boxPoints!({[224.0, 262.5], [343.0, 344.0], 90.0})
  ```

- Fixed the mapping from a type to the corresponding function guard in `py_src/helper.py`. ([#99](https://github.com/cocoa-xu/evision/issues/99))


### Changed
- Display `RotatedRect` type as `{centre={x, y}, size={s1, s2}, angle}` in docs.

## v0.1.8 (2022-10-08)
[Browse the Repository](https://github.com/cocoa-xu/evision/tree/v0.1.8) | [Released Assets](https://github.com/cocoa-xu/evision/releases/tag/v0.1.8)
### Changed
- `CMake` and `make` (`nmake` if on Windows) will not be used to download and deploy precompiled binaries for Elixir users.

  This means that `evision` can be downloaded and deployed once Erlang and Elixir are properly installed on the system.

## v0.1.7 (2022-10-07)
[Browse the Repository](https://github.com/cocoa-xu/evision/tree/v0.1.7) | [Released Assets](https://github.com/cocoa-xu/evision/releases/tag/v0.1.7)
### Breaking Changes
- `EVISION_PREFER_PRECOMPILED` is set to `true` by default. 
   
   `:evision` will try to use precompiled binaries if available. Otherwise, it will fallback to building from source.

- Precompiled binary filename changed:

  ```
  arm64-apple-darwin => aarch64-apple-darwin
  amd64-windows-msvc => x86_64-windows-msvc
  ```

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

- raise RuntimeError for all unimplemented `:nx` callbacks.

  ```elixir
  raise RuntimeError, "not implemented yet"
  ```

- Elixir functions that have the same name and arity will be grouped together now.

  This should massively reduce the number of warnings emitted by the elixir compiler.

- Only generate corresponding binding code.
  - Only generate binding code for Elixir when compiling `:evision` using `mix`; 
  - Only generate binding code for erlang when compiling `:evision` using `rebar`;
  
  It's possible to generate erlang and Elixir at the same time. However, currently it's only possible to do so when compiling evision using `mix`.

  ```shell
  # default value is `elixir` when compiling evision using `mix`
  # default value is `erlang` when compiling evision using `rebar`
  #
  # expected format is a comma-separated string
  export EVISION_GENERATE_LANG="erlang,elixir"
  ```

- Better inline docs.
  - Inline docs will have a section for `Positional Arguments` and a section for `Keyword Arguments`. For example,

    ~~~elixir
    @doc """
    ### Positional Arguments
    - **bboxes**: vector_Rect2d. 
    - **scores**: vector_float. 
    - **score_threshold**: float. 
    - **nms_threshold**: float. 

    ### Keyword Arguments
    - **eta**: float.
    - **top_k**: int.

    Performs non maximum suppression given boxes and corresponding scores.

    Python prototype (for reference): 
    ```
    NMSBoxes(bboxes, scores, score_threshold, nms_threshold[, eta[, top_k]]) -> indices
    ```
    """
    @doc namespace: :"cv.dnn"
    def nmsBoxes(bboxes, scores, score_threshold, nms_threshold, opts)
    ~~~

  - If a function (same name and arity) has multiple variants, the inline docs will show each of them in section `## Variant VAR_INDEX`. For example,
  
    ~~~elixir
    @doc """
    #### Variant 1:

    ##### Positional Arguments
    - **dx**: UMat. 
    - **dy**: UMat. 
    - **threshold1**: double. 
    - **threshold2**: double. 

    ##### Keyword Arguments
    - **edges**: UMat.
    - **l2gradient**: bool.

      \\overload
      Finds edges in an image using the Canny algorithm with custom image gradient.
        \\f$=\\sqrt{(dI/dx)^2 + (dI/dy)^2}\\f$ should be used to calculate the image gradient magnitude (
        L2gradient=true ), or whether the default \\f$L\\_1\\f$ norm \\f$=|dI/dx|+|dI/dy|\\f$ is enough (
        L2gradient=false ).

    Python prototype (for reference): 
    ```
    Canny(dx, dy, threshold1, threshold2[, edges[, L2gradient]]) -> edges
    ```
    #### Variant 2:

    ##### Positional Arguments
    - **image**: UMat. 
    - **threshold1**: double. 
    - **threshold2**: double. 

    ##### Keyword Arguments
    - **edges**: UMat.
    - **apertureSize**: int.
    - **l2gradient**: bool.

    Finds edges in an image using the Canny algorithm @cite Canny86 .
      The function finds edges in the input image and marks them in the output map edges using the
      Canny algorithm. The smallest value between threshold1 and threshold2 is used for edge linking. The
      largest value is used to find initial segments of strong edges. See
      <http://en.wikipedia.org/wiki/Canny_edge_detector>
        \\f$=\\sqrt{(dI/dx)^2 + (dI/dy)^2}\\f$ should be used to calculate the image gradient magnitude (
        L2gradient=true ), or whether the default \\f$L\\_1\\f$ norm \\f$=|dI/dx|+|dI/dy|\\f$ is enough (
        L2gradient=false ).

    Python prototype (for reference): 
    ```
    Canny(image, threshold1, threshold2[, edges[, apertureSize[, L2gradient]]]) -> edges
    ```

    """
    @doc namespace: :cv
    def canny(image, threshold1, threshold2, opts)
    when (is_reference(image) or is_struct(image)) and is_number(threshold1) and is_number(threshold2) and is_list(opts) and (opts == [] or is_tuple(hd(opts))), do: # variant 2

    def canny(dx, dy, threshold1, threshold2)
    when (is_reference(dx) or is_struct(dx)) and (is_reference(dy) or is_struct(dy)) and is_number(threshold1) and is_number(threshold2), do: # variant 1
    ~~~

- Better integration with `:nx`.

  ```elixir
  iex> t = Nx.tensor([[[0,0,0], [255, 255, 255]]], type: :u8)
  #Nx.Tensor<
    u8[1][2][3]
    [
      [
        [0, 0, 0],
        [255, 255, 255]
      ]
    ]
  >
  iex> mat = Evision.imread!("test.png")
  %Evision.Mat{
    channels: 3,
    dims: 2,
    type: {:u, 8},
    raw_type: 16,
    shape: {1, 2, 3},
    ref: #Reference<0.2067356221.74055707.218654>
  }
  iex> mat = Evision.Mat.channel_as_last_dim!(mat)
  %Evision.Mat{
    channels: 1,
    dims: 3,
    type: {:u, 8},
    raw_type: 0,
    shape: {1, 2, 3},
    ref: #Reference<0.2067356221.74055698.218182>
  }
  iex> result = Evision.Mat.add!(t, mat)
  %Evision.Mat{
    channels: 1,
    dims: 3,
    type: {:u, 8},
    raw_type: 0,
    shape: {1, 2, 3},
    ref: #Reference<0.2067356221.74055698.218184>
  }
  iex> Evision.Nx.to_nx!(result)
  #Nx.Tensor<
    u8[1][2][3]
    Evision.Backend
    [
      [
        [255, 255, 255],
        [255, 255, 255]
      ]
    ]
  >
  ```

- Implemented property setter for `cv::Ptr<>` wrapped types. For example,

  ```elixir
  iex> k = Evision.KalmanFilter.kalmanFilter!(1, 1)
  #Reference<0.382162378.457572372.189094>
  iex> Evision.KalmanFilter.get_gain!(k) |> Evision.Nx.to_nx!
  #Nx.Tensor<
    f32[1][1]
    Evision.Backend
    [
      [0.0]
    ]
  >
  iex> Evision.KalmanFilter.set_gain!(k, Evision.Mat.literal!([1.0], :f32))
  #Reference<0.382162378.457572372.189094>
  iex> Evision.KalmanFilter.get_gain!(k) |> Evision.Nx.to_nx!
  #Nx.Tensor<
    f32[1][1]
    Evision.Backend
    [
      [1.0]
    ]
  >
  ```

- More detailed error message for property getter/setter. For example,

  - When setting a property that is type `A` and value passed to the setter is type `B`, and there is no known conversion from `B` to `A`, then it will return an error-tuple

    ```elixir 
    iex> k = Evision.KalmanFilter.kalmanFilter!(1, 1)
    iex> Evision.KalmanFilter.set_gain(k, :p)
    {:error, "cannot assign new value, mismatched type?"}
    iex> Evision.KalmanFilter.set_gain(k, :p)
    ** (RuntimeError) cannot assign new value, mismatched type?
        (evision 0.1.7) lib/generated/evision_kalmanfilter.ex:175: Evision.KalmanFilter.set_gain!/2
        iex:7: (file)
    ```

  - For property getter/setter, if the `self` passed in is a different type than what is expected, an error-tuple will be returned

    ```elixir
    iex> mat = Evision.Mat.literal!([1.0], :f32)
    %Evision.Mat{
      channels: 1,
      dims: 2,
      type: {:f, 32},
      raw_type: 5,
      shape: {1, 1},
      ref: #Reference<0.1499445684.3682467860.58544>
    }
    iex> Evision.KalmanFilter.set_gain(mat, mat) 
    {:error,
    "cannot get `Ptr<cv::KalmanFilter>` from `self`: mismatched type or invalid resource?"}
    iex> Evision.KalmanFilter.set_gain!(mat, mat)
    ** (RuntimeError) cannot get `Ptr<cv::KalmanFilter>` from `self`: mismatched type or invalid resource?
        (evision 0.1.7) lib/generated/evision_kalmanfilter.ex:175: Evision.KalmanFilter.set_gain!/2
        iex:2: (file)
    ```

- `evision_##NAME##_getp` (in `c_src/erlcompat.hpp`) should just return true or false. 
  
  Returning a `ERL_NIF_TERM` (`enif_make_badarg`) in the macro (when `enif_get_resource` fails) will prevent the caller from returning an error-tuple with detailed error message.

- Improved the quality of generated inline docs.

  Also displays what variable(s) will be returned (when applicable) in the `##### Return` section.

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

  `Evision.Mat.literal/3` will return a valid 2D image if the keyword argument, `as_2d`, is set to `true` and if the list literal can be represented as a 2D image.
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

- Added `Evision.Mat.channel_as_last_dim/1`.

  This function does the opposite as to `Evision.Mat.last_dim_as_channel/1`.

  If the number of channels of the input Evision.Mat is greater than 1,
  then this function would convert the input Evision.Mat with dims `dims=list(int())` to a `1`-channel Evision.Mat with dims `[dims | channels]`.

  If the number of channels of the input Evision.Mat is equal to 1,
  - if dims == shape, then nothing happens
  - otherwise, a new Evision.Mat that has dims=`[dims | channels]` will be returned
  
  For example,

  ```elixir
  iex> mat = Evision.imread!("test.png")
  %Evision.Mat{
    channels: 3,
    dims: 2,
    type: {:u, 8},
    raw_type: 16,
    shape: {1, 2, 3},
    ref: #Reference<0.2067356221.74055707.218654>
  }
  iex> mat = Evision.Mat.channel_as_last_dim!(mat)
  %Evision.Mat{
    channels: 1,
    dims: 3,
    type: {:u, 8},
    raw_type: 0,
    shape: {1, 2, 3},
    ref: #Reference<0.2067356221.74055698.218182>
  }
  ```

- Automatically displays a tabbed output in Livebook if the type of evaluated result is `Evision.Mat`.

  This is an optional feature. To enable it, `:kino` should be added to `deps`, e.g.,

  ```elixir
  defp deps do
    [
      # ...
      {:kino, "~> 0.7"},
      # ...
    ]
  end
  ```

  Now, with `:kino` >= v0.7 available, a tabbed output will shown in Livebook if the evaluated result is an `Evision.Mat`.

  A `Raw` tab will always be the first one, e.g.,

  ```elixir
  %Evision.Mat{
    channels: 1,
    dims: 3,
    type: {:u, 8},
    raw_type: 0,
    shape: {1, 2, 3},
    ref: #Reference<0.3310236255.1057357843.168932>
  }
  ```

  For 2D images (`dims == 2`), the second tab will be `Image`, which displays the image.

  For all `Evision.Mat`, the last tab will be `Numerical`, which shows the numbers behind the scene. Of course, for large size `Evision.Mat`, only part of the data will be shown. A example output in this tab:

  ```elixir
  #Nx.Tensor<
    u8[1][2][3]
    Evision.Backend
    [
      [
        [1, 2, 3],
        [1, 2, 3]
      ]
    ]
  >
  ```

## v0.1.6 (2022-09-29)
[Browse the Repository](https://github.com/cocoa-xu/evision/tree/v0.1.6) | [Released Assets](https://github.com/cocoa-xu/evision/releases/tag/v0.1.6)
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

### Bug Fixes
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

### Bug Fixes
- Fixed class inheritance issue in `py_src/class_info.py`.
- Fixed missing comma in example livebooks' `Mix.install`. Thanks to @dbii.

### Added
- Added decision tree and random forest example.

## v0.1.3 (2022-09-01)
[Browse the Repository](https://github.com/cocoa-xu/evision/tree/v0.1.3) | [Released Assets](https://github.com/cocoa-xu/evision/releases/tag/v0.1.3)
### Bug Fixes
- Fixed issues in restoring files from precompiled package for macOS and Linux.
  - Paths are now quoted. 
  - using `cp -RPf` on macOS while `cp -a` on Linux.
- Fixed `destroyAllWindows` in NIF.
  It was generated as 'erlang:destroyAllWindows/1' but it should be 'erlang:destroyAllWindows/0'.

## v0.1.2 (2022-08-26)
[Browse the Repository](https://github.com/cocoa-xu/evision/tree/v0.1.2) | [Released Assets](https://github.com/cocoa-xu/evision/releases/tag/v0.1.2)
### Bug Fixes
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
