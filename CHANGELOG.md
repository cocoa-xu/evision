# Changelog

## v0.1.10 (main)
[Browse the Repository](https://github.com/cocoa-xu/evision)
### Breaking Changes
- Say goodbye to the bang(!) version functions!

  Thanks to @josevalim who wrote me this `Errorize` module back in Feb 2022, and in v0.1.10 this module will be removed. There are two main reasons for this:

  - I've managed to structurise all `#references` that have their own modules in #101.
  - After generating function specs, dialyzer seems to be really upset about these bang(!) version functions, and would emit a few thousand warnings.

- Return value changed if the first return type of the function is `bool`

  - If the function only returns a `bool`, the updated return value will simple be `true` or `false`.
  
    ```elixir
    # before
    iex> :ok = Evision.imwrite!("/path/to/image.png", img)
    iex> :error = Evision.imwrite!("/path/to/image.png", invalid_img)
    # after
    iex> true = Evision.imwrite!("/path/to/image.png", img)
    iex> true = Evision.imwrite!("/path/to/image.png", invalid_img)
    ```

  - If the first return type is `bool`, and there is another value to return:
  
    ```elixir
    # before
    iex> frame = Evision.VideoCapture.read!(capture) # has a frame available
    iex> :error = Evision.VideoCapture.read!(capture) # cannot read / no more available frames
    # after
    iex> frame = Evision.VideoCapture.read!(capture) # has a frame available
    iex> false = Evision.VideoCapture.read!(capture) # cannot read / no more available frames
    ```
  
  - If the first return type is `bool`, and there are more than one value to return:

    ```elixir
    # before
    iex> {val1, val2} = Evision.SomeModule.some_function!(arg1) # when succeeded
    iex> :error = Evision.SomeModule.some_function!(capture) # when failed
    # after
    iex> {val1, val2} = Evision.SomeModule.some_function!(arg1) # when succeeded
    iex> false = Evision.SomeModule.some_function!(capture) # when failed
    ```

- `std::string` and `cv::String` will be wrapped in a binary term instead of a list.

  For example,

  ```elixir
  # before
  iex> {'detected text', _, _} = Evision.QRCodeDetector.detectAndDecode!(qr, img)
  # after
  iex> {"detected text", _, _} = Evision.QRCodeDetector.detectAndDecode!(qr, img)
  ```

- Structurised all `#reference`s that have their own module.

  For example, 

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

### Changed
- Improved cross reference in inline docs. For example,

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
- Function spec for all functions, including generated ones.

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

  `Evision.Mat.literal/3` will return a vaild 2D image if the keyword argument, `as_2d`, is set to `true` and if the list literal can be represented as a 2D image.
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
