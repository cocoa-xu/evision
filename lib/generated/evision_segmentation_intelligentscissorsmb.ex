defmodule Evision.Segmentation.IntelligentScissorsMB do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Segmentation.IntelligentScissorsMB` struct.

  - **ref**. `reference()`

    The underlying erlang resource variable.

  """
  @type t :: %__MODULE__{
    ref: reference()
  }
  @enforce_keys [:ref]
  defstruct [:ref]
  alias __MODULE__, as: T

  @doc false
  def to_struct({:ok, %{class: Evision.Segmentation.IntelligentScissorsMB, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Segmentation.IntelligentScissorsMB, ref: ref}) do
    %T{
      ref: ref
    }
  end

  @doc false
  def to_struct(ret) do
    Evision.Internal.Structurise.to_struct(ret)
  end
  
  @doc false
  def from_struct(%T{ref: ref}) do
    ref
  end

  @doc """
  IntelligentScissorsMB
  ##### Return
  - **self**: `Evision.Segmentation.IntelligentScissorsMB.t()`

  Python prototype (for reference only):
  ```python3
  IntelligentScissorsMB() -> <segmentation_IntelligentScissorsMB object>
  ```
  """
  @spec intelligentScissorsMB() :: Evision.Segmentation.IntelligentScissorsMB.t() | {:error, String.t()}
  def intelligentScissorsMB() do
    positional = [
    ]
    :evision_nif.segmentation_segmentation_IntelligentScissorsMB_IntelligentScissorsMB(positional)
    |> to_struct()
  end

  @doc """
  Specify input image and extract image features

  ##### Positional Arguments
  - **self**: `Evision.Segmentation.IntelligentScissorsMB.t()`
  - **image**: `Evision.Mat`.

    input image. Type is #CV_8UC1 / #CV_8UC3

  ##### Return
  - **retval**: `Evision.Segmentation.IntelligentScissorsMB.t()`

  Python prototype (for reference only):
  ```python3
  applyImage(image) -> retval
  ```
  """
  @spec applyImage(Evision.Segmentation.IntelligentScissorsMB.t(), Evision.Mat.maybe_mat_in()) :: Evision.Segmentation.IntelligentScissorsMB.t() | {:error, String.t()}
  def applyImage(self, image) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.segmentation_segmentation_IntelligentScissorsMB_applyImage(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Specify custom features of input image

  ##### Positional Arguments
  - **self**: `Evision.Segmentation.IntelligentScissorsMB.t()`
  - **non_edge**: `Evision.Mat`.

    Specify cost of non-edge pixels. Type is CV_8UC1. Expected values are `{0, 1}`.

  - **gradient_direction**: `Evision.Mat`.

    Specify gradient direction feature. Type is CV_32FC2. Values are expected to be normalized: `x^2 + y^2 == 1`

  - **gradient_magnitude**: `Evision.Mat`.

    Specify cost of gradient magnitude function: Type is CV_32FC1. Values should be in range `[0, 1]`.

  ##### Keyword Arguments
  - **image**: `Evision.Mat`.

    **Optional parameter**. Must be specified if subset of features is specified (non-specified features are calculated internally)

  ##### Return
  - **retval**: `Evision.Segmentation.IntelligentScissorsMB.t()`

   Customized advanced variant of applyImage() call.

  Python prototype (for reference only):
  ```python3
  applyImageFeatures(non_edge, gradient_direction, gradient_magnitude[, image]) -> retval
  ```
  """
  @spec applyImageFeatures(Evision.Segmentation.IntelligentScissorsMB.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:image, term()}] | nil) :: Evision.Segmentation.IntelligentScissorsMB.t() | {:error, String.t()}
  def applyImageFeatures(self, non_edge, gradient_direction, gradient_magnitude, opts) when (is_struct(non_edge, Evision.Mat) or is_struct(non_edge, Nx.Tensor) or is_number(non_edge) or is_tuple(non_edge)) and (is_struct(gradient_direction, Evision.Mat) or is_struct(gradient_direction, Nx.Tensor) or is_number(gradient_direction) or is_tuple(gradient_direction)) and (is_struct(gradient_magnitude, Evision.Mat) or is_struct(gradient_magnitude, Nx.Tensor) or is_number(gradient_magnitude) or is_tuple(gradient_magnitude)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:image])
    positional = [
      non_edge: Evision.Internal.Structurise.from_struct(non_edge),
      gradient_direction: Evision.Internal.Structurise.from_struct(gradient_direction),
      gradient_magnitude: Evision.Internal.Structurise.from_struct(gradient_magnitude)
    ]
    :evision_nif.segmentation_segmentation_IntelligentScissorsMB_applyImageFeatures(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Specify custom features of input image

  ##### Positional Arguments
  - **self**: `Evision.Segmentation.IntelligentScissorsMB.t()`
  - **non_edge**: `Evision.Mat`.

    Specify cost of non-edge pixels. Type is CV_8UC1. Expected values are `{0, 1}`.

  - **gradient_direction**: `Evision.Mat`.

    Specify gradient direction feature. Type is CV_32FC2. Values are expected to be normalized: `x^2 + y^2 == 1`

  - **gradient_magnitude**: `Evision.Mat`.

    Specify cost of gradient magnitude function: Type is CV_32FC1. Values should be in range `[0, 1]`.

  ##### Keyword Arguments
  - **image**: `Evision.Mat`.

    **Optional parameter**. Must be specified if subset of features is specified (non-specified features are calculated internally)

  ##### Return
  - **retval**: `Evision.Segmentation.IntelligentScissorsMB.t()`

   Customized advanced variant of applyImage() call.

  Python prototype (for reference only):
  ```python3
  applyImageFeatures(non_edge, gradient_direction, gradient_magnitude[, image]) -> retval
  ```
  """
  @spec applyImageFeatures(Evision.Segmentation.IntelligentScissorsMB.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.Segmentation.IntelligentScissorsMB.t() | {:error, String.t()}
  def applyImageFeatures(self, non_edge, gradient_direction, gradient_magnitude) when (is_struct(non_edge, Evision.Mat) or is_struct(non_edge, Nx.Tensor) or is_number(non_edge) or is_tuple(non_edge)) and (is_struct(gradient_direction, Evision.Mat) or is_struct(gradient_direction, Nx.Tensor) or is_number(gradient_direction) or is_tuple(gradient_direction)) and (is_struct(gradient_magnitude, Evision.Mat) or is_struct(gradient_magnitude, Nx.Tensor) or is_number(gradient_magnitude) or is_tuple(gradient_magnitude))
  do
    positional = [
      non_edge: Evision.Internal.Structurise.from_struct(non_edge),
      gradient_direction: Evision.Internal.Structurise.from_struct(gradient_direction),
      gradient_magnitude: Evision.Internal.Structurise.from_struct(gradient_magnitude)
    ]
    :evision_nif.segmentation_segmentation_IntelligentScissorsMB_applyImageFeatures(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Prepares a map of optimal paths for the given source point on the image

  ##### Positional Arguments
  - **self**: `Evision.Segmentation.IntelligentScissorsMB.t()`
  - **sourcePt**: `Point`.

    The source point used to find the paths

  **Note**: applyImage() / applyImageFeatures() must be called before this call

  Python prototype (for reference only):
  ```python3
  buildMap(sourcePt) -> None
  ```
  """
  @spec buildMap(Evision.Segmentation.IntelligentScissorsMB.t(), {number(), number()}) :: Evision.Segmentation.IntelligentScissorsMB.t() | {:error, String.t()}
  def buildMap(self, sourcePt) when is_tuple(sourcePt)
  do
    positional = [
      sourcePt: Evision.Internal.Structurise.from_struct(sourcePt)
    ]
    :evision_nif.segmentation_segmentation_IntelligentScissorsMB_buildMap(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Extracts optimal contour for the given target point on the image

  ##### Positional Arguments
  - **self**: `Evision.Segmentation.IntelligentScissorsMB.t()`
  - **targetPt**: `Point`.

    The target point

  ##### Keyword Arguments
  - **backward**: `bool`.

    Flag to indicate reverse order of retrived pixels (use "true" value to fetch points from the target to the source point)

  ##### Return
  - **contour**: `Evision.Mat.t()`.

    The list of pixels which contains optimal path between the source and the target points of the image. Type is CV_32SC2 (compatible with `std::vector<Point>`)

  **Note**: buildMap() must be called before this call

  Python prototype (for reference only):
  ```python3
  getContour(targetPt[, contour[, backward]]) -> contour
  ```
  """
  @spec getContour(Evision.Segmentation.IntelligentScissorsMB.t(), {number(), number()}, [{:backward, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def getContour(self, targetPt, opts) when is_tuple(targetPt) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:backward])
    positional = [
      targetPt: Evision.Internal.Structurise.from_struct(targetPt)
    ]
    :evision_nif.segmentation_segmentation_IntelligentScissorsMB_getContour(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Extracts optimal contour for the given target point on the image

  ##### Positional Arguments
  - **self**: `Evision.Segmentation.IntelligentScissorsMB.t()`
  - **targetPt**: `Point`.

    The target point

  ##### Keyword Arguments
  - **backward**: `bool`.

    Flag to indicate reverse order of retrived pixels (use "true" value to fetch points from the target to the source point)

  ##### Return
  - **contour**: `Evision.Mat.t()`.

    The list of pixels which contains optimal path between the source and the target points of the image. Type is CV_32SC2 (compatible with `std::vector<Point>`)

  **Note**: buildMap() must be called before this call

  Python prototype (for reference only):
  ```python3
  getContour(targetPt[, contour[, backward]]) -> contour
  ```
  """
  @spec getContour(Evision.Segmentation.IntelligentScissorsMB.t(), {number(), number()}) :: Evision.Mat.t() | {:error, String.t()}
  def getContour(self, targetPt) when is_tuple(targetPt)
  do
    positional = [
      targetPt: Evision.Internal.Structurise.from_struct(targetPt)
    ]
    :evision_nif.segmentation_segmentation_IntelligentScissorsMB_getContour(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Switch edge feature extractor to use Canny edge detector

  ##### Positional Arguments
  - **self**: `Evision.Segmentation.IntelligentScissorsMB.t()`
  - **threshold1**: `double`
  - **threshold2**: `double`

  ##### Keyword Arguments
  - **apertureSize**: `integer()`.
  - **l2gradient**: `bool`.

  ##### Return
  - **retval**: `Evision.Segmentation.IntelligentScissorsMB.t()`

  **Note**: "Laplacian Zero-Crossing" feature extractor is used by default (following to original article)
  @sa Canny

  Python prototype (for reference only):
  ```python3
  setEdgeFeatureCannyParameters(threshold1, threshold2[, apertureSize[, L2gradient]]) -> retval
  ```
  """
  @spec setEdgeFeatureCannyParameters(Evision.Segmentation.IntelligentScissorsMB.t(), number(), number(), [{:apertureSize, term()} | {:l2gradient, term()}] | nil) :: Evision.Segmentation.IntelligentScissorsMB.t() | {:error, String.t()}
  def setEdgeFeatureCannyParameters(self, threshold1, threshold2, opts) when is_number(threshold1) and is_number(threshold2) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:apertureSize, :l2gradient])
    positional = [
      threshold1: Evision.Internal.Structurise.from_struct(threshold1),
      threshold2: Evision.Internal.Structurise.from_struct(threshold2)
    ]
    :evision_nif.segmentation_segmentation_IntelligentScissorsMB_setEdgeFeatureCannyParameters(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Switch edge feature extractor to use Canny edge detector

  ##### Positional Arguments
  - **self**: `Evision.Segmentation.IntelligentScissorsMB.t()`
  - **threshold1**: `double`
  - **threshold2**: `double`

  ##### Keyword Arguments
  - **apertureSize**: `integer()`.
  - **l2gradient**: `bool`.

  ##### Return
  - **retval**: `Evision.Segmentation.IntelligentScissorsMB.t()`

  **Note**: "Laplacian Zero-Crossing" feature extractor is used by default (following to original article)
  @sa Canny

  Python prototype (for reference only):
  ```python3
  setEdgeFeatureCannyParameters(threshold1, threshold2[, apertureSize[, L2gradient]]) -> retval
  ```
  """
  @spec setEdgeFeatureCannyParameters(Evision.Segmentation.IntelligentScissorsMB.t(), number(), number()) :: Evision.Segmentation.IntelligentScissorsMB.t() | {:error, String.t()}
  def setEdgeFeatureCannyParameters(self, threshold1, threshold2) when is_number(threshold1) and is_number(threshold2)
  do
    positional = [
      threshold1: Evision.Internal.Structurise.from_struct(threshold1),
      threshold2: Evision.Internal.Structurise.from_struct(threshold2)
    ]
    :evision_nif.segmentation_segmentation_IntelligentScissorsMB_setEdgeFeatureCannyParameters(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Switch to "Laplacian Zero-Crossing" edge feature extractor and specify its parameters

  ##### Positional Arguments
  - **self**: `Evision.Segmentation.IntelligentScissorsMB.t()`

  ##### Keyword Arguments
  - **gradient_magnitude_min_value**: `float`.

    Minimal gradient magnitude value for edge pixels (default: 0, check is disabled)

  ##### Return
  - **retval**: `Evision.Segmentation.IntelligentScissorsMB.t()`

   This feature extractor is used by default according to article.
   Implementation has additional filtering for regions with low-amplitude noise.
   This filtering is enabled through parameter of minimal gradient amplitude (use some small value 4, 8, 16).
  **Note**: Current implementation of this feature extractor is based on processing of grayscale images (color image is converted to grayscale image first).
  **Note**: Canny edge detector is a bit slower, but provides better results (especially on color images): use setEdgeFeatureCannyParameters().

  Python prototype (for reference only):
  ```python3
  setEdgeFeatureZeroCrossingParameters([, gradient_magnitude_min_value]) -> retval
  ```
  """
  @spec setEdgeFeatureZeroCrossingParameters(Evision.Segmentation.IntelligentScissorsMB.t(), [{:gradient_magnitude_min_value, term()}] | nil) :: Evision.Segmentation.IntelligentScissorsMB.t() | {:error, String.t()}
  def setEdgeFeatureZeroCrossingParameters(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:gradient_magnitude_min_value])
    positional = [
    ]
    :evision_nif.segmentation_segmentation_IntelligentScissorsMB_setEdgeFeatureZeroCrossingParameters(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Switch to "Laplacian Zero-Crossing" edge feature extractor and specify its parameters

  ##### Positional Arguments
  - **self**: `Evision.Segmentation.IntelligentScissorsMB.t()`

  ##### Keyword Arguments
  - **gradient_magnitude_min_value**: `float`.

    Minimal gradient magnitude value for edge pixels (default: 0, check is disabled)

  ##### Return
  - **retval**: `Evision.Segmentation.IntelligentScissorsMB.t()`

   This feature extractor is used by default according to article.
   Implementation has additional filtering for regions with low-amplitude noise.
   This filtering is enabled through parameter of minimal gradient amplitude (use some small value 4, 8, 16).
  **Note**: Current implementation of this feature extractor is based on processing of grayscale images (color image is converted to grayscale image first).
  **Note**: Canny edge detector is a bit slower, but provides better results (especially on color images): use setEdgeFeatureCannyParameters().

  Python prototype (for reference only):
  ```python3
  setEdgeFeatureZeroCrossingParameters([, gradient_magnitude_min_value]) -> retval
  ```
  """
  @spec setEdgeFeatureZeroCrossingParameters(Evision.Segmentation.IntelligentScissorsMB.t()) :: Evision.Segmentation.IntelligentScissorsMB.t() | {:error, String.t()}
  def setEdgeFeatureZeroCrossingParameters(self) do
    positional = [
    ]
    :evision_nif.segmentation_segmentation_IntelligentScissorsMB_setEdgeFeatureZeroCrossingParameters(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Specify gradient magnitude max value threshold

  ##### Positional Arguments
  - **self**: `Evision.Segmentation.IntelligentScissorsMB.t()`

  ##### Keyword Arguments
  - **gradient_magnitude_threshold_max**: `float`.

    Specify gradient magnitude max value threshold (default: 0, disabled)

  ##### Return
  - **retval**: `Evision.Segmentation.IntelligentScissorsMB.t()`

   Zero limit value is used to disable gradient magnitude thresholding (default behavior, as described in original article).
   Otherwize pixels with `gradient magnitude >= threshold` have zero cost.
  **Note**: Thresholding should be used for images with irregular regions (to avoid stuck on parameters from high-contract areas, like embedded logos).

  Python prototype (for reference only):
  ```python3
  setGradientMagnitudeMaxLimit([, gradient_magnitude_threshold_max]) -> retval
  ```
  """
  @spec setGradientMagnitudeMaxLimit(Evision.Segmentation.IntelligentScissorsMB.t(), [{:gradient_magnitude_threshold_max, term()}] | nil) :: Evision.Segmentation.IntelligentScissorsMB.t() | {:error, String.t()}
  def setGradientMagnitudeMaxLimit(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:gradient_magnitude_threshold_max])
    positional = [
    ]
    :evision_nif.segmentation_segmentation_IntelligentScissorsMB_setGradientMagnitudeMaxLimit(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Specify gradient magnitude max value threshold

  ##### Positional Arguments
  - **self**: `Evision.Segmentation.IntelligentScissorsMB.t()`

  ##### Keyword Arguments
  - **gradient_magnitude_threshold_max**: `float`.

    Specify gradient magnitude max value threshold (default: 0, disabled)

  ##### Return
  - **retval**: `Evision.Segmentation.IntelligentScissorsMB.t()`

   Zero limit value is used to disable gradient magnitude thresholding (default behavior, as described in original article).
   Otherwize pixels with `gradient magnitude >= threshold` have zero cost.
  **Note**: Thresholding should be used for images with irregular regions (to avoid stuck on parameters from high-contract areas, like embedded logos).

  Python prototype (for reference only):
  ```python3
  setGradientMagnitudeMaxLimit([, gradient_magnitude_threshold_max]) -> retval
  ```
  """
  @spec setGradientMagnitudeMaxLimit(Evision.Segmentation.IntelligentScissorsMB.t()) :: Evision.Segmentation.IntelligentScissorsMB.t() | {:error, String.t()}
  def setGradientMagnitudeMaxLimit(self) do
    positional = [
    ]
    :evision_nif.segmentation_segmentation_IntelligentScissorsMB_setGradientMagnitudeMaxLimit(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Specify weights of feature functions

  ##### Positional Arguments
  - **self**: `Evision.Segmentation.IntelligentScissorsMB.t()`
  - **weight_non_edge**: `float`.

    Specify cost of non-edge pixels (default: 0.43f)

  - **weight_gradient_direction**: `float`.

    Specify cost of gradient direction function (default: 0.43f)

  - **weight_gradient_magnitude**: `float`.

    Specify cost of gradient magnitude function (default: 0.14f)

  ##### Return
  - **retval**: `Evision.Segmentation.IntelligentScissorsMB.t()`

   Consider keeping weights normalized (sum of weights equals to 1.0)
   Discrete dynamic programming (DP) goal is minimization of costs between pixels.

  Python prototype (for reference only):
  ```python3
  setWeights(weight_non_edge, weight_gradient_direction, weight_gradient_magnitude) -> retval
  ```
  """
  @spec setWeights(Evision.Segmentation.IntelligentScissorsMB.t(), number(), number(), number()) :: Evision.Segmentation.IntelligentScissorsMB.t() | {:error, String.t()}
  def setWeights(self, weight_non_edge, weight_gradient_direction, weight_gradient_magnitude) when is_float(weight_non_edge) and is_float(weight_gradient_direction) and is_float(weight_gradient_magnitude)
  do
    positional = [
      weight_non_edge: Evision.Internal.Structurise.from_struct(weight_non_edge),
      weight_gradient_direction: Evision.Internal.Structurise.from_struct(weight_gradient_direction),
      weight_gradient_magnitude: Evision.Internal.Structurise.from_struct(weight_gradient_magnitude)
    ]
    :evision_nif.segmentation_segmentation_IntelligentScissorsMB_setWeights(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
