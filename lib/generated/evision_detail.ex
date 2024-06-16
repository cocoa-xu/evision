defmodule Evision.Detail do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Detail` struct.

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
  def to_struct({:ok, %{class: Evision.Detail, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Detail, ref: ref}) do
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
  Estimates focal lengths for each given camera.

  ##### Positional Arguments
  - **hs**: `[Evision.Mat]`

  ##### Return
  - **retval**: `bool`
  - **k**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  calibrateRotatingCamera(Hs[, K]) -> retval, K
  ```
  """
  @spec calibrateRotatingCamera(list(Evision.Mat.maybe_mat_in()), [{atom(), term()},...] | nil) :: Evision.Mat.t() | false | {:error, String.t()}
  def calibrateRotatingCamera(hs, opts) when is_list(hs) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      hs: Evision.Internal.Structurise.from_struct(hs)
    ]
    :evision_nif.detail_calibrateRotatingCamera(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Estimates focal lengths for each given camera.

  ##### Positional Arguments
  - **hs**: `[Evision.Mat]`

  ##### Return
  - **retval**: `bool`
  - **k**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  calibrateRotatingCamera(Hs[, K]) -> retval, K
  ```
  """
  @spec calibrateRotatingCamera(list(Evision.Mat.maybe_mat_in())) :: Evision.Mat.t() | false | {:error, String.t()}
  def calibrateRotatingCamera(hs) when is_list(hs)
  do
    positional = [
      hs: Evision.Internal.Structurise.from_struct(hs)
    ]
    :evision_nif.detail_calibrateRotatingCamera(positional)
    |> to_struct()
  end

  @doc """
  
  ##### Positional Arguments
  - **featuresFinder**: `Evision.Feature2D`.

  - **images**: `[Evision.Mat]`.

  ##### Keyword Arguments
  - **masks**: `[Evision.Mat]`.

  ##### Return
  - **features**: `[Evision.Detail.ImageFeatures]`.

  Python prototype (for reference only):
  ```python3
  computeImageFeatures(featuresFinder, images[, masks]) -> features
  ```
  """
  @spec computeImageFeatures(Evision.Feature2D.t(), list(Evision.Mat.maybe_mat_in()), [{:masks, term()}] | nil) :: list(Evision.Detail.ImageFeatures.t()) | {:error, String.t()}
  def computeImageFeatures(featuresFinder, images, opts) when is_list(images) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:masks])
    positional = [
      featuresFinder: Evision.Internal.Structurise.from_struct(featuresFinder),
      images: Evision.Internal.Structurise.from_struct(images)
    ]
    :evision_nif.detail_computeImageFeatures(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  
  ##### Positional Arguments
  - **featuresFinder**: `Evision.Feature2D`.

  - **images**: `[Evision.Mat]`.

  ##### Keyword Arguments
  - **masks**: `[Evision.Mat]`.

  ##### Return
  - **features**: `[Evision.Detail.ImageFeatures]`.

  Python prototype (for reference only):
  ```python3
  computeImageFeatures(featuresFinder, images[, masks]) -> features
  ```
  """
  @spec computeImageFeatures(Evision.Feature2D.t(), list(Evision.Mat.maybe_mat_in())) :: list(Evision.Detail.ImageFeatures.t()) | {:error, String.t()}
  def computeImageFeatures(featuresFinder, images) when is_list(images)
  do
    positional = [
      featuresFinder: Evision.Internal.Structurise.from_struct(featuresFinder),
      images: Evision.Internal.Structurise.from_struct(images)
    ]
    :evision_nif.detail_computeImageFeatures(positional)
    |> to_struct()
  end

  @doc """
  
  ##### Positional Arguments
  - **featuresFinder**: `Evision.Feature2D`.

  - **image**: `Evision.Mat`.

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.

  ##### Return
  - **features**: `Evision.Detail.ImageFeatures.t()`.

  Python prototype (for reference only):
  ```python3
  computeImageFeatures2(featuresFinder, image[, mask]) -> features
  ```
  """
  @spec computeImageFeatures2(Evision.Feature2D.t(), Evision.Mat.maybe_mat_in(), [{:mask, term()}] | nil) :: Evision.Detail.ImageFeatures.t() | {:error, String.t()}
  def computeImageFeatures2(featuresFinder, image, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:mask])
    positional = [
      featuresFinder: Evision.Internal.Structurise.from_struct(featuresFinder),
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.detail_computeImageFeatures2(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  
  ##### Positional Arguments
  - **featuresFinder**: `Evision.Feature2D`.

  - **image**: `Evision.Mat`.

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.

  ##### Return
  - **features**: `Evision.Detail.ImageFeatures.t()`.

  Python prototype (for reference only):
  ```python3
  computeImageFeatures2(featuresFinder, image[, mask]) -> features
  ```
  """
  @spec computeImageFeatures2(Evision.Feature2D.t(), Evision.Mat.maybe_mat_in()) :: Evision.Detail.ImageFeatures.t() | {:error, String.t()}
  def computeImageFeatures2(featuresFinder, image) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image))
  do
    positional = [
      featuresFinder: Evision.Internal.Structurise.from_struct(featuresFinder),
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.detail_computeImageFeatures2(positional)
    |> to_struct()
  end

  @doc """
  createLaplacePyr

  ##### Positional Arguments
  - **img**: `Evision.Mat`
  - **num_levels**: `integer()`

  ##### Return
  - **pyr**: `[Evision.Mat]`

  Python prototype (for reference only):
  ```python3
  createLaplacePyr(img, num_levels, pyr) -> pyr
  ```
  """
  @spec createLaplacePyr(Evision.Mat.maybe_mat_in(), integer(), list(Evision.Mat.maybe_mat_in())) :: list(Evision.Mat.t()) | {:error, String.t()}
  def createLaplacePyr(img, num_levels, pyr) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img)) and is_integer(num_levels) and is_list(pyr)
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img),
      num_levels: Evision.Internal.Structurise.from_struct(num_levels),
      pyr: Evision.Internal.Structurise.from_struct(pyr)
    ]
    :evision_nif.detail_createLaplacePyr(positional)
    |> to_struct()
  end

  @doc """
  createLaplacePyrGpu

  ##### Positional Arguments
  - **img**: `Evision.Mat`
  - **num_levels**: `integer()`

  ##### Return
  - **pyr**: `[Evision.Mat]`

  Python prototype (for reference only):
  ```python3
  createLaplacePyrGpu(img, num_levels, pyr) -> pyr
  ```
  """
  @spec createLaplacePyrGpu(Evision.Mat.maybe_mat_in(), integer(), list(Evision.Mat.maybe_mat_in())) :: list(Evision.Mat.t()) | {:error, String.t()}
  def createLaplacePyrGpu(img, num_levels, pyr) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img)) and is_integer(num_levels) and is_list(pyr)
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img),
      num_levels: Evision.Internal.Structurise.from_struct(num_levels),
      pyr: Evision.Internal.Structurise.from_struct(pyr)
    ]
    :evision_nif.detail_createLaplacePyrGpu(positional)
    |> to_struct()
  end

  @doc """
  createWeightMap

  ##### Positional Arguments
  - **mask**: `Evision.Mat`
  - **sharpness**: `float`

  ##### Return
  - **weight**: `Evision.Mat.t()`

  Python prototype (for reference only):
  ```python3
  createWeightMap(mask, sharpness, weight) -> weight
  ```
  """
  @spec createWeightMap(Evision.Mat.maybe_mat_in(), number(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def createWeightMap(mask, sharpness, weight) when (is_struct(mask, Evision.Mat) or is_struct(mask, Nx.Tensor) or is_number(mask) or is_tuple(mask)) and is_float(sharpness) and (is_struct(weight, Evision.Mat) or is_struct(weight, Nx.Tensor) or is_number(weight) or is_tuple(weight))
  do
    positional = [
      mask: Evision.Internal.Structurise.from_struct(mask),
      sharpness: Evision.Internal.Structurise.from_struct(sharpness),
      weight: Evision.Internal.Structurise.from_struct(weight)
    ]
    :evision_nif.detail_createWeightMap(positional)
    |> to_struct()
  end

  @doc """
  Tries to estimate focal lengths from the given homography under the assumption that the camera
  undergoes rotations around its centre only.

  ##### Positional Arguments
  - **h**: `Evision.Mat`.

    Homography.

  - **f0**: `double`.

    Estimated focal length along X axis.

  - **f1**: `double`.

    Estimated focal length along Y axis.

  - **f0_ok**: `bool`.

    True, if f0 was estimated successfully, false otherwise.

  - **f1_ok**: `bool`.

    True, if f1 was estimated successfully, false otherwise.

  See "Construction of Panoramic Image Mosaics with Global and Local Alignment"
  by Heung-Yeung Shum and Richard Szeliski.

  Python prototype (for reference only):
  ```python3
  focalsFromHomography(H, f0, f1, f0_ok, f1_ok) -> None
  ```
  """
  @spec focalsFromHomography(Evision.Mat.maybe_mat_in(), number(), number(), boolean(), boolean()) :: :ok | {:error, String.t()}
  def focalsFromHomography(h, f0, f1, f0_ok, f1_ok) when (is_struct(h, Evision.Mat) or is_struct(h, Nx.Tensor) or is_number(h) or is_tuple(h)) and is_number(f0) and is_number(f1) and is_boolean(f0_ok) and is_boolean(f1_ok)
  do
    positional = [
      h: Evision.Internal.Structurise.from_struct(h),
      f0: Evision.Internal.Structurise.from_struct(f0),
      f1: Evision.Internal.Structurise.from_struct(f1),
      f0_ok: Evision.Internal.Structurise.from_struct(f0_ok),
      f1_ok: Evision.Internal.Structurise.from_struct(f1_ok)
    ]
    :evision_nif.detail_focalsFromHomography(positional)
    |> to_struct()
  end

  @doc """
  leaveBiggestComponent

  ##### Positional Arguments
  - **features**: `[Evision.Detail.ImageFeatures]`
  - **pairwise_matches**: `[Evision.Detail.MatchesInfo]`
  - **conf_threshold**: `float`

  ##### Return
  - **retval**: `[integer()]`

  Python prototype (for reference only):
  ```python3
  leaveBiggestComponent(features, pairwise_matches, conf_threshold) -> retval
  ```
  """
  @spec leaveBiggestComponent(list(Evision.Detail.ImageFeatures.t()), list(Evision.Detail.MatchesInfo.t()), number()) :: list(integer()) | {:error, String.t()}
  def leaveBiggestComponent(features, pairwise_matches, conf_threshold) when is_list(features) and is_list(pairwise_matches) and is_float(conf_threshold)
  do
    positional = [
      features: Evision.Internal.Structurise.from_struct(features),
      pairwise_matches: Evision.Internal.Structurise.from_struct(pairwise_matches),
      conf_threshold: Evision.Internal.Structurise.from_struct(conf_threshold)
    ]
    :evision_nif.detail_leaveBiggestComponent(positional)
    |> to_struct()
  end

  @doc """
  matchesGraphAsString

  ##### Positional Arguments
  - **paths**: `[String]`
  - **pairwise_matches**: `[Evision.Detail.MatchesInfo]`
  - **conf_threshold**: `float`

  ##### Return
  - **retval**: `String`

  Python prototype (for reference only):
  ```python3
  matchesGraphAsString(paths, pairwise_matches, conf_threshold) -> retval
  ```
  """
  @spec matchesGraphAsString(list(binary()), list(Evision.Detail.MatchesInfo.t()), number()) :: binary() | {:error, String.t()}
  def matchesGraphAsString(paths, pairwise_matches, conf_threshold) when is_list(paths) and is_list(pairwise_matches) and is_float(conf_threshold)
  do
    positional = [
      paths: Evision.Internal.Structurise.from_struct(paths),
      pairwise_matches: Evision.Internal.Structurise.from_struct(pairwise_matches),
      conf_threshold: Evision.Internal.Structurise.from_struct(conf_threshold)
    ]
    :evision_nif.detail_matchesGraphAsString(positional)
    |> to_struct()
  end

  @doc """
  normalizeUsingWeightMap

  ##### Positional Arguments
  - **weight**: `Evision.Mat`

  ##### Return
  - **src**: `Evision.Mat.t()`

  Python prototype (for reference only):
  ```python3
  normalizeUsingWeightMap(weight, src) -> src
  ```
  """
  @spec normalizeUsingWeightMap(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def normalizeUsingWeightMap(weight, src) when (is_struct(weight, Evision.Mat) or is_struct(weight, Nx.Tensor) or is_number(weight) or is_tuple(weight)) and (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src))
  do
    positional = [
      weight: Evision.Internal.Structurise.from_struct(weight),
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.detail_normalizeUsingWeightMap(positional)
    |> to_struct()
  end

  @doc """
  overlapRoi

  ##### Positional Arguments
  - **tl1**: `Point`
  - **tl2**: `Point`
  - **sz1**: `Size`
  - **sz2**: `Size`
  - **roi**: `Rect`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  overlapRoi(tl1, tl2, sz1, sz2, roi) -> retval
  ```
  """
  @spec overlapRoi({number(), number()}, {number(), number()}, {number(), number()}, {number(), number()}, {number(), number(), number(), number()}) :: boolean() | {:error, String.t()}
  def overlapRoi(tl1, tl2, sz1, sz2, roi) when is_tuple(tl1) and is_tuple(tl2) and is_tuple(sz1) and is_tuple(sz2) and is_tuple(roi)
  do
    positional = [
      tl1: Evision.Internal.Structurise.from_struct(tl1),
      tl2: Evision.Internal.Structurise.from_struct(tl2),
      sz1: Evision.Internal.Structurise.from_struct(sz1),
      sz2: Evision.Internal.Structurise.from_struct(sz2),
      roi: Evision.Internal.Structurise.from_struct(roi)
    ]
    :evision_nif.detail_overlapRoi(positional)
    |> to_struct()
  end

  @doc """
  restoreImageFromLaplacePyr
  ##### Return
  - **pyr**: `[Evision.Mat]`

  Python prototype (for reference only):
  ```python3
  restoreImageFromLaplacePyr(pyr) -> pyr
  ```
  """
  @spec restoreImageFromLaplacePyr(list(Evision.Mat.maybe_mat_in())) :: list(Evision.Mat.t()) | {:error, String.t()}
  def restoreImageFromLaplacePyr(pyr) when is_list(pyr)
  do
    positional = [
      pyr: Evision.Internal.Structurise.from_struct(pyr)
    ]
    :evision_nif.detail_restoreImageFromLaplacePyr(positional)
    |> to_struct()
  end

  @doc """
  restoreImageFromLaplacePyrGpu
  ##### Return
  - **pyr**: `[Evision.Mat]`

  Python prototype (for reference only):
  ```python3
  restoreImageFromLaplacePyrGpu(pyr) -> pyr
  ```
  """
  @spec restoreImageFromLaplacePyrGpu(list(Evision.Mat.maybe_mat_in())) :: list(Evision.Mat.t()) | {:error, String.t()}
  def restoreImageFromLaplacePyrGpu(pyr) when is_list(pyr)
  do
    positional = [
      pyr: Evision.Internal.Structurise.from_struct(pyr)
    ]
    :evision_nif.detail_restoreImageFromLaplacePyrGpu(positional)
    |> to_struct()
  end

  @doc """
  resultRoi

  ##### Positional Arguments
  - **corners**: `[Point]`
  - **sizes**: `[Size]`

  ##### Return
  - **retval**: `Rect`

  Python prototype (for reference only):
  ```python3
  resultRoi(corners, sizes) -> retval
  ```
  """
  @spec resultRoi(list({number(), number()}), list({number(), number()})) :: {number(), number(), number(), number()} | {:error, String.t()}
  def resultRoi(corners, sizes) when is_list(corners) and is_list(sizes)
  do
    positional = [
      corners: Evision.Internal.Structurise.from_struct(corners),
      sizes: Evision.Internal.Structurise.from_struct(sizes)
    ]
    :evision_nif.detail_resultRoi(positional)
    |> to_struct()
  end

  @doc """
  resultRoiIntersection

  ##### Positional Arguments
  - **corners**: `[Point]`
  - **sizes**: `[Size]`

  ##### Return
  - **retval**: `Rect`

  Python prototype (for reference only):
  ```python3
  resultRoiIntersection(corners, sizes) -> retval
  ```
  """
  @spec resultRoiIntersection(list({number(), number()}), list({number(), number()})) :: {number(), number(), number(), number()} | {:error, String.t()}
  def resultRoiIntersection(corners, sizes) when is_list(corners) and is_list(sizes)
  do
    positional = [
      corners: Evision.Internal.Structurise.from_struct(corners),
      sizes: Evision.Internal.Structurise.from_struct(sizes)
    ]
    :evision_nif.detail_resultRoiIntersection(positional)
    |> to_struct()
  end

  @doc """
  resultTl

  ##### Positional Arguments
  - **corners**: `[Point]`

  ##### Return
  - **retval**: `Point`

  Python prototype (for reference only):
  ```python3
  resultTl(corners) -> retval
  ```
  """
  @spec resultTl(list({number(), number()})) :: {number(), number()} | {:error, String.t()}
  def resultTl(corners) when is_list(corners)
  do
    positional = [
      corners: Evision.Internal.Structurise.from_struct(corners)
    ]
    :evision_nif.detail_resultTl(positional)
    |> to_struct()
  end

  @doc """
  selectRandomSubset

  ##### Positional Arguments
  - **count**: `integer()`
  - **size**: `integer()`
  - **subset**: `[integer()]`

  Python prototype (for reference only):
  ```python3
  selectRandomSubset(count, size, subset) -> None
  ```
  """
  @spec selectRandomSubset(integer(), integer(), list(integer())) :: :ok | {:error, String.t()}
  def selectRandomSubset(count, size, subset) when is_integer(count) and is_integer(size) and is_list(subset)
  do
    positional = [
      count: Evision.Internal.Structurise.from_struct(count),
      size: Evision.Internal.Structurise.from_struct(size),
      subset: Evision.Internal.Structurise.from_struct(subset)
    ]
    :evision_nif.detail_selectRandomSubset(positional)
    |> to_struct()
  end

  @doc """
  stitchingLogLevel
  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  stitchingLogLevel() -> retval
  ```
  """
  @spec stitchingLogLevel() :: integer() | {:error, String.t()}
  def stitchingLogLevel() do
    positional = [
    ]
    :evision_nif.detail_stitchingLogLevel(positional)
    |> to_struct()
  end

  @doc """
  Tries to make panorama more horizontal (or vertical).

  ##### Positional Arguments
  - **kind**: `WaveCorrectKind`.

    Correction kind, see detail::WaveCorrectKind.

  ##### Return
  - **rmats**: `[Evision.Mat]`.

    Camera rotation matrices.

  Python prototype (for reference only):
  ```python3
  waveCorrect(rmats, kind) -> rmats
  ```
  """
  @spec waveCorrect(list(Evision.Mat.maybe_mat_in()), Evision.Detail.WaveCorrectKind.enum()) :: list(Evision.Mat.t()) | {:error, String.t()}
  def waveCorrect(rmats, kind) when is_list(rmats) and is_integer(kind)
  do
    positional = [
      rmats: Evision.Internal.Structurise.from_struct(rmats),
      kind: Evision.Internal.Structurise.from_struct(kind)
    ]
    :evision_nif.detail_waveCorrect(positional)
    |> to_struct()
  end
end
