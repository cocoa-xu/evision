defmodule Evision.RGBD.RgbdICPOdometry do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `RGBD.RgbdICPOdometry` struct.

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
  def to_struct({:ok, %{class: Evision.RGBD.RgbdICPOdometry, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.RGBD.RgbdICPOdometry, ref: ref}) do
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
  create
  ##### Keyword Arguments
  - **cameraMatrix**: `Evision.Mat`.

    Camera matrix

  - **minDepth**: `float`.

    Pixels with depth less than minDepth will not be used

  - **maxDepth**: `float`.

    Pixels with depth larger than maxDepth will not be used

  - **maxDepthDiff**: `float`.

    Correspondences between pixels of two given frames will be filtered out
    if their depth difference is larger than maxDepthDiff

  - **maxPointsPart**: `float`.

    The method uses a random pixels subset of size frameWidth x frameHeight x pointsPart

  - **iterCounts**: `[integer()]`.

    Count of iterations on each pyramid level.

  - **minGradientMagnitudes**: `[float]`.

    For each pyramid level the pixels will be filtered out
    if they have gradient magnitude less than minGradientMagnitudes[level].

  - **transformType**: `integer()`.

    Class of trasformation

  ##### Return
  - **retval**: `RgbdICPOdometry`

  Constructor.

  Python prototype (for reference only):
  ```python3
  create([, cameraMatrix[, minDepth[, maxDepth[, maxDepthDiff[, maxPointsPart[, iterCounts[, minGradientMagnitudes[, transformType]]]]]]]]) -> retval
  ```
  """
  @spec create([{:cameraMatrix, term()} | {:iterCounts, term()} | {:maxDepth, term()} | {:maxDepthDiff, term()} | {:maxPointsPart, term()} | {:minDepth, term()} | {:minGradientMagnitudes, term()} | {:transformType, term()}] | nil) :: Evision.RGBD.RgbdICPOdometry.t() | {:error, String.t()}
  def create(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:cameraMatrix, :iterCounts, :maxDepth, :maxDepthDiff, :maxPointsPart, :minDepth, :minGradientMagnitudes, :transformType])
    positional = [
    ]
    :evision_nif.rgbd_rgbd_RgbdICPOdometry_create_static(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  create
  ##### Keyword Arguments
  - **cameraMatrix**: `Evision.Mat`.

    Camera matrix

  - **minDepth**: `float`.

    Pixels with depth less than minDepth will not be used

  - **maxDepth**: `float`.

    Pixels with depth larger than maxDepth will not be used

  - **maxDepthDiff**: `float`.

    Correspondences between pixels of two given frames will be filtered out
    if their depth difference is larger than maxDepthDiff

  - **maxPointsPart**: `float`.

    The method uses a random pixels subset of size frameWidth x frameHeight x pointsPart

  - **iterCounts**: `[integer()]`.

    Count of iterations on each pyramid level.

  - **minGradientMagnitudes**: `[float]`.

    For each pyramid level the pixels will be filtered out
    if they have gradient magnitude less than minGradientMagnitudes[level].

  - **transformType**: `integer()`.

    Class of trasformation

  ##### Return
  - **retval**: `RgbdICPOdometry`

  Constructor.

  Python prototype (for reference only):
  ```python3
  create([, cameraMatrix[, minDepth[, maxDepth[, maxDepthDiff[, maxPointsPart[, iterCounts[, minGradientMagnitudes[, transformType]]]]]]]]) -> retval
  ```
  """
  @spec create() :: Evision.RGBD.RgbdICPOdometry.t() | {:error, String.t()}
  def create() do
    positional = [
    ]
    :evision_nif.rgbd_rgbd_RgbdICPOdometry_create_static(positional)
    |> to_struct()
  end

  @doc """
  getCameraMatrix

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdICPOdometry.t()`

  ##### Return
  - **retval**: `Evision.Mat.t()`

  Python prototype (for reference only):
  ```python3
  getCameraMatrix() -> retval
  ```
  """
  @spec getCameraMatrix(Evision.RGBD.RgbdICPOdometry.t()) :: Evision.Mat.t() | {:error, String.t()}
  def getCameraMatrix(self) do
    positional = [
    ]
    :evision_nif.rgbd_rgbd_RgbdICPOdometry_getCameraMatrix(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getIterationCounts

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdICPOdometry.t()`

  ##### Return
  - **retval**: `Evision.Mat.t()`

  Python prototype (for reference only):
  ```python3
  getIterationCounts() -> retval
  ```
  """
  @spec getIterationCounts(Evision.RGBD.RgbdICPOdometry.t()) :: Evision.Mat.t() | {:error, String.t()}
  def getIterationCounts(self) do
    positional = [
    ]
    :evision_nif.rgbd_rgbd_RgbdICPOdometry_getIterationCounts(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMaxDepth

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdICPOdometry.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getMaxDepth() -> retval
  ```
  """
  @spec getMaxDepth(Evision.RGBD.RgbdICPOdometry.t()) :: number() | {:error, String.t()}
  def getMaxDepth(self) do
    positional = [
    ]
    :evision_nif.rgbd_rgbd_RgbdICPOdometry_getMaxDepth(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMaxDepthDiff

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdICPOdometry.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getMaxDepthDiff() -> retval
  ```
  """
  @spec getMaxDepthDiff(Evision.RGBD.RgbdICPOdometry.t()) :: number() | {:error, String.t()}
  def getMaxDepthDiff(self) do
    positional = [
    ]
    :evision_nif.rgbd_rgbd_RgbdICPOdometry_getMaxDepthDiff(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMaxPointsPart

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdICPOdometry.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getMaxPointsPart() -> retval
  ```
  """
  @spec getMaxPointsPart(Evision.RGBD.RgbdICPOdometry.t()) :: number() | {:error, String.t()}
  def getMaxPointsPart(self) do
    positional = [
    ]
    :evision_nif.rgbd_rgbd_RgbdICPOdometry_getMaxPointsPart(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMaxRotation

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdICPOdometry.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getMaxRotation() -> retval
  ```
  """
  @spec getMaxRotation(Evision.RGBD.RgbdICPOdometry.t()) :: number() | {:error, String.t()}
  def getMaxRotation(self) do
    positional = [
    ]
    :evision_nif.rgbd_rgbd_RgbdICPOdometry_getMaxRotation(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMaxTranslation

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdICPOdometry.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getMaxTranslation() -> retval
  ```
  """
  @spec getMaxTranslation(Evision.RGBD.RgbdICPOdometry.t()) :: number() | {:error, String.t()}
  def getMaxTranslation(self) do
    positional = [
    ]
    :evision_nif.rgbd_rgbd_RgbdICPOdometry_getMaxTranslation(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMinDepth

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdICPOdometry.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getMinDepth() -> retval
  ```
  """
  @spec getMinDepth(Evision.RGBD.RgbdICPOdometry.t()) :: number() | {:error, String.t()}
  def getMinDepth(self) do
    positional = [
    ]
    :evision_nif.rgbd_rgbd_RgbdICPOdometry_getMinDepth(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMinGradientMagnitudes

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdICPOdometry.t()`

  ##### Return
  - **retval**: `Evision.Mat.t()`

  Python prototype (for reference only):
  ```python3
  getMinGradientMagnitudes() -> retval
  ```
  """
  @spec getMinGradientMagnitudes(Evision.RGBD.RgbdICPOdometry.t()) :: Evision.Mat.t() | {:error, String.t()}
  def getMinGradientMagnitudes(self) do
    positional = [
    ]
    :evision_nif.rgbd_rgbd_RgbdICPOdometry_getMinGradientMagnitudes(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getNormalsComputer

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdICPOdometry.t()`

  ##### Return
  - **retval**: `RgbdNormals`

  Python prototype (for reference only):
  ```python3
  getNormalsComputer() -> retval
  ```
  """
  @spec getNormalsComputer(Evision.RGBD.RgbdICPOdometry.t()) :: Evision.RGBD.RgbdNormals.t() | {:error, String.t()}
  def getNormalsComputer(self) do
    positional = [
    ]
    :evision_nif.rgbd_rgbd_RgbdICPOdometry_getNormalsComputer(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getTransformType

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdICPOdometry.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getTransformType() -> retval
  ```
  """
  @spec getTransformType(Evision.RGBD.RgbdICPOdometry.t()) :: integer() | {:error, String.t()}
  def getTransformType(self) do
    positional = [
    ]
    :evision_nif.rgbd_rgbd_RgbdICPOdometry_getTransformType(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  prepareFrameCache

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdICPOdometry.t()`
  - **frame**: `OdometryFrame`
  - **cacheType**: `integer()`

  ##### Return
  - **retval**: `Size`

  Python prototype (for reference only):
  ```python3
  prepareFrameCache(frame, cacheType) -> retval
  ```
  """
  @spec prepareFrameCache(Evision.RGBD.RgbdICPOdometry.t(), Evision.RGBD.OdometryFrame.t(), integer()) :: {number(), number()} | {:error, String.t()}
  def prepareFrameCache(self, frame, cacheType) when is_struct(frame, Evision.RGBD.OdometryFrame) and is_integer(cacheType)
  do
    positional = [
      frame: Evision.Internal.Structurise.from_struct(frame),
      cacheType: Evision.Internal.Structurise.from_struct(cacheType)
    ]
    :evision_nif.rgbd_rgbd_RgbdICPOdometry_prepareFrameCache(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setCameraMatrix

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdICPOdometry.t()`
  - **val**: `Evision.Mat`

  Python prototype (for reference only):
  ```python3
  setCameraMatrix(val) -> None
  ```
  """
  @spec setCameraMatrix(Evision.RGBD.RgbdICPOdometry.t(), Evision.Mat.maybe_mat_in()) :: Evision.RGBD.RgbdICPOdometry.t() | {:error, String.t()}
  def setCameraMatrix(self, val) when (is_struct(val, Evision.Mat) or is_struct(val, Nx.Tensor) or is_number(val) or is_tuple(val))
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.rgbd_rgbd_RgbdICPOdometry_setCameraMatrix(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setIterationCounts

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdICPOdometry.t()`
  - **val**: `Evision.Mat`

  Python prototype (for reference only):
  ```python3
  setIterationCounts(val) -> None
  ```
  """
  @spec setIterationCounts(Evision.RGBD.RgbdICPOdometry.t(), Evision.Mat.maybe_mat_in()) :: Evision.RGBD.RgbdICPOdometry.t() | {:error, String.t()}
  def setIterationCounts(self, val) when (is_struct(val, Evision.Mat) or is_struct(val, Nx.Tensor) or is_number(val) or is_tuple(val))
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.rgbd_rgbd_RgbdICPOdometry_setIterationCounts(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMaxDepth

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdICPOdometry.t()`
  - **val**: `double`

  Python prototype (for reference only):
  ```python3
  setMaxDepth(val) -> None
  ```
  """
  @spec setMaxDepth(Evision.RGBD.RgbdICPOdometry.t(), number()) :: Evision.RGBD.RgbdICPOdometry.t() | {:error, String.t()}
  def setMaxDepth(self, val) when is_number(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.rgbd_rgbd_RgbdICPOdometry_setMaxDepth(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMaxDepthDiff

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdICPOdometry.t()`
  - **val**: `double`

  Python prototype (for reference only):
  ```python3
  setMaxDepthDiff(val) -> None
  ```
  """
  @spec setMaxDepthDiff(Evision.RGBD.RgbdICPOdometry.t(), number()) :: Evision.RGBD.RgbdICPOdometry.t() | {:error, String.t()}
  def setMaxDepthDiff(self, val) when is_number(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.rgbd_rgbd_RgbdICPOdometry_setMaxDepthDiff(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMaxPointsPart

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdICPOdometry.t()`
  - **val**: `double`

  Python prototype (for reference only):
  ```python3
  setMaxPointsPart(val) -> None
  ```
  """
  @spec setMaxPointsPart(Evision.RGBD.RgbdICPOdometry.t(), number()) :: Evision.RGBD.RgbdICPOdometry.t() | {:error, String.t()}
  def setMaxPointsPart(self, val) when is_number(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.rgbd_rgbd_RgbdICPOdometry_setMaxPointsPart(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMaxRotation

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdICPOdometry.t()`
  - **val**: `double`

  Python prototype (for reference only):
  ```python3
  setMaxRotation(val) -> None
  ```
  """
  @spec setMaxRotation(Evision.RGBD.RgbdICPOdometry.t(), number()) :: Evision.RGBD.RgbdICPOdometry.t() | {:error, String.t()}
  def setMaxRotation(self, val) when is_number(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.rgbd_rgbd_RgbdICPOdometry_setMaxRotation(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMaxTranslation

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdICPOdometry.t()`
  - **val**: `double`

  Python prototype (for reference only):
  ```python3
  setMaxTranslation(val) -> None
  ```
  """
  @spec setMaxTranslation(Evision.RGBD.RgbdICPOdometry.t(), number()) :: Evision.RGBD.RgbdICPOdometry.t() | {:error, String.t()}
  def setMaxTranslation(self, val) when is_number(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.rgbd_rgbd_RgbdICPOdometry_setMaxTranslation(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMinDepth

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdICPOdometry.t()`
  - **val**: `double`

  Python prototype (for reference only):
  ```python3
  setMinDepth(val) -> None
  ```
  """
  @spec setMinDepth(Evision.RGBD.RgbdICPOdometry.t(), number()) :: Evision.RGBD.RgbdICPOdometry.t() | {:error, String.t()}
  def setMinDepth(self, val) when is_number(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.rgbd_rgbd_RgbdICPOdometry_setMinDepth(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMinGradientMagnitudes

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdICPOdometry.t()`
  - **val**: `Evision.Mat`

  Python prototype (for reference only):
  ```python3
  setMinGradientMagnitudes(val) -> None
  ```
  """
  @spec setMinGradientMagnitudes(Evision.RGBD.RgbdICPOdometry.t(), Evision.Mat.maybe_mat_in()) :: Evision.RGBD.RgbdICPOdometry.t() | {:error, String.t()}
  def setMinGradientMagnitudes(self, val) when (is_struct(val, Evision.Mat) or is_struct(val, Nx.Tensor) or is_number(val) or is_tuple(val))
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.rgbd_rgbd_RgbdICPOdometry_setMinGradientMagnitudes(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setTransformType

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdICPOdometry.t()`
  - **val**: `integer()`

  Python prototype (for reference only):
  ```python3
  setTransformType(val) -> None
  ```
  """
  @spec setTransformType(Evision.RGBD.RgbdICPOdometry.t(), integer()) :: Evision.RGBD.RgbdICPOdometry.t() | {:error, String.t()}
  def setTransformType(self, val) when is_integer(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.rgbd_rgbd_RgbdICPOdometry_setTransformType(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
