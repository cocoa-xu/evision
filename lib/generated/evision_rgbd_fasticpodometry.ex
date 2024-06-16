defmodule Evision.RGBD.FastICPOdometry do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `RGBD.FastICPOdometry` struct.

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
  def to_struct({:ok, %{class: Evision.RGBD.FastICPOdometry, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.RGBD.FastICPOdometry, ref: ref}) do
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

  ##### Positional Arguments
  - **cameraMatrix**: `Evision.Mat`.

    Camera matrix

  ##### Keyword Arguments
  - **maxDistDiff**: `float`.

    Correspondences between pixels of two given frames will be filtered out
    if their depth difference is larger than maxDepthDiff

  - **angleThreshold**: `float`.

    Correspondence will be filtered out
    if an angle between their normals is bigger than threshold

  - **sigmaDepth**: `float`.

    Depth sigma in meters for bilateral smooth

  - **sigmaSpatial**: `float`.

    Spatial sigma in pixels for bilateral smooth

  - **kernelSize**: `integer()`.

    Kernel size in pixels for bilateral smooth

  - **iterCounts**: `[integer()]`.

    Count of iterations on each pyramid level

  ##### Return
  - **retval**: `FastICPOdometry`

  Constructor.

  Python prototype (for reference only):
  ```python3
  create(cameraMatrix[, maxDistDiff[, angleThreshold[, sigmaDepth[, sigmaSpatial[, kernelSize[, iterCounts]]]]]]) -> retval
  ```
  """
  @spec create(Evision.Mat.maybe_mat_in(), [{:angleThreshold, term()} | {:iterCounts, term()} | {:kernelSize, term()} | {:maxDistDiff, term()} | {:sigmaDepth, term()} | {:sigmaSpatial, term()}] | nil) :: Evision.RGBD.FastICPOdometry.t() | {:error, String.t()}
  def create(cameraMatrix, opts) when (is_struct(cameraMatrix, Evision.Mat) or is_struct(cameraMatrix, Nx.Tensor) or is_number(cameraMatrix) or is_tuple(cameraMatrix)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:angleThreshold, :iterCounts, :kernelSize, :maxDistDiff, :sigmaDepth, :sigmaSpatial])
    positional = [
      cameraMatrix: Evision.Internal.Structurise.from_struct(cameraMatrix)
    ]
    :evision_nif.rgbd_rgbd_FastICPOdometry_create_static(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  create

  ##### Positional Arguments
  - **cameraMatrix**: `Evision.Mat`.

    Camera matrix

  ##### Keyword Arguments
  - **maxDistDiff**: `float`.

    Correspondences between pixels of two given frames will be filtered out
    if their depth difference is larger than maxDepthDiff

  - **angleThreshold**: `float`.

    Correspondence will be filtered out
    if an angle between their normals is bigger than threshold

  - **sigmaDepth**: `float`.

    Depth sigma in meters for bilateral smooth

  - **sigmaSpatial**: `float`.

    Spatial sigma in pixels for bilateral smooth

  - **kernelSize**: `integer()`.

    Kernel size in pixels for bilateral smooth

  - **iterCounts**: `[integer()]`.

    Count of iterations on each pyramid level

  ##### Return
  - **retval**: `FastICPOdometry`

  Constructor.

  Python prototype (for reference only):
  ```python3
  create(cameraMatrix[, maxDistDiff[, angleThreshold[, sigmaDepth[, sigmaSpatial[, kernelSize[, iterCounts]]]]]]) -> retval
  ```
  """
  @spec create(Evision.Mat.maybe_mat_in()) :: Evision.RGBD.FastICPOdometry.t() | {:error, String.t()}
  def create(cameraMatrix) when (is_struct(cameraMatrix, Evision.Mat) or is_struct(cameraMatrix, Nx.Tensor) or is_number(cameraMatrix) or is_tuple(cameraMatrix))
  do
    positional = [
      cameraMatrix: Evision.Internal.Structurise.from_struct(cameraMatrix)
    ]
    :evision_nif.rgbd_rgbd_FastICPOdometry_create_static(positional)
    |> to_struct()
  end

  @doc """
  getAngleThreshold

  ##### Positional Arguments
  - **self**: `Evision.RGBD.FastICPOdometry.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  getAngleThreshold() -> retval
  ```
  """
  @spec getAngleThreshold(Evision.RGBD.FastICPOdometry.t()) :: number() | {:error, String.t()}
  def getAngleThreshold(self) do
    positional = [
    ]
    :evision_nif.rgbd_rgbd_FastICPOdometry_getAngleThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getCameraMatrix

  ##### Positional Arguments
  - **self**: `Evision.RGBD.FastICPOdometry.t()`

  ##### Return
  - **retval**: `Evision.Mat.t()`

  Python prototype (for reference only):
  ```python3
  getCameraMatrix() -> retval
  ```
  """
  @spec getCameraMatrix(Evision.RGBD.FastICPOdometry.t()) :: Evision.Mat.t() | {:error, String.t()}
  def getCameraMatrix(self) do
    positional = [
    ]
    :evision_nif.rgbd_rgbd_FastICPOdometry_getCameraMatrix(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getIterationCounts

  ##### Positional Arguments
  - **self**: `Evision.RGBD.FastICPOdometry.t()`

  ##### Return
  - **retval**: `Evision.Mat.t()`

  Python prototype (for reference only):
  ```python3
  getIterationCounts() -> retval
  ```
  """
  @spec getIterationCounts(Evision.RGBD.FastICPOdometry.t()) :: Evision.Mat.t() | {:error, String.t()}
  def getIterationCounts(self) do
    positional = [
    ]
    :evision_nif.rgbd_rgbd_FastICPOdometry_getIterationCounts(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getKernelSize

  ##### Positional Arguments
  - **self**: `Evision.RGBD.FastICPOdometry.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getKernelSize() -> retval
  ```
  """
  @spec getKernelSize(Evision.RGBD.FastICPOdometry.t()) :: integer() | {:error, String.t()}
  def getKernelSize(self) do
    positional = [
    ]
    :evision_nif.rgbd_rgbd_FastICPOdometry_getKernelSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMaxDistDiff

  ##### Positional Arguments
  - **self**: `Evision.RGBD.FastICPOdometry.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getMaxDistDiff() -> retval
  ```
  """
  @spec getMaxDistDiff(Evision.RGBD.FastICPOdometry.t()) :: number() | {:error, String.t()}
  def getMaxDistDiff(self) do
    positional = [
    ]
    :evision_nif.rgbd_rgbd_FastICPOdometry_getMaxDistDiff(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getSigmaDepth

  ##### Positional Arguments
  - **self**: `Evision.RGBD.FastICPOdometry.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  getSigmaDepth() -> retval
  ```
  """
  @spec getSigmaDepth(Evision.RGBD.FastICPOdometry.t()) :: number() | {:error, String.t()}
  def getSigmaDepth(self) do
    positional = [
    ]
    :evision_nif.rgbd_rgbd_FastICPOdometry_getSigmaDepth(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getSigmaSpatial

  ##### Positional Arguments
  - **self**: `Evision.RGBD.FastICPOdometry.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  getSigmaSpatial() -> retval
  ```
  """
  @spec getSigmaSpatial(Evision.RGBD.FastICPOdometry.t()) :: number() | {:error, String.t()}
  def getSigmaSpatial(self) do
    positional = [
    ]
    :evision_nif.rgbd_rgbd_FastICPOdometry_getSigmaSpatial(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getTransformType

  ##### Positional Arguments
  - **self**: `Evision.RGBD.FastICPOdometry.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getTransformType() -> retval
  ```
  """
  @spec getTransformType(Evision.RGBD.FastICPOdometry.t()) :: integer() | {:error, String.t()}
  def getTransformType(self) do
    positional = [
    ]
    :evision_nif.rgbd_rgbd_FastICPOdometry_getTransformType(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  prepareFrameCache

  ##### Positional Arguments
  - **self**: `Evision.RGBD.FastICPOdometry.t()`
  - **frame**: `OdometryFrame`
  - **cacheType**: `integer()`

  ##### Return
  - **retval**: `Size`

  Python prototype (for reference only):
  ```python3
  prepareFrameCache(frame, cacheType) -> retval
  ```
  """
  @spec prepareFrameCache(Evision.RGBD.FastICPOdometry.t(), Evision.RGBD.OdometryFrame.t(), integer()) :: {number(), number()} | {:error, String.t()}
  def prepareFrameCache(self, frame, cacheType) when is_struct(frame, Evision.RGBD.OdometryFrame) and is_integer(cacheType)
  do
    positional = [
      frame: Evision.Internal.Structurise.from_struct(frame),
      cacheType: Evision.Internal.Structurise.from_struct(cacheType)
    ]
    :evision_nif.rgbd_rgbd_FastICPOdometry_prepareFrameCache(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setAngleThreshold

  ##### Positional Arguments
  - **self**: `Evision.RGBD.FastICPOdometry.t()`
  - **f**: `float`

  Python prototype (for reference only):
  ```python3
  setAngleThreshold(f) -> None
  ```
  """
  @spec setAngleThreshold(Evision.RGBD.FastICPOdometry.t(), number()) :: Evision.RGBD.FastICPOdometry.t() | {:error, String.t()}
  def setAngleThreshold(self, f) when is_float(f)
  do
    positional = [
      f: Evision.Internal.Structurise.from_struct(f)
    ]
    :evision_nif.rgbd_rgbd_FastICPOdometry_setAngleThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setCameraMatrix

  ##### Positional Arguments
  - **self**: `Evision.RGBD.FastICPOdometry.t()`
  - **val**: `Evision.Mat`

  Python prototype (for reference only):
  ```python3
  setCameraMatrix(val) -> None
  ```
  """
  @spec setCameraMatrix(Evision.RGBD.FastICPOdometry.t(), Evision.Mat.maybe_mat_in()) :: Evision.RGBD.FastICPOdometry.t() | {:error, String.t()}
  def setCameraMatrix(self, val) when (is_struct(val, Evision.Mat) or is_struct(val, Nx.Tensor) or is_number(val) or is_tuple(val))
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.rgbd_rgbd_FastICPOdometry_setCameraMatrix(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setIterationCounts

  ##### Positional Arguments
  - **self**: `Evision.RGBD.FastICPOdometry.t()`
  - **val**: `Evision.Mat`

  Python prototype (for reference only):
  ```python3
  setIterationCounts(val) -> None
  ```
  """
  @spec setIterationCounts(Evision.RGBD.FastICPOdometry.t(), Evision.Mat.maybe_mat_in()) :: Evision.RGBD.FastICPOdometry.t() | {:error, String.t()}
  def setIterationCounts(self, val) when (is_struct(val, Evision.Mat) or is_struct(val, Nx.Tensor) or is_number(val) or is_tuple(val))
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.rgbd_rgbd_FastICPOdometry_setIterationCounts(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setKernelSize

  ##### Positional Arguments
  - **self**: `Evision.RGBD.FastICPOdometry.t()`
  - **f**: `integer()`

  Python prototype (for reference only):
  ```python3
  setKernelSize(f) -> None
  ```
  """
  @spec setKernelSize(Evision.RGBD.FastICPOdometry.t(), integer()) :: Evision.RGBD.FastICPOdometry.t() | {:error, String.t()}
  def setKernelSize(self, f) when is_integer(f)
  do
    positional = [
      f: Evision.Internal.Structurise.from_struct(f)
    ]
    :evision_nif.rgbd_rgbd_FastICPOdometry_setKernelSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMaxDistDiff

  ##### Positional Arguments
  - **self**: `Evision.RGBD.FastICPOdometry.t()`
  - **val**: `float`

  Python prototype (for reference only):
  ```python3
  setMaxDistDiff(val) -> None
  ```
  """
  @spec setMaxDistDiff(Evision.RGBD.FastICPOdometry.t(), number()) :: Evision.RGBD.FastICPOdometry.t() | {:error, String.t()}
  def setMaxDistDiff(self, val) when is_float(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.rgbd_rgbd_FastICPOdometry_setMaxDistDiff(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setSigmaDepth

  ##### Positional Arguments
  - **self**: `Evision.RGBD.FastICPOdometry.t()`
  - **f**: `float`

  Python prototype (for reference only):
  ```python3
  setSigmaDepth(f) -> None
  ```
  """
  @spec setSigmaDepth(Evision.RGBD.FastICPOdometry.t(), number()) :: Evision.RGBD.FastICPOdometry.t() | {:error, String.t()}
  def setSigmaDepth(self, f) when is_float(f)
  do
    positional = [
      f: Evision.Internal.Structurise.from_struct(f)
    ]
    :evision_nif.rgbd_rgbd_FastICPOdometry_setSigmaDepth(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setSigmaSpatial

  ##### Positional Arguments
  - **self**: `Evision.RGBD.FastICPOdometry.t()`
  - **f**: `float`

  Python prototype (for reference only):
  ```python3
  setSigmaSpatial(f) -> None
  ```
  """
  @spec setSigmaSpatial(Evision.RGBD.FastICPOdometry.t(), number()) :: Evision.RGBD.FastICPOdometry.t() | {:error, String.t()}
  def setSigmaSpatial(self, f) when is_float(f)
  do
    positional = [
      f: Evision.Internal.Structurise.from_struct(f)
    ]
    :evision_nif.rgbd_rgbd_FastICPOdometry_setSigmaSpatial(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setTransformType

  ##### Positional Arguments
  - **self**: `Evision.RGBD.FastICPOdometry.t()`
  - **val**: `integer()`

  Python prototype (for reference only):
  ```python3
  setTransformType(val) -> None
  ```
  """
  @spec setTransformType(Evision.RGBD.FastICPOdometry.t(), integer()) :: Evision.RGBD.FastICPOdometry.t() | {:error, String.t()}
  def setTransformType(self, val) when is_integer(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.rgbd_rgbd_FastICPOdometry_setTransformType(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
