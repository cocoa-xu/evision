defmodule Evision.RGBD.Odometry do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `RGBD.Odometry` struct.

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
  def to_struct({:ok, %{class: Evision.RGBD.Odometry, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.RGBD.Odometry, ref: ref}) do
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
  @type enum :: integer()
  @doc enum: true
  def cv_ROTATION, do: 1
  @doc enum: true
  def cv_TRANSLATION, do: 2
  @doc enum: true
  def cv_RIGID_BODY_MOTION, do: 4


  @doc """
  DEFAULT_MAX_DEPTH

  ##### Positional Arguments
  - **self**: `Evision.RGBD.Odometry.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  DEFAULT_MAX_DEPTH() -> retval
  ```
  """
  @spec default_max_depth(Evision.RGBD.Odometry.t()) :: number() | {:error, String.t()}
  def default_max_depth(self) do
    positional = [
    ]
    :evision_nif.rgbd_rgbd_Odometry_DEFAULT_MAX_DEPTH(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  DEFAULT_MAX_DEPTH_DIFF

  ##### Positional Arguments
  - **self**: `Evision.RGBD.Odometry.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  DEFAULT_MAX_DEPTH_DIFF() -> retval
  ```
  """
  @spec default_max_depth_diff(Evision.RGBD.Odometry.t()) :: number() | {:error, String.t()}
  def default_max_depth_diff(self) do
    positional = [
    ]
    :evision_nif.rgbd_rgbd_Odometry_DEFAULT_MAX_DEPTH_DIFF(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  DEFAULT_MAX_POINTS_PART

  ##### Positional Arguments
  - **self**: `Evision.RGBD.Odometry.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  DEFAULT_MAX_POINTS_PART() -> retval
  ```
  """
  @spec default_max_points_part(Evision.RGBD.Odometry.t()) :: number() | {:error, String.t()}
  def default_max_points_part(self) do
    positional = [
    ]
    :evision_nif.rgbd_rgbd_Odometry_DEFAULT_MAX_POINTS_PART(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  DEFAULT_MAX_ROTATION

  ##### Positional Arguments
  - **self**: `Evision.RGBD.Odometry.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  DEFAULT_MAX_ROTATION() -> retval
  ```
  """
  @spec default_max_rotation(Evision.RGBD.Odometry.t()) :: number() | {:error, String.t()}
  def default_max_rotation(self) do
    positional = [
    ]
    :evision_nif.rgbd_rgbd_Odometry_DEFAULT_MAX_ROTATION(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  DEFAULT_MAX_TRANSLATION

  ##### Positional Arguments
  - **self**: `Evision.RGBD.Odometry.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  DEFAULT_MAX_TRANSLATION() -> retval
  ```
  """
  @spec default_max_translation(Evision.RGBD.Odometry.t()) :: number() | {:error, String.t()}
  def default_max_translation(self) do
    positional = [
    ]
    :evision_nif.rgbd_rgbd_Odometry_DEFAULT_MAX_TRANSLATION(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  DEFAULT_MIN_DEPTH

  ##### Positional Arguments
  - **self**: `Evision.RGBD.Odometry.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  DEFAULT_MIN_DEPTH() -> retval
  ```
  """
  @spec default_min_depth(Evision.RGBD.Odometry.t()) :: number() | {:error, String.t()}
  def default_min_depth(self) do
    positional = [
    ]
    :evision_nif.rgbd_rgbd_Odometry_DEFAULT_MIN_DEPTH(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Clears the algorithm state

  ##### Positional Arguments
  - **self**: `Evision.RGBD.Odometry.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.RGBD.Odometry.t()) :: Evision.RGBD.Odometry.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.rgbd_Odometry_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  compute

  ##### Positional Arguments
  - **self**: `Evision.RGBD.Odometry.t()`
  - **srcImage**: `Evision.Mat`.

    Image data of the source frame (CV_8UC1)

  - **srcDepth**: `Evision.Mat`.

    Depth data of the source frame (CV_32FC1, in meters)

  - **srcMask**: `Evision.Mat`.

    Mask that sets which pixels have to be used from the source frame (CV_8UC1)

  - **dstImage**: `Evision.Mat`.

    Image data of the destination frame (CV_8UC1)

  - **dstDepth**: `Evision.Mat`.

    Depth data of the destination frame (CV_32FC1, in meters)

  - **dstMask**: `Evision.Mat`.

    Mask that sets which pixels have to be used from the destination frame (CV_8UC1)

  ##### Keyword Arguments
  - **initRt**: `Evision.Mat`.

    Initial transformation from the source frame to the destination one (optional)

  ##### Return
  - **retval**: `bool`
  - **rt**: `Evision.Mat.t()`.

    Resulting transformation from the source frame to the destination one (rigid body motion):
    dst_p = Rt * src_p, where dst_p is a homogeneous point in the destination frame and src_p is
    homogeneous point in the source frame,
    Rt is 4x4 matrix of CV_64FC1 type.

  Method to compute a transformation from the source frame to the destination one.
   Some odometry algorithms do not used some data of frames (eg. ICP does not use images).
   In such case corresponding arguments can be set as empty Mat.
   The method returns true if all internal computations were possible (e.g. there were enough correspondences,
   system of equations has a solution, etc) and resulting transformation satisfies some test if it's provided
   by the Odometry inheritor implementation (e.g. thresholds for maximum translation and rotation).

  Python prototype (for reference only):
  ```python3
  compute(srcImage, srcDepth, srcMask, dstImage, dstDepth, dstMask[, Rt[, initRt]]) -> retval, Rt
  ```
  """
  @spec compute(Evision.RGBD.Odometry.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:initRt, term()}] | nil) :: Evision.Mat.t() | false | {:error, String.t()}
  def compute(self, srcImage, srcDepth, srcMask, dstImage, dstDepth, dstMask, opts) when (is_struct(srcImage, Evision.Mat) or is_struct(srcImage, Nx.Tensor) or is_number(srcImage) or is_tuple(srcImage)) and (is_struct(srcDepth, Evision.Mat) or is_struct(srcDepth, Nx.Tensor) or is_number(srcDepth) or is_tuple(srcDepth)) and (is_struct(srcMask, Evision.Mat) or is_struct(srcMask, Nx.Tensor) or is_number(srcMask) or is_tuple(srcMask)) and (is_struct(dstImage, Evision.Mat) or is_struct(dstImage, Nx.Tensor) or is_number(dstImage) or is_tuple(dstImage)) and (is_struct(dstDepth, Evision.Mat) or is_struct(dstDepth, Nx.Tensor) or is_number(dstDepth) or is_tuple(dstDepth)) and (is_struct(dstMask, Evision.Mat) or is_struct(dstMask, Nx.Tensor) or is_number(dstMask) or is_tuple(dstMask)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:initRt])
    positional = [
      srcImage: Evision.Internal.Structurise.from_struct(srcImage),
      srcDepth: Evision.Internal.Structurise.from_struct(srcDepth),
      srcMask: Evision.Internal.Structurise.from_struct(srcMask),
      dstImage: Evision.Internal.Structurise.from_struct(dstImage),
      dstDepth: Evision.Internal.Structurise.from_struct(dstDepth),
      dstMask: Evision.Internal.Structurise.from_struct(dstMask)
    ]
    :evision_nif.rgbd_rgbd_Odometry_compute(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  compute

  ##### Positional Arguments
  - **self**: `Evision.RGBD.Odometry.t()`
  - **srcImage**: `Evision.Mat`.

    Image data of the source frame (CV_8UC1)

  - **srcDepth**: `Evision.Mat`.

    Depth data of the source frame (CV_32FC1, in meters)

  - **srcMask**: `Evision.Mat`.

    Mask that sets which pixels have to be used from the source frame (CV_8UC1)

  - **dstImage**: `Evision.Mat`.

    Image data of the destination frame (CV_8UC1)

  - **dstDepth**: `Evision.Mat`.

    Depth data of the destination frame (CV_32FC1, in meters)

  - **dstMask**: `Evision.Mat`.

    Mask that sets which pixels have to be used from the destination frame (CV_8UC1)

  ##### Keyword Arguments
  - **initRt**: `Evision.Mat`.

    Initial transformation from the source frame to the destination one (optional)

  ##### Return
  - **retval**: `bool`
  - **rt**: `Evision.Mat.t()`.

    Resulting transformation from the source frame to the destination one (rigid body motion):
    dst_p = Rt * src_p, where dst_p is a homogeneous point in the destination frame and src_p is
    homogeneous point in the source frame,
    Rt is 4x4 matrix of CV_64FC1 type.

  Method to compute a transformation from the source frame to the destination one.
   Some odometry algorithms do not used some data of frames (eg. ICP does not use images).
   In such case corresponding arguments can be set as empty Mat.
   The method returns true if all internal computations were possible (e.g. there were enough correspondences,
   system of equations has a solution, etc) and resulting transformation satisfies some test if it's provided
   by the Odometry inheritor implementation (e.g. thresholds for maximum translation and rotation).

  Python prototype (for reference only):
  ```python3
  compute(srcImage, srcDepth, srcMask, dstImage, dstDepth, dstMask[, Rt[, initRt]]) -> retval, Rt
  ```
  """
  @spec compute(Evision.RGBD.Odometry.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | false | {:error, String.t()}
  def compute(self, srcImage, srcDepth, srcMask, dstImage, dstDepth, dstMask) when (is_struct(srcImage, Evision.Mat) or is_struct(srcImage, Nx.Tensor) or is_number(srcImage) or is_tuple(srcImage)) and (is_struct(srcDepth, Evision.Mat) or is_struct(srcDepth, Nx.Tensor) or is_number(srcDepth) or is_tuple(srcDepth)) and (is_struct(srcMask, Evision.Mat) or is_struct(srcMask, Nx.Tensor) or is_number(srcMask) or is_tuple(srcMask)) and (is_struct(dstImage, Evision.Mat) or is_struct(dstImage, Nx.Tensor) or is_number(dstImage) or is_tuple(dstImage)) and (is_struct(dstDepth, Evision.Mat) or is_struct(dstDepth, Nx.Tensor) or is_number(dstDepth) or is_tuple(dstDepth)) and (is_struct(dstMask, Evision.Mat) or is_struct(dstMask, Nx.Tensor) or is_number(dstMask) or is_tuple(dstMask))
  do
    positional = [
      srcImage: Evision.Internal.Structurise.from_struct(srcImage),
      srcDepth: Evision.Internal.Structurise.from_struct(srcDepth),
      srcMask: Evision.Internal.Structurise.from_struct(srcMask),
      dstImage: Evision.Internal.Structurise.from_struct(dstImage),
      dstDepth: Evision.Internal.Structurise.from_struct(dstDepth),
      dstMask: Evision.Internal.Structurise.from_struct(dstMask)
    ]
    :evision_nif.rgbd_rgbd_Odometry_compute(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  compute2

  ##### Positional Arguments
  - **self**: `Evision.RGBD.Odometry.t()`
  - **srcFrame**: `OdometryFrame`
  - **dstFrame**: `OdometryFrame`

  ##### Keyword Arguments
  - **initRt**: `Evision.Mat`.

  ##### Return
  - **retval**: `bool`
  - **rt**: `Evision.Mat.t()`.

  One more method to compute a transformation from the source frame to the destination one.
   It is designed to save on computing the frame data (image pyramids, normals, etc.).

  Python prototype (for reference only):
  ```python3
  compute2(srcFrame, dstFrame[, Rt[, initRt]]) -> retval, Rt
  ```
  """
  @spec compute2(Evision.RGBD.Odometry.t(), Evision.RGBD.OdometryFrame.t(), Evision.RGBD.OdometryFrame.t(), [{:initRt, term()}] | nil) :: Evision.Mat.t() | false | {:error, String.t()}
  def compute2(self, srcFrame, dstFrame, opts) when is_struct(srcFrame, Evision.RGBD.OdometryFrame) and is_struct(dstFrame, Evision.RGBD.OdometryFrame) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:initRt])
    positional = [
      srcFrame: Evision.Internal.Structurise.from_struct(srcFrame),
      dstFrame: Evision.Internal.Structurise.from_struct(dstFrame)
    ]
    :evision_nif.rgbd_rgbd_Odometry_compute2(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  compute2

  ##### Positional Arguments
  - **self**: `Evision.RGBD.Odometry.t()`
  - **srcFrame**: `OdometryFrame`
  - **dstFrame**: `OdometryFrame`

  ##### Keyword Arguments
  - **initRt**: `Evision.Mat`.

  ##### Return
  - **retval**: `bool`
  - **rt**: `Evision.Mat.t()`.

  One more method to compute a transformation from the source frame to the destination one.
   It is designed to save on computing the frame data (image pyramids, normals, etc.).

  Python prototype (for reference only):
  ```python3
  compute2(srcFrame, dstFrame[, Rt[, initRt]]) -> retval, Rt
  ```
  """
  @spec compute2(Evision.RGBD.Odometry.t(), Evision.RGBD.OdometryFrame.t(), Evision.RGBD.OdometryFrame.t()) :: Evision.Mat.t() | false | {:error, String.t()}
  def compute2(self, srcFrame, dstFrame) when is_struct(srcFrame, Evision.RGBD.OdometryFrame) and is_struct(dstFrame, Evision.RGBD.OdometryFrame)
  do
    positional = [
      srcFrame: Evision.Internal.Structurise.from_struct(srcFrame),
      dstFrame: Evision.Internal.Structurise.from_struct(dstFrame)
    ]
    :evision_nif.rgbd_rgbd_Odometry_compute2(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  create

  ##### Positional Arguments
  - **odometryType**: `String`

  ##### Return
  - **retval**: `Odometry`

  Python prototype (for reference only):
  ```python3
  create(odometryType) -> retval
  ```
  """
  @spec create(binary()) :: Evision.RGBD.Odometry.t() | {:error, String.t()}
  def create(odometryType) when is_binary(odometryType)
  do
    positional = [
      odometryType: Evision.Internal.Structurise.from_struct(odometryType)
    ]
    :evision_nif.rgbd_rgbd_Odometry_create_static(positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.RGBD.Odometry.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.RGBD.Odometry.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.rgbd_Odometry_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getCameraMatrix

  ##### Positional Arguments
  - **self**: `Evision.RGBD.Odometry.t()`

  ##### Return
  - **retval**: `Evision.Mat.t()`

  @see `setCameraMatrix/2`

  Python prototype (for reference only):
  ```python3
  getCameraMatrix() -> retval
  ```
  """
  @spec getCameraMatrix(Evision.RGBD.Odometry.t()) :: Evision.Mat.t() | {:error, String.t()}
  def getCameraMatrix(self) do
    positional = [
    ]
    :evision_nif.rgbd_rgbd_Odometry_getCameraMatrix(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.RGBD.Odometry.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.RGBD.Odometry.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.rgbd_Odometry_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getTransformType

  ##### Positional Arguments
  - **self**: `Evision.RGBD.Odometry.t()`

  ##### Return
  - **retval**: `integer()`

  @see `setTransformType/2`

  Python prototype (for reference only):
  ```python3
  getTransformType() -> retval
  ```
  """
  @spec getTransformType(Evision.RGBD.Odometry.t()) :: integer() | {:error, String.t()}
  def getTransformType(self) do
    positional = [
    ]
    :evision_nif.rgbd_rgbd_Odometry_getTransformType(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  prepareFrameCache

  ##### Positional Arguments
  - **self**: `Evision.RGBD.Odometry.t()`
  - **frame**: `OdometryFrame`.

    The odometry which will process the frame.

  - **cacheType**: `integer()`.

    The cache type: CACHE_SRC, CACHE_DST or CACHE_ALL.

  ##### Return
  - **retval**: `Size`

  Prepare a cache for the frame. The function checks the precomputed/passed data (throws the error if this data
   does not satisfy) and computes all remaining cache data needed for the frame. Returned size is a resolution
   of the prepared frame.

  Python prototype (for reference only):
  ```python3
  prepareFrameCache(frame, cacheType) -> retval
  ```
  """
  @spec prepareFrameCache(Evision.RGBD.Odometry.t(), Evision.RGBD.OdometryFrame.t(), integer()) :: {number(), number()} | {:error, String.t()}
  def prepareFrameCache(self, frame, cacheType) when is_struct(frame, Evision.RGBD.OdometryFrame) and is_integer(cacheType)
  do
    positional = [
      frame: Evision.Internal.Structurise.from_struct(frame),
      cacheType: Evision.Internal.Structurise.from_struct(cacheType)
    ]
    :evision_nif.rgbd_rgbd_Odometry_prepareFrameCache(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.RGBD.Odometry.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.RGBD.Odometry.t(), Evision.FileNode.t()) :: Evision.RGBD.Odometry.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.rgbd_Odometry_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.RGBD.Odometry.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.RGBD.Odometry.t(), binary()) :: Evision.RGBD.Odometry.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.rgbd_Odometry_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setCameraMatrix

  ##### Positional Arguments
  - **self**: `Evision.RGBD.Odometry.t()`
  - **val**: `Evision.Mat`

  @see `getCameraMatrix/1`

  Python prototype (for reference only):
  ```python3
  setCameraMatrix(val) -> None
  ```
  """
  @spec setCameraMatrix(Evision.RGBD.Odometry.t(), Evision.Mat.maybe_mat_in()) :: Evision.RGBD.Odometry.t() | {:error, String.t()}
  def setCameraMatrix(self, val) when (is_struct(val, Evision.Mat) or is_struct(val, Nx.Tensor) or is_number(val) or is_tuple(val))
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.rgbd_rgbd_Odometry_setCameraMatrix(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setTransformType

  ##### Positional Arguments
  - **self**: `Evision.RGBD.Odometry.t()`
  - **val**: `integer()`

  @see `getTransformType/1`

  Python prototype (for reference only):
  ```python3
  setTransformType(val) -> None
  ```
  """
  @spec setTransformType(Evision.RGBD.Odometry.t(), integer()) :: Evision.RGBD.Odometry.t() | {:error, String.t()}
  def setTransformType(self, val) when is_integer(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.rgbd_rgbd_Odometry_setTransformType(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.RGBD.Odometry.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.RGBD.Odometry.t(), Evision.FileStorage.t(), binary()) :: Evision.RGBD.Odometry.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.rgbd_Odometry_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.RGBD.Odometry.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.RGBD.Odometry.t(), Evision.FileStorage.t()) :: Evision.RGBD.Odometry.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.rgbd_Odometry_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
