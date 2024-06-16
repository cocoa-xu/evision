defmodule Evision.RGBD.RgbdPlane do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `RGBD.RgbdPlane` struct.

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
  def to_struct({:ok, %{class: Evision.RGBD.RgbdPlane, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.RGBD.RgbdPlane, ref: ref}) do
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
  apply

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdPlane.t()`
  - **points3d**: `Evision.Mat`.

    the 3d points organized like the depth image: rows x cols with 3 channels

  - **normals**: `Evision.Mat`.

    the normals for every point in the depth image

  ##### Return
  - **mask**: `Evision.Mat.t()`.

    An image where each pixel is labeled with the plane it belongs to
    and 255 if it does not belong to any plane

  - **plane_coefficients**: `Evision.Mat.t()`.

    the coefficients of the corresponding planes (a,b,c,d) such that ax+by+cz+d=0, norm(a,b,c)=1
    and c < 0 (so that the normal points towards the camera)

  Find The planes in a depth image

  Python prototype (for reference only):
  ```python3
  apply(points3d, normals[, mask[, plane_coefficients]]) -> mask, plane_coefficients
  ```
  """
  @spec apply(Evision.RGBD.RgbdPlane.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: {Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def apply(self, points3d, normals, opts) when (is_struct(points3d, Evision.Mat) or is_struct(points3d, Nx.Tensor) or is_number(points3d) or is_tuple(points3d)) and (is_struct(normals, Evision.Mat) or is_struct(normals, Nx.Tensor) or is_number(normals) or is_tuple(normals)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      points3d: Evision.Internal.Structurise.from_struct(points3d),
      normals: Evision.Internal.Structurise.from_struct(normals)
    ]
    :evision_nif.rgbd_rgbd_RgbdPlane_apply(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  apply

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdPlane.t()`
  - **points3d**: `Evision.Mat`.

    the 3d points organized like the depth image: rows x cols with 3 channels

  - **normals**: `Evision.Mat`.

    the normals for every point in the depth image

  ##### Return
  - **mask**: `Evision.Mat.t()`.

    An image where each pixel is labeled with the plane it belongs to
    and 255 if it does not belong to any plane

  - **plane_coefficients**: `Evision.Mat.t()`.

    the coefficients of the corresponding planes (a,b,c,d) such that ax+by+cz+d=0, norm(a,b,c)=1
    and c < 0 (so that the normal points towards the camera)

  Find The planes in a depth image

  Python prototype (for reference only):
  ```python3
  apply(points3d, normals[, mask[, plane_coefficients]]) -> mask, plane_coefficients
  ```
  #### Variant 2:
  apply

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdPlane.t()`
  - **points3d**: `Evision.Mat`.

    the 3d points organized like the depth image: rows x cols with 3 channels

  ##### Return
  - **mask**: `Evision.Mat.t()`.

    An image where each pixel is labeled with the plane it belongs to
    and 255 if it does not belong to any plane

  - **plane_coefficients**: `Evision.Mat.t()`.

    the coefficients of the corresponding planes (a,b,c,d) such that ax+by+cz+d=0

  Find The planes in a depth image but without doing a normal check, which is faster but less accurate

  Python prototype (for reference only):
  ```python3
  apply(points3d[, mask[, plane_coefficients]]) -> mask, plane_coefficients
  ```

  """
  @spec apply(Evision.RGBD.RgbdPlane.t(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: {Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def apply(self, points3d, opts) when (is_struct(points3d, Evision.Mat) or is_struct(points3d, Nx.Tensor) or is_number(points3d) or is_tuple(points3d)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      points3d: Evision.Internal.Structurise.from_struct(points3d)
    ]
    :evision_nif.rgbd_rgbd_RgbdPlane_apply(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec apply(Evision.RGBD.RgbdPlane.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: {Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def apply(self, points3d, normals) when (is_struct(points3d, Evision.Mat) or is_struct(points3d, Nx.Tensor) or is_number(points3d) or is_tuple(points3d)) and (is_struct(normals, Evision.Mat) or is_struct(normals, Nx.Tensor) or is_number(normals) or is_tuple(normals))
  do
    positional = [
      points3d: Evision.Internal.Structurise.from_struct(points3d),
      normals: Evision.Internal.Structurise.from_struct(normals)
    ]
    :evision_nif.rgbd_rgbd_RgbdPlane_apply(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  apply

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdPlane.t()`
  - **points3d**: `Evision.Mat`.

    the 3d points organized like the depth image: rows x cols with 3 channels

  ##### Return
  - **mask**: `Evision.Mat.t()`.

    An image where each pixel is labeled with the plane it belongs to
    and 255 if it does not belong to any plane

  - **plane_coefficients**: `Evision.Mat.t()`.

    the coefficients of the corresponding planes (a,b,c,d) such that ax+by+cz+d=0

  Find The planes in a depth image but without doing a normal check, which is faster but less accurate

  Python prototype (for reference only):
  ```python3
  apply(points3d[, mask[, plane_coefficients]]) -> mask, plane_coefficients
  ```
  """
  @spec apply(Evision.RGBD.RgbdPlane.t(), Evision.Mat.maybe_mat_in()) :: {Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def apply(self, points3d) when (is_struct(points3d, Evision.Mat) or is_struct(points3d, Nx.Tensor) or is_number(points3d) or is_tuple(points3d))
  do
    positional = [
      points3d: Evision.Internal.Structurise.from_struct(points3d)
    ]
    :evision_nif.rgbd_rgbd_RgbdPlane_apply(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Clears the algorithm state

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdPlane.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.RGBD.RgbdPlane.t()) :: Evision.RGBD.RgbdPlane.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.rgbd_RgbdPlane_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  create

  ##### Positional Arguments
  - **method**: `integer()`.

    The method to use to compute the planes.

  - **block_size**: `integer()`.

    The size of the blocks to look at for a stable MSE

  - **min_size**: `integer()`.

    The minimum size of a cluster to be considered a plane

  - **threshold**: `double`.

    The maximum distance of a point from a plane to belong to it (in meters)

  ##### Keyword Arguments
  - **sensor_error_a**: `double`.

    coefficient of the sensor error. 0 by default, 0.0075 for a Kinect

  - **sensor_error_b**: `double`.

    coefficient of the sensor error. 0 by default

  - **sensor_error_c**: `double`.

    coefficient of the sensor error. 0 by default

  ##### Return
  - **retval**: `RgbdPlane`

  Constructor

  Python prototype (for reference only):
  ```python3
  create(method, block_size, min_size, threshold[, sensor_error_a[, sensor_error_b[, sensor_error_c]]]) -> retval
  ```
  """
  @spec create(integer(), integer(), integer(), number(), [{:sensor_error_a, term()} | {:sensor_error_b, term()} | {:sensor_error_c, term()}] | nil) :: Evision.RGBD.RgbdPlane.t() | {:error, String.t()}
  def create(method, block_size, min_size, threshold, opts) when is_integer(method) and is_integer(block_size) and is_integer(min_size) and is_number(threshold) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:sensor_error_a, :sensor_error_b, :sensor_error_c])
    positional = [
      method: Evision.Internal.Structurise.from_struct(method),
      block_size: Evision.Internal.Structurise.from_struct(block_size),
      min_size: Evision.Internal.Structurise.from_struct(min_size),
      threshold: Evision.Internal.Structurise.from_struct(threshold)
    ]
    :evision_nif.rgbd_rgbd_RgbdPlane_create_static(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  create

  ##### Positional Arguments
  - **method**: `integer()`.

    The method to use to compute the planes.

  - **block_size**: `integer()`.

    The size of the blocks to look at for a stable MSE

  - **min_size**: `integer()`.

    The minimum size of a cluster to be considered a plane

  - **threshold**: `double`.

    The maximum distance of a point from a plane to belong to it (in meters)

  ##### Keyword Arguments
  - **sensor_error_a**: `double`.

    coefficient of the sensor error. 0 by default, 0.0075 for a Kinect

  - **sensor_error_b**: `double`.

    coefficient of the sensor error. 0 by default

  - **sensor_error_c**: `double`.

    coefficient of the sensor error. 0 by default

  ##### Return
  - **retval**: `RgbdPlane`

  Constructor

  Python prototype (for reference only):
  ```python3
  create(method, block_size, min_size, threshold[, sensor_error_a[, sensor_error_b[, sensor_error_c]]]) -> retval
  ```
  """
  @spec create(integer(), integer(), integer(), number()) :: Evision.RGBD.RgbdPlane.t() | {:error, String.t()}
  def create(method, block_size, min_size, threshold) when is_integer(method) and is_integer(block_size) and is_integer(min_size) and is_number(threshold)
  do
    positional = [
      method: Evision.Internal.Structurise.from_struct(method),
      block_size: Evision.Internal.Structurise.from_struct(block_size),
      min_size: Evision.Internal.Structurise.from_struct(min_size),
      threshold: Evision.Internal.Structurise.from_struct(threshold)
    ]
    :evision_nif.rgbd_rgbd_RgbdPlane_create_static(positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdPlane.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.RGBD.RgbdPlane.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.rgbd_RgbdPlane_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getBlockSize

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdPlane.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getBlockSize() -> retval
  ```
  """
  @spec getBlockSize(Evision.RGBD.RgbdPlane.t()) :: integer() | {:error, String.t()}
  def getBlockSize(self) do
    positional = [
    ]
    :evision_nif.rgbd_rgbd_RgbdPlane_getBlockSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdPlane.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.RGBD.RgbdPlane.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.rgbd_RgbdPlane_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMethod

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdPlane.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getMethod() -> retval
  ```
  """
  @spec getMethod(Evision.RGBD.RgbdPlane.t()) :: integer() | {:error, String.t()}
  def getMethod(self) do
    positional = [
    ]
    :evision_nif.rgbd_rgbd_RgbdPlane_getMethod(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMinSize

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdPlane.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getMinSize() -> retval
  ```
  """
  @spec getMinSize(Evision.RGBD.RgbdPlane.t()) :: integer() | {:error, String.t()}
  def getMinSize(self) do
    positional = [
    ]
    :evision_nif.rgbd_rgbd_RgbdPlane_getMinSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getSensorErrorA

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdPlane.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getSensorErrorA() -> retval
  ```
  """
  @spec getSensorErrorA(Evision.RGBD.RgbdPlane.t()) :: number() | {:error, String.t()}
  def getSensorErrorA(self) do
    positional = [
    ]
    :evision_nif.rgbd_rgbd_RgbdPlane_getSensorErrorA(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getSensorErrorB

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdPlane.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getSensorErrorB() -> retval
  ```
  """
  @spec getSensorErrorB(Evision.RGBD.RgbdPlane.t()) :: number() | {:error, String.t()}
  def getSensorErrorB(self) do
    positional = [
    ]
    :evision_nif.rgbd_rgbd_RgbdPlane_getSensorErrorB(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getSensorErrorC

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdPlane.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getSensorErrorC() -> retval
  ```
  """
  @spec getSensorErrorC(Evision.RGBD.RgbdPlane.t()) :: number() | {:error, String.t()}
  def getSensorErrorC(self) do
    positional = [
    ]
    :evision_nif.rgbd_rgbd_RgbdPlane_getSensorErrorC(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getThreshold

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdPlane.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getThreshold() -> retval
  ```
  """
  @spec getThreshold(Evision.RGBD.RgbdPlane.t()) :: number() | {:error, String.t()}
  def getThreshold(self) do
    positional = [
    ]
    :evision_nif.rgbd_rgbd_RgbdPlane_getThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdPlane.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.RGBD.RgbdPlane.t(), Evision.FileNode.t()) :: Evision.RGBD.RgbdPlane.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.rgbd_RgbdPlane_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdPlane.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.RGBD.RgbdPlane.t(), binary()) :: Evision.RGBD.RgbdPlane.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.rgbd_RgbdPlane_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setBlockSize

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdPlane.t()`
  - **val**: `integer()`

  Python prototype (for reference only):
  ```python3
  setBlockSize(val) -> None
  ```
  """
  @spec setBlockSize(Evision.RGBD.RgbdPlane.t(), integer()) :: Evision.RGBD.RgbdPlane.t() | {:error, String.t()}
  def setBlockSize(self, val) when is_integer(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.rgbd_rgbd_RgbdPlane_setBlockSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMethod

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdPlane.t()`
  - **val**: `integer()`

  Python prototype (for reference only):
  ```python3
  setMethod(val) -> None
  ```
  """
  @spec setMethod(Evision.RGBD.RgbdPlane.t(), integer()) :: Evision.RGBD.RgbdPlane.t() | {:error, String.t()}
  def setMethod(self, val) when is_integer(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.rgbd_rgbd_RgbdPlane_setMethod(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMinSize

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdPlane.t()`
  - **val**: `integer()`

  Python prototype (for reference only):
  ```python3
  setMinSize(val) -> None
  ```
  """
  @spec setMinSize(Evision.RGBD.RgbdPlane.t(), integer()) :: Evision.RGBD.RgbdPlane.t() | {:error, String.t()}
  def setMinSize(self, val) when is_integer(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.rgbd_rgbd_RgbdPlane_setMinSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setSensorErrorA

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdPlane.t()`
  - **val**: `double`

  Python prototype (for reference only):
  ```python3
  setSensorErrorA(val) -> None
  ```
  """
  @spec setSensorErrorA(Evision.RGBD.RgbdPlane.t(), number()) :: Evision.RGBD.RgbdPlane.t() | {:error, String.t()}
  def setSensorErrorA(self, val) when is_number(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.rgbd_rgbd_RgbdPlane_setSensorErrorA(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setSensorErrorB

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdPlane.t()`
  - **val**: `double`

  Python prototype (for reference only):
  ```python3
  setSensorErrorB(val) -> None
  ```
  """
  @spec setSensorErrorB(Evision.RGBD.RgbdPlane.t(), number()) :: Evision.RGBD.RgbdPlane.t() | {:error, String.t()}
  def setSensorErrorB(self, val) when is_number(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.rgbd_rgbd_RgbdPlane_setSensorErrorB(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setSensorErrorC

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdPlane.t()`
  - **val**: `double`

  Python prototype (for reference only):
  ```python3
  setSensorErrorC(val) -> None
  ```
  """
  @spec setSensorErrorC(Evision.RGBD.RgbdPlane.t(), number()) :: Evision.RGBD.RgbdPlane.t() | {:error, String.t()}
  def setSensorErrorC(self, val) when is_number(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.rgbd_rgbd_RgbdPlane_setSensorErrorC(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setThreshold

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdPlane.t()`
  - **val**: `double`

  Python prototype (for reference only):
  ```python3
  setThreshold(val) -> None
  ```
  """
  @spec setThreshold(Evision.RGBD.RgbdPlane.t(), number()) :: Evision.RGBD.RgbdPlane.t() | {:error, String.t()}
  def setThreshold(self, val) when is_number(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.rgbd_rgbd_RgbdPlane_setThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdPlane.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.RGBD.RgbdPlane.t(), Evision.FileStorage.t(), binary()) :: Evision.RGBD.RgbdPlane.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.rgbd_RgbdPlane_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdPlane.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.RGBD.RgbdPlane.t(), Evision.FileStorage.t()) :: Evision.RGBD.RgbdPlane.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.rgbd_RgbdPlane_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
