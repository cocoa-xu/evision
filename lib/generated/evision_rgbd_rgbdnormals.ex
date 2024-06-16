defmodule Evision.RGBD.RgbdNormals do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `RGBD.RgbdNormals` struct.

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
  def to_struct({:ok, %{class: Evision.RGBD.RgbdNormals, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.RGBD.RgbdNormals, ref: ref}) do
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
  - **self**: `Evision.RGBD.RgbdNormals.t()`
  - **points**: `Evision.Mat`.

    a rows x cols x 3 matrix of CV_32F/CV64F or a rows x cols x 1 CV_U16S

  ##### Return
  - **normals**: `Evision.Mat.t()`.

    a rows x cols x 3 matrix

  Given a set of 3d points in a depth image, compute the normals at each point.

  Python prototype (for reference only):
  ```python3
  apply(points[, normals]) -> normals
  ```
  """
  @spec apply(Evision.RGBD.RgbdNormals.t(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def apply(self, points, opts) when (is_struct(points, Evision.Mat) or is_struct(points, Nx.Tensor) or is_number(points) or is_tuple(points)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      points: Evision.Internal.Structurise.from_struct(points)
    ]
    :evision_nif.rgbd_rgbd_RgbdNormals_apply(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  apply

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdNormals.t()`
  - **points**: `Evision.Mat`.

    a rows x cols x 3 matrix of CV_32F/CV64F or a rows x cols x 1 CV_U16S

  ##### Return
  - **normals**: `Evision.Mat.t()`.

    a rows x cols x 3 matrix

  Given a set of 3d points in a depth image, compute the normals at each point.

  Python prototype (for reference only):
  ```python3
  apply(points[, normals]) -> normals
  ```
  """
  @spec apply(Evision.RGBD.RgbdNormals.t(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def apply(self, points) when (is_struct(points, Evision.Mat) or is_struct(points, Nx.Tensor) or is_number(points) or is_tuple(points))
  do
    positional = [
      points: Evision.Internal.Structurise.from_struct(points)
    ]
    :evision_nif.rgbd_rgbd_RgbdNormals_apply(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Clears the algorithm state

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdNormals.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.RGBD.RgbdNormals.t()) :: Evision.RGBD.RgbdNormals.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.rgbd_RgbdNormals_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  create

  ##### Positional Arguments
  - **rows**: `integer()`.

    the number of rows of the depth image normals will be computed on

  - **cols**: `integer()`.

    the number of cols of the depth image normals will be computed on

  - **depth**: `integer()`.

    the depth of the normals (only CV_32F or CV_64F)

  - **k**: `Evision.Mat`.

    the calibration matrix to use

  ##### Keyword Arguments
  - **window_size**: `integer()`.

    the window size to compute the normals: can only be 1,3,5 or 7

  - **method**: `integer()`.

    one of the methods to use: RGBD_NORMALS_METHOD_SRI, RGBD_NORMALS_METHOD_FALS

  ##### Return
  - **retval**: `RgbdNormals`

  Constructor

  Python prototype (for reference only):
  ```python3
  create(rows, cols, depth, K[, window_size[, method]]) -> retval
  ```
  """
  @spec create(integer(), integer(), integer(), Evision.Mat.maybe_mat_in(), [{:method, term()} | {:window_size, term()}] | nil) :: Evision.RGBD.RgbdNormals.t() | {:error, String.t()}
  def create(rows, cols, depth, k, opts) when is_integer(rows) and is_integer(cols) and is_integer(depth) and (is_struct(k, Evision.Mat) or is_struct(k, Nx.Tensor) or is_number(k) or is_tuple(k)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:method, :window_size])
    positional = [
      rows: Evision.Internal.Structurise.from_struct(rows),
      cols: Evision.Internal.Structurise.from_struct(cols),
      depth: Evision.Internal.Structurise.from_struct(depth),
      k: Evision.Internal.Structurise.from_struct(k)
    ]
    :evision_nif.rgbd_rgbd_RgbdNormals_create_static(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  create

  ##### Positional Arguments
  - **rows**: `integer()`.

    the number of rows of the depth image normals will be computed on

  - **cols**: `integer()`.

    the number of cols of the depth image normals will be computed on

  - **depth**: `integer()`.

    the depth of the normals (only CV_32F or CV_64F)

  - **k**: `Evision.Mat`.

    the calibration matrix to use

  ##### Keyword Arguments
  - **window_size**: `integer()`.

    the window size to compute the normals: can only be 1,3,5 or 7

  - **method**: `integer()`.

    one of the methods to use: RGBD_NORMALS_METHOD_SRI, RGBD_NORMALS_METHOD_FALS

  ##### Return
  - **retval**: `RgbdNormals`

  Constructor

  Python prototype (for reference only):
  ```python3
  create(rows, cols, depth, K[, window_size[, method]]) -> retval
  ```
  """
  @spec create(integer(), integer(), integer(), Evision.Mat.maybe_mat_in()) :: Evision.RGBD.RgbdNormals.t() | {:error, String.t()}
  def create(rows, cols, depth, k) when is_integer(rows) and is_integer(cols) and is_integer(depth) and (is_struct(k, Evision.Mat) or is_struct(k, Nx.Tensor) or is_number(k) or is_tuple(k))
  do
    positional = [
      rows: Evision.Internal.Structurise.from_struct(rows),
      cols: Evision.Internal.Structurise.from_struct(cols),
      depth: Evision.Internal.Structurise.from_struct(depth),
      k: Evision.Internal.Structurise.from_struct(k)
    ]
    :evision_nif.rgbd_rgbd_RgbdNormals_create_static(positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdNormals.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.RGBD.RgbdNormals.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.rgbd_RgbdNormals_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getCols

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdNormals.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getCols() -> retval
  ```
  """
  @spec getCols(Evision.RGBD.RgbdNormals.t()) :: integer() | {:error, String.t()}
  def getCols(self) do
    positional = [
    ]
    :evision_nif.rgbd_rgbd_RgbdNormals_getCols(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdNormals.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.RGBD.RgbdNormals.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.rgbd_RgbdNormals_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDepth

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdNormals.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getDepth() -> retval
  ```
  """
  @spec getDepth(Evision.RGBD.RgbdNormals.t()) :: integer() | {:error, String.t()}
  def getDepth(self) do
    positional = [
    ]
    :evision_nif.rgbd_rgbd_RgbdNormals_getDepth(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getK

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdNormals.t()`

  ##### Return
  - **retval**: `Evision.Mat.t()`

  Python prototype (for reference only):
  ```python3
  getK() -> retval
  ```
  """
  @spec getK(Evision.RGBD.RgbdNormals.t()) :: Evision.Mat.t() | {:error, String.t()}
  def getK(self) do
    positional = [
    ]
    :evision_nif.rgbd_rgbd_RgbdNormals_getK(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMethod

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdNormals.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getMethod() -> retval
  ```
  """
  @spec getMethod(Evision.RGBD.RgbdNormals.t()) :: integer() | {:error, String.t()}
  def getMethod(self) do
    positional = [
    ]
    :evision_nif.rgbd_rgbd_RgbdNormals_getMethod(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getRows

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdNormals.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getRows() -> retval
  ```
  """
  @spec getRows(Evision.RGBD.RgbdNormals.t()) :: integer() | {:error, String.t()}
  def getRows(self) do
    positional = [
    ]
    :evision_nif.rgbd_rgbd_RgbdNormals_getRows(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getWindowSize

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdNormals.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getWindowSize() -> retval
  ```
  """
  @spec getWindowSize(Evision.RGBD.RgbdNormals.t()) :: integer() | {:error, String.t()}
  def getWindowSize(self) do
    positional = [
    ]
    :evision_nif.rgbd_rgbd_RgbdNormals_getWindowSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  initialize

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdNormals.t()`

  Initializes some data that is cached for later computation
   If that function is not called, it will be called the first time normals are computed

  Python prototype (for reference only):
  ```python3
  initialize() -> None
  ```
  """
  @spec initialize(Evision.RGBD.RgbdNormals.t()) :: Evision.RGBD.RgbdNormals.t() | {:error, String.t()}
  def initialize(self) do
    positional = [
    ]
    :evision_nif.rgbd_rgbd_RgbdNormals_initialize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdNormals.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.RGBD.RgbdNormals.t(), Evision.FileNode.t()) :: Evision.RGBD.RgbdNormals.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.rgbd_RgbdNormals_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdNormals.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.RGBD.RgbdNormals.t(), binary()) :: Evision.RGBD.RgbdNormals.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.rgbd_RgbdNormals_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setCols

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdNormals.t()`
  - **val**: `integer()`

  Python prototype (for reference only):
  ```python3
  setCols(val) -> None
  ```
  """
  @spec setCols(Evision.RGBD.RgbdNormals.t(), integer()) :: Evision.RGBD.RgbdNormals.t() | {:error, String.t()}
  def setCols(self, val) when is_integer(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.rgbd_rgbd_RgbdNormals_setCols(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setDepth

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdNormals.t()`
  - **val**: `integer()`

  Python prototype (for reference only):
  ```python3
  setDepth(val) -> None
  ```
  """
  @spec setDepth(Evision.RGBD.RgbdNormals.t(), integer()) :: Evision.RGBD.RgbdNormals.t() | {:error, String.t()}
  def setDepth(self, val) when is_integer(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.rgbd_rgbd_RgbdNormals_setDepth(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setK

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdNormals.t()`
  - **val**: `Evision.Mat`

  Python prototype (for reference only):
  ```python3
  setK(val) -> None
  ```
  """
  @spec setK(Evision.RGBD.RgbdNormals.t(), Evision.Mat.maybe_mat_in()) :: Evision.RGBD.RgbdNormals.t() | {:error, String.t()}
  def setK(self, val) when (is_struct(val, Evision.Mat) or is_struct(val, Nx.Tensor) or is_number(val) or is_tuple(val))
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.rgbd_rgbd_RgbdNormals_setK(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMethod

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdNormals.t()`
  - **val**: `integer()`

  Python prototype (for reference only):
  ```python3
  setMethod(val) -> None
  ```
  """
  @spec setMethod(Evision.RGBD.RgbdNormals.t(), integer()) :: Evision.RGBD.RgbdNormals.t() | {:error, String.t()}
  def setMethod(self, val) when is_integer(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.rgbd_rgbd_RgbdNormals_setMethod(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setRows

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdNormals.t()`
  - **val**: `integer()`

  Python prototype (for reference only):
  ```python3
  setRows(val) -> None
  ```
  """
  @spec setRows(Evision.RGBD.RgbdNormals.t(), integer()) :: Evision.RGBD.RgbdNormals.t() | {:error, String.t()}
  def setRows(self, val) when is_integer(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.rgbd_rgbd_RgbdNormals_setRows(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setWindowSize

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdNormals.t()`
  - **val**: `integer()`

  Python prototype (for reference only):
  ```python3
  setWindowSize(val) -> None
  ```
  """
  @spec setWindowSize(Evision.RGBD.RgbdNormals.t(), integer()) :: Evision.RGBD.RgbdNormals.t() | {:error, String.t()}
  def setWindowSize(self, val) when is_integer(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.rgbd_rgbd_RgbdNormals_setWindowSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdNormals.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.RGBD.RgbdNormals.t(), Evision.FileStorage.t(), binary()) :: Evision.RGBD.RgbdNormals.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.rgbd_RgbdNormals_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdNormals.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.RGBD.RgbdNormals.t(), Evision.FileStorage.t()) :: Evision.RGBD.RgbdNormals.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.rgbd_RgbdNormals_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
