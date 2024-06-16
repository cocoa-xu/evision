defmodule Evision.StereoMatcher do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `StereoMatcher` struct.

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
  def to_struct({:ok, %{class: Evision.StereoMatcher, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.StereoMatcher, ref: ref}) do
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
  import Bitwise

  @type enum :: integer()
  @doc enum: true
  def cv_DISP_SHIFT, do: 4
  @doc enum: true
  def cv_DISP_SCALE, do: bsl(1, cv_DISP_SHIFT())


  @doc """
  Clears the algorithm state

  ##### Positional Arguments
  - **self**: `Evision.StereoMatcher.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.StereoMatcher.t()) :: Evision.StereoMatcher.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.stereoMatcher_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Computes disparity map for the specified stereo pair

  ##### Positional Arguments
  - **self**: `Evision.StereoMatcher.t()`
  - **left**: `Evision.Mat`.

    Left 8-bit single-channel image.

  - **right**: `Evision.Mat`.

    Right image of the same size and the same type as the left one.

  ##### Return
  - **disparity**: `Evision.Mat.t()`.

    Output disparity map. It has the same size as the input images. Some algorithms,
    like StereoBM or StereoSGBM compute 16-bit fixed-point disparity map (where each disparity value
    has 4 fractional bits), whereas other algorithms output 32-bit floating-point disparity map.

  Python prototype (for reference only):
  ```python3
  compute(left, right[, disparity]) -> disparity
  ```
  """
  @spec compute(Evision.StereoMatcher.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def compute(self, left, right, opts) when (is_struct(left, Evision.Mat) or is_struct(left, Nx.Tensor) or is_number(left) or is_tuple(left)) and (is_struct(right, Evision.Mat) or is_struct(right, Nx.Tensor) or is_number(right) or is_tuple(right)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      left: Evision.Internal.Structurise.from_struct(left),
      right: Evision.Internal.Structurise.from_struct(right)
    ]
    :evision_nif.stereoMatcher_compute(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Computes disparity map for the specified stereo pair

  ##### Positional Arguments
  - **self**: `Evision.StereoMatcher.t()`
  - **left**: `Evision.Mat`.

    Left 8-bit single-channel image.

  - **right**: `Evision.Mat`.

    Right image of the same size and the same type as the left one.

  ##### Return
  - **disparity**: `Evision.Mat.t()`.

    Output disparity map. It has the same size as the input images. Some algorithms,
    like StereoBM or StereoSGBM compute 16-bit fixed-point disparity map (where each disparity value
    has 4 fractional bits), whereas other algorithms output 32-bit floating-point disparity map.

  Python prototype (for reference only):
  ```python3
  compute(left, right[, disparity]) -> disparity
  ```
  """
  @spec compute(Evision.StereoMatcher.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def compute(self, left, right) when (is_struct(left, Evision.Mat) or is_struct(left, Nx.Tensor) or is_number(left) or is_tuple(left)) and (is_struct(right, Evision.Mat) or is_struct(right, Nx.Tensor) or is_number(right) or is_tuple(right))
  do
    positional = [
      left: Evision.Internal.Structurise.from_struct(left),
      right: Evision.Internal.Structurise.from_struct(right)
    ]
    :evision_nif.stereoMatcher_compute(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.StereoMatcher.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.StereoMatcher.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.stereoMatcher_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getBlockSize

  ##### Positional Arguments
  - **self**: `Evision.StereoMatcher.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getBlockSize() -> retval
  ```
  """
  @spec getBlockSize(Evision.StereoMatcher.t()) :: integer() | {:error, String.t()}
  def getBlockSize(self) do
    positional = [
    ]
    :evision_nif.stereoMatcher_getBlockSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.StereoMatcher.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.StereoMatcher.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.stereoMatcher_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDisp12MaxDiff

  ##### Positional Arguments
  - **self**: `Evision.StereoMatcher.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getDisp12MaxDiff() -> retval
  ```
  """
  @spec getDisp12MaxDiff(Evision.StereoMatcher.t()) :: integer() | {:error, String.t()}
  def getDisp12MaxDiff(self) do
    positional = [
    ]
    :evision_nif.stereoMatcher_getDisp12MaxDiff(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMinDisparity

  ##### Positional Arguments
  - **self**: `Evision.StereoMatcher.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getMinDisparity() -> retval
  ```
  """
  @spec getMinDisparity(Evision.StereoMatcher.t()) :: integer() | {:error, String.t()}
  def getMinDisparity(self) do
    positional = [
    ]
    :evision_nif.stereoMatcher_getMinDisparity(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getNumDisparities

  ##### Positional Arguments
  - **self**: `Evision.StereoMatcher.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getNumDisparities() -> retval
  ```
  """
  @spec getNumDisparities(Evision.StereoMatcher.t()) :: integer() | {:error, String.t()}
  def getNumDisparities(self) do
    positional = [
    ]
    :evision_nif.stereoMatcher_getNumDisparities(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getSpeckleRange

  ##### Positional Arguments
  - **self**: `Evision.StereoMatcher.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getSpeckleRange() -> retval
  ```
  """
  @spec getSpeckleRange(Evision.StereoMatcher.t()) :: integer() | {:error, String.t()}
  def getSpeckleRange(self) do
    positional = [
    ]
    :evision_nif.stereoMatcher_getSpeckleRange(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getSpeckleWindowSize

  ##### Positional Arguments
  - **self**: `Evision.StereoMatcher.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getSpeckleWindowSize() -> retval
  ```
  """
  @spec getSpeckleWindowSize(Evision.StereoMatcher.t()) :: integer() | {:error, String.t()}
  def getSpeckleWindowSize(self) do
    positional = [
    ]
    :evision_nif.stereoMatcher_getSpeckleWindowSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.StereoMatcher.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.StereoMatcher.t(), Evision.FileNode.t()) :: Evision.StereoMatcher.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.stereoMatcher_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.StereoMatcher.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.StereoMatcher.t(), binary()) :: Evision.StereoMatcher.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.stereoMatcher_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setBlockSize

  ##### Positional Arguments
  - **self**: `Evision.StereoMatcher.t()`
  - **blockSize**: `integer()`

  Python prototype (for reference only):
  ```python3
  setBlockSize(blockSize) -> None
  ```
  """
  @spec setBlockSize(Evision.StereoMatcher.t(), integer()) :: Evision.StereoMatcher.t() | {:error, String.t()}
  def setBlockSize(self, blockSize) when is_integer(blockSize)
  do
    positional = [
      blockSize: Evision.Internal.Structurise.from_struct(blockSize)
    ]
    :evision_nif.stereoMatcher_setBlockSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setDisp12MaxDiff

  ##### Positional Arguments
  - **self**: `Evision.StereoMatcher.t()`
  - **disp12MaxDiff**: `integer()`

  Python prototype (for reference only):
  ```python3
  setDisp12MaxDiff(disp12MaxDiff) -> None
  ```
  """
  @spec setDisp12MaxDiff(Evision.StereoMatcher.t(), integer()) :: Evision.StereoMatcher.t() | {:error, String.t()}
  def setDisp12MaxDiff(self, disp12MaxDiff) when is_integer(disp12MaxDiff)
  do
    positional = [
      disp12MaxDiff: Evision.Internal.Structurise.from_struct(disp12MaxDiff)
    ]
    :evision_nif.stereoMatcher_setDisp12MaxDiff(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMinDisparity

  ##### Positional Arguments
  - **self**: `Evision.StereoMatcher.t()`
  - **minDisparity**: `integer()`

  Python prototype (for reference only):
  ```python3
  setMinDisparity(minDisparity) -> None
  ```
  """
  @spec setMinDisparity(Evision.StereoMatcher.t(), integer()) :: Evision.StereoMatcher.t() | {:error, String.t()}
  def setMinDisparity(self, minDisparity) when is_integer(minDisparity)
  do
    positional = [
      minDisparity: Evision.Internal.Structurise.from_struct(minDisparity)
    ]
    :evision_nif.stereoMatcher_setMinDisparity(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setNumDisparities

  ##### Positional Arguments
  - **self**: `Evision.StereoMatcher.t()`
  - **numDisparities**: `integer()`

  Python prototype (for reference only):
  ```python3
  setNumDisparities(numDisparities) -> None
  ```
  """
  @spec setNumDisparities(Evision.StereoMatcher.t(), integer()) :: Evision.StereoMatcher.t() | {:error, String.t()}
  def setNumDisparities(self, numDisparities) when is_integer(numDisparities)
  do
    positional = [
      numDisparities: Evision.Internal.Structurise.from_struct(numDisparities)
    ]
    :evision_nif.stereoMatcher_setNumDisparities(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setSpeckleRange

  ##### Positional Arguments
  - **self**: `Evision.StereoMatcher.t()`
  - **speckleRange**: `integer()`

  Python prototype (for reference only):
  ```python3
  setSpeckleRange(speckleRange) -> None
  ```
  """
  @spec setSpeckleRange(Evision.StereoMatcher.t(), integer()) :: Evision.StereoMatcher.t() | {:error, String.t()}
  def setSpeckleRange(self, speckleRange) when is_integer(speckleRange)
  do
    positional = [
      speckleRange: Evision.Internal.Structurise.from_struct(speckleRange)
    ]
    :evision_nif.stereoMatcher_setSpeckleRange(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setSpeckleWindowSize

  ##### Positional Arguments
  - **self**: `Evision.StereoMatcher.t()`
  - **speckleWindowSize**: `integer()`

  Python prototype (for reference only):
  ```python3
  setSpeckleWindowSize(speckleWindowSize) -> None
  ```
  """
  @spec setSpeckleWindowSize(Evision.StereoMatcher.t(), integer()) :: Evision.StereoMatcher.t() | {:error, String.t()}
  def setSpeckleWindowSize(self, speckleWindowSize) when is_integer(speckleWindowSize)
  do
    positional = [
      speckleWindowSize: Evision.Internal.Structurise.from_struct(speckleWindowSize)
    ]
    :evision_nif.stereoMatcher_setSpeckleWindowSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.StereoMatcher.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.StereoMatcher.t(), Evision.FileStorage.t(), binary()) :: Evision.StereoMatcher.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.stereoMatcher_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.StereoMatcher.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.StereoMatcher.t(), Evision.FileStorage.t()) :: Evision.StereoMatcher.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.stereoMatcher_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
