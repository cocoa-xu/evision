defmodule Evision.CLAHE do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `CLAHE` struct.

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
  def to_struct({:ok, %{class: Evision.CLAHE, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.CLAHE, ref: ref}) do
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
  Equalizes the histogram of a grayscale image using Contrast Limited Adaptive Histogram Equalization.

  ##### Positional Arguments
  - **self**: `Evision.CLAHE.t()`
  - **src**: `Evision.Mat`.

    Source image of type CV_8UC1 or CV_16UC1.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination image.

  Python prototype (for reference only):
  ```python3
  apply(src[, dst]) -> dst
  ```
  """
  @spec apply(Evision.CLAHE.t(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def apply(self, src, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.clahe_apply(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Equalizes the histogram of a grayscale image using Contrast Limited Adaptive Histogram Equalization.

  ##### Positional Arguments
  - **self**: `Evision.CLAHE.t()`
  - **src**: `Evision.Mat`.

    Source image of type CV_8UC1 or CV_16UC1.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination image.

  Python prototype (for reference only):
  ```python3
  apply(src[, dst]) -> dst
  ```
  """
  @spec apply(Evision.CLAHE.t(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def apply(self, src) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.clahe_apply(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Clears the algorithm state

  ##### Positional Arguments
  - **self**: `Evision.CLAHE.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.CLAHE.t()) :: Evision.CLAHE.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.clahe_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  collectGarbage

  ##### Positional Arguments
  - **self**: `Evision.CLAHE.t()`

  Python prototype (for reference only):
  ```python3
  collectGarbage() -> None
  ```
  """
  @spec collectGarbage(Evision.CLAHE.t()) :: Evision.CLAHE.t() | {:error, String.t()}
  def collectGarbage(self) do
    positional = [
    ]
    :evision_nif.clahe_collectGarbage(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.CLAHE.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.CLAHE.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.clahe_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getClipLimit

  ##### Positional Arguments
  - **self**: `Evision.CLAHE.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getClipLimit() -> retval
  ```
  """
  @spec getClipLimit(Evision.CLAHE.t()) :: number() | {:error, String.t()}
  def getClipLimit(self) do
    positional = [
    ]
    :evision_nif.clahe_getClipLimit(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.CLAHE.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.CLAHE.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.clahe_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getTilesGridSize

  ##### Positional Arguments
  - **self**: `Evision.CLAHE.t()`

  ##### Return
  - **retval**: `Size`

  Python prototype (for reference only):
  ```python3
  getTilesGridSize() -> retval
  ```
  """
  @spec getTilesGridSize(Evision.CLAHE.t()) :: {number(), number()} | {:error, String.t()}
  def getTilesGridSize(self) do
    positional = [
    ]
    :evision_nif.clahe_getTilesGridSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.CLAHE.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.CLAHE.t(), Evision.FileNode.t()) :: Evision.CLAHE.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.clahe_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.CLAHE.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.CLAHE.t(), binary()) :: Evision.CLAHE.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.clahe_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Sets threshold for contrast limiting.

  ##### Positional Arguments
  - **self**: `Evision.CLAHE.t()`
  - **clipLimit**: `double`.

    threshold value.

  Python prototype (for reference only):
  ```python3
  setClipLimit(clipLimit) -> None
  ```
  """
  @spec setClipLimit(Evision.CLAHE.t(), number()) :: Evision.CLAHE.t() | {:error, String.t()}
  def setClipLimit(self, clipLimit) when is_number(clipLimit)
  do
    positional = [
      clipLimit: Evision.Internal.Structurise.from_struct(clipLimit)
    ]
    :evision_nif.clahe_setClipLimit(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Sets size of grid for histogram equalization. Input image will be divided into
  equally sized rectangular tiles.

  ##### Positional Arguments
  - **self**: `Evision.CLAHE.t()`
  - **tileGridSize**: `Size`.

    defines the number of tiles in row and column.

  Python prototype (for reference only):
  ```python3
  setTilesGridSize(tileGridSize) -> None
  ```
  """
  @spec setTilesGridSize(Evision.CLAHE.t(), {number(), number()}) :: Evision.CLAHE.t() | {:error, String.t()}
  def setTilesGridSize(self, tileGridSize) when is_tuple(tileGridSize)
  do
    positional = [
      tileGridSize: Evision.Internal.Structurise.from_struct(tileGridSize)
    ]
    :evision_nif.clahe_setTilesGridSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.CLAHE.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.CLAHE.t(), Evision.FileStorage.t(), binary()) :: Evision.CLAHE.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.clahe_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.CLAHE.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.CLAHE.t(), Evision.FileStorage.t()) :: Evision.CLAHE.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.clahe_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
