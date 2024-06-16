defmodule Evision.XImgProc.GraphSegmentation do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `XImgProc.GraphSegmentation` struct.

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
  def to_struct({:ok, %{class: Evision.XImgProc.GraphSegmentation, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.XImgProc.GraphSegmentation, ref: ref}) do
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
  Clears the algorithm state

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.GraphSegmentation.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.XImgProc.GraphSegmentation.t()) :: Evision.XImgProc.GraphSegmentation.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.ximgproc_segmentation_GraphSegmentation_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.GraphSegmentation.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.XImgProc.GraphSegmentation.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.ximgproc_segmentation_GraphSegmentation_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.GraphSegmentation.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.XImgProc.GraphSegmentation.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.ximgproc_segmentation_GraphSegmentation_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getK

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.GraphSegmentation.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  getK() -> retval
  ```
  """
  @spec getK(Evision.XImgProc.GraphSegmentation.t()) :: number() | {:error, String.t()}
  def getK(self) do
    positional = [
    ]
    :evision_nif.ximgproc_segmentation_ximgproc_segmentation_GraphSegmentation_getK(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMinSize

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.GraphSegmentation.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getMinSize() -> retval
  ```
  """
  @spec getMinSize(Evision.XImgProc.GraphSegmentation.t()) :: integer() | {:error, String.t()}
  def getMinSize(self) do
    positional = [
    ]
    :evision_nif.ximgproc_segmentation_ximgproc_segmentation_GraphSegmentation_getMinSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getSigma

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.GraphSegmentation.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getSigma() -> retval
  ```
  """
  @spec getSigma(Evision.XImgProc.GraphSegmentation.t()) :: number() | {:error, String.t()}
  def getSigma(self) do
    positional = [
    ]
    :evision_nif.ximgproc_segmentation_ximgproc_segmentation_GraphSegmentation_getSigma(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Segment an image and store output in dst

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.GraphSegmentation.t()`
  - **src**: `Evision.Mat`.

    The input image. Any number of channel (1 (Eg: Gray), 3 (Eg: RGB), 4 (Eg: RGB-D)) can be provided

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    The output segmentation. It's a CV_32SC1 Mat with the same number of cols and rows as input image, with an unique, sequential, id for each pixel.

  Python prototype (for reference only):
  ```python3
  processImage(src[, dst]) -> dst
  ```
  """
  @spec processImage(Evision.XImgProc.GraphSegmentation.t(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def processImage(self, src, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.ximgproc_segmentation_ximgproc_segmentation_GraphSegmentation_processImage(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Segment an image and store output in dst

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.GraphSegmentation.t()`
  - **src**: `Evision.Mat`.

    The input image. Any number of channel (1 (Eg: Gray), 3 (Eg: RGB), 4 (Eg: RGB-D)) can be provided

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    The output segmentation. It's a CV_32SC1 Mat with the same number of cols and rows as input image, with an unique, sequential, id for each pixel.

  Python prototype (for reference only):
  ```python3
  processImage(src[, dst]) -> dst
  ```
  """
  @spec processImage(Evision.XImgProc.GraphSegmentation.t(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def processImage(self, src) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.ximgproc_segmentation_ximgproc_segmentation_GraphSegmentation_processImage(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.GraphSegmentation.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.XImgProc.GraphSegmentation.t(), Evision.FileNode.t()) :: Evision.XImgProc.GraphSegmentation.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.ximgproc_segmentation_GraphSegmentation_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.GraphSegmentation.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.XImgProc.GraphSegmentation.t(), binary()) :: Evision.XImgProc.GraphSegmentation.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.ximgproc_segmentation_GraphSegmentation_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setK

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.GraphSegmentation.t()`
  - **k**: `float`

  Python prototype (for reference only):
  ```python3
  setK(k) -> None
  ```
  """
  @spec setK(Evision.XImgProc.GraphSegmentation.t(), number()) :: Evision.XImgProc.GraphSegmentation.t() | {:error, String.t()}
  def setK(self, k) when is_float(k)
  do
    positional = [
      k: Evision.Internal.Structurise.from_struct(k)
    ]
    :evision_nif.ximgproc_segmentation_ximgproc_segmentation_GraphSegmentation_setK(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMinSize

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.GraphSegmentation.t()`
  - **min_size**: `integer()`

  Python prototype (for reference only):
  ```python3
  setMinSize(min_size) -> None
  ```
  """
  @spec setMinSize(Evision.XImgProc.GraphSegmentation.t(), integer()) :: Evision.XImgProc.GraphSegmentation.t() | {:error, String.t()}
  def setMinSize(self, min_size) when is_integer(min_size)
  do
    positional = [
      min_size: Evision.Internal.Structurise.from_struct(min_size)
    ]
    :evision_nif.ximgproc_segmentation_ximgproc_segmentation_GraphSegmentation_setMinSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setSigma

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.GraphSegmentation.t()`
  - **sigma**: `double`

  Python prototype (for reference only):
  ```python3
  setSigma(sigma) -> None
  ```
  """
  @spec setSigma(Evision.XImgProc.GraphSegmentation.t(), number()) :: Evision.XImgProc.GraphSegmentation.t() | {:error, String.t()}
  def setSigma(self, sigma) when is_number(sigma)
  do
    positional = [
      sigma: Evision.Internal.Structurise.from_struct(sigma)
    ]
    :evision_nif.ximgproc_segmentation_ximgproc_segmentation_GraphSegmentation_setSigma(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.GraphSegmentation.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.XImgProc.GraphSegmentation.t(), Evision.FileStorage.t(), binary()) :: Evision.XImgProc.GraphSegmentation.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.ximgproc_segmentation_GraphSegmentation_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.GraphSegmentation.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.XImgProc.GraphSegmentation.t(), Evision.FileStorage.t()) :: Evision.XImgProc.GraphSegmentation.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.ximgproc_segmentation_GraphSegmentation_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
