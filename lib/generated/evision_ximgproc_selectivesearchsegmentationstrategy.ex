defmodule Evision.XImgProc.SelectiveSearchSegmentationStrategy do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `XImgProc.SelectiveSearchSegmentationStrategy` struct.

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
  def to_struct({:ok, %{class: Evision.XImgProc.SelectiveSearchSegmentationStrategy, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.XImgProc.SelectiveSearchSegmentationStrategy, ref: ref}) do
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
  - **self**: `Evision.XImgProc.SelectiveSearchSegmentationStrategy.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.XImgProc.SelectiveSearchSegmentationStrategy.t()) :: Evision.XImgProc.SelectiveSearchSegmentationStrategy.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.ximgproc_segmentation_SelectiveSearchSegmentationStrategy_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.SelectiveSearchSegmentationStrategy.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.XImgProc.SelectiveSearchSegmentationStrategy.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.ximgproc_segmentation_SelectiveSearchSegmentationStrategy_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Return the score between two regions (between 0 and 1)

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.SelectiveSearchSegmentationStrategy.t()`
  - **r1**: `integer()`.

    The first region

  - **r2**: `integer()`.

    The second region

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  get(r1, r2) -> retval
  ```
  """
  @spec get(Evision.XImgProc.SelectiveSearchSegmentationStrategy.t(), integer(), integer()) :: number() | {:error, String.t()}
  def get(self, r1, r2) when is_integer(r1) and is_integer(r2)
  do
    positional = [
      r1: Evision.Internal.Structurise.from_struct(r1),
      r2: Evision.Internal.Structurise.from_struct(r2)
    ]
    :evision_nif.ximgproc_segmentation_ximgproc_segmentation_SelectiveSearchSegmentationStrategy_get(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.SelectiveSearchSegmentationStrategy.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.XImgProc.SelectiveSearchSegmentationStrategy.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.ximgproc_segmentation_SelectiveSearchSegmentationStrategy_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Inform the strategy that two regions will be merged

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.SelectiveSearchSegmentationStrategy.t()`
  - **r1**: `integer()`.

    The first region

  - **r2**: `integer()`.

    The second region

  Python prototype (for reference only):
  ```python3
  merge(r1, r2) -> None
  ```
  """
  @spec merge(Evision.XImgProc.SelectiveSearchSegmentationStrategy.t(), integer(), integer()) :: Evision.XImgProc.SelectiveSearchSegmentationStrategy.t() | {:error, String.t()}
  def merge(self, r1, r2) when is_integer(r1) and is_integer(r2)
  do
    positional = [
      r1: Evision.Internal.Structurise.from_struct(r1),
      r2: Evision.Internal.Structurise.from_struct(r2)
    ]
    :evision_nif.ximgproc_segmentation_ximgproc_segmentation_SelectiveSearchSegmentationStrategy_merge(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.SelectiveSearchSegmentationStrategy.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.XImgProc.SelectiveSearchSegmentationStrategy.t(), Evision.FileNode.t()) :: Evision.XImgProc.SelectiveSearchSegmentationStrategy.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.ximgproc_segmentation_SelectiveSearchSegmentationStrategy_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.SelectiveSearchSegmentationStrategy.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.XImgProc.SelectiveSearchSegmentationStrategy.t(), binary()) :: Evision.XImgProc.SelectiveSearchSegmentationStrategy.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.ximgproc_segmentation_SelectiveSearchSegmentationStrategy_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Set a initial image, with a segmentation.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.SelectiveSearchSegmentationStrategy.t()`
  - **img**: `Evision.Mat`.

    The input image. Any number of channel can be provided

  - **regions**: `Evision.Mat`.

    A segmentation of the image. The parameter must be the same size of img.

  - **sizes**: `Evision.Mat`.

    The sizes of different regions

  ##### Keyword Arguments
  - **image_id**: `integer()`.

    If not set to -1, try to cache pre-computations. If the same set og (img, regions, size) is used, the image_id need to be the same.

  Python prototype (for reference only):
  ```python3
  setImage(img, regions, sizes[, image_id]) -> None
  ```
  """
  @spec setImage(Evision.XImgProc.SelectiveSearchSegmentationStrategy.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:image_id, term()}] | nil) :: Evision.XImgProc.SelectiveSearchSegmentationStrategy.t() | {:error, String.t()}
  def setImage(self, img, regions, sizes, opts) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img)) and (is_struct(regions, Evision.Mat) or is_struct(regions, Nx.Tensor) or is_number(regions) or is_tuple(regions)) and (is_struct(sizes, Evision.Mat) or is_struct(sizes, Nx.Tensor) or is_number(sizes) or is_tuple(sizes)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:image_id])
    positional = [
      img: Evision.Internal.Structurise.from_struct(img),
      regions: Evision.Internal.Structurise.from_struct(regions),
      sizes: Evision.Internal.Structurise.from_struct(sizes)
    ]
    :evision_nif.ximgproc_segmentation_ximgproc_segmentation_SelectiveSearchSegmentationStrategy_setImage(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Set a initial image, with a segmentation.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.SelectiveSearchSegmentationStrategy.t()`
  - **img**: `Evision.Mat`.

    The input image. Any number of channel can be provided

  - **regions**: `Evision.Mat`.

    A segmentation of the image. The parameter must be the same size of img.

  - **sizes**: `Evision.Mat`.

    The sizes of different regions

  ##### Keyword Arguments
  - **image_id**: `integer()`.

    If not set to -1, try to cache pre-computations. If the same set og (img, regions, size) is used, the image_id need to be the same.

  Python prototype (for reference only):
  ```python3
  setImage(img, regions, sizes[, image_id]) -> None
  ```
  """
  @spec setImage(Evision.XImgProc.SelectiveSearchSegmentationStrategy.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.XImgProc.SelectiveSearchSegmentationStrategy.t() | {:error, String.t()}
  def setImage(self, img, regions, sizes) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img)) and (is_struct(regions, Evision.Mat) or is_struct(regions, Nx.Tensor) or is_number(regions) or is_tuple(regions)) and (is_struct(sizes, Evision.Mat) or is_struct(sizes, Nx.Tensor) or is_number(sizes) or is_tuple(sizes))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img),
      regions: Evision.Internal.Structurise.from_struct(regions),
      sizes: Evision.Internal.Structurise.from_struct(sizes)
    ]
    :evision_nif.ximgproc_segmentation_ximgproc_segmentation_SelectiveSearchSegmentationStrategy_setImage(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.SelectiveSearchSegmentationStrategy.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.XImgProc.SelectiveSearchSegmentationStrategy.t(), Evision.FileStorage.t(), binary()) :: Evision.XImgProc.SelectiveSearchSegmentationStrategy.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.ximgproc_segmentation_SelectiveSearchSegmentationStrategy_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.SelectiveSearchSegmentationStrategy.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.XImgProc.SelectiveSearchSegmentationStrategy.t(), Evision.FileStorage.t()) :: Evision.XImgProc.SelectiveSearchSegmentationStrategy.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.ximgproc_segmentation_SelectiveSearchSegmentationStrategy_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
