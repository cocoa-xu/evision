defmodule Evision.XImgProc.ScanSegment do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `XImgProc.ScanSegment` struct.

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
  def to_struct({:ok, %{class: Evision.XImgProc.ScanSegment, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.XImgProc.ScanSegment, ref: ref}) do
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
  - **self**: `Evision.XImgProc.ScanSegment.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.XImgProc.ScanSegment.t()) :: Evision.XImgProc.ScanSegment.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.ximgproc_ScanSegment_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.ScanSegment.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.XImgProc.ScanSegment.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.ximgproc_ScanSegment_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.ScanSegment.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.XImgProc.ScanSegment.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.ximgproc_ScanSegment_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns the mask of the superpixel segmentation stored in the ScanSegment object.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.ScanSegment.t()`

  ##### Keyword Arguments
  - **thick_line**: `bool`.

    If false, the border is only one pixel wide, otherwise all pixels at the border are masked.

  ##### Return
  - **image**: `Evision.Mat.t()`.

    Return: CV_8UC1 image mask where -1 indicates that the pixel is a superpixel border, and 0 otherwise.

  The function return the boundaries of the superpixel segmentation.

  Python prototype (for reference only):
  ```python3
  getLabelContourMask([, image[, thick_line]]) -> image
  ```
  """
  @spec getLabelContourMask(Evision.XImgProc.ScanSegment.t(), [{:thick_line, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def getLabelContourMask(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:thick_line])
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_ScanSegment_getLabelContourMask(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Returns the mask of the superpixel segmentation stored in the ScanSegment object.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.ScanSegment.t()`

  ##### Keyword Arguments
  - **thick_line**: `bool`.

    If false, the border is only one pixel wide, otherwise all pixels at the border are masked.

  ##### Return
  - **image**: `Evision.Mat.t()`.

    Return: CV_8UC1 image mask where -1 indicates that the pixel is a superpixel border, and 0 otherwise.

  The function return the boundaries of the superpixel segmentation.

  Python prototype (for reference only):
  ```python3
  getLabelContourMask([, image[, thick_line]]) -> image
  ```
  """
  @spec getLabelContourMask(Evision.XImgProc.ScanSegment.t()) :: Evision.Mat.t() | {:error, String.t()}
  def getLabelContourMask(self) do
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_ScanSegment_getLabelContourMask(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns the segmentation labeling of the image.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.ScanSegment.t()`

  ##### Return
  - **labels_out**: `Evision.Mat.t()`.

    Return: A CV_32UC1 integer array containing the labels of the superpixel
    segmentation. The labels are in the range [0, getNumberOfSuperpixels()].

  Each label represents a superpixel, and each pixel is assigned to one superpixel label.

  Python prototype (for reference only):
  ```python3
  getLabels([, labels_out]) -> labels_out
  ```
  """
  @spec getLabels(Evision.XImgProc.ScanSegment.t(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def getLabels(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_ScanSegment_getLabels(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Returns the segmentation labeling of the image.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.ScanSegment.t()`

  ##### Return
  - **labels_out**: `Evision.Mat.t()`.

    Return: A CV_32UC1 integer array containing the labels of the superpixel
    segmentation. The labels are in the range [0, getNumberOfSuperpixels()].

  Each label represents a superpixel, and each pixel is assigned to one superpixel label.

  Python prototype (for reference only):
  ```python3
  getLabels([, labels_out]) -> labels_out
  ```
  """
  @spec getLabels(Evision.XImgProc.ScanSegment.t()) :: Evision.Mat.t() | {:error, String.t()}
  def getLabels(self) do
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_ScanSegment_getLabels(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns the actual superpixel segmentation from the last image processed using iterate.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.ScanSegment.t()`

  ##### Return
  - **retval**: `integer()`

  Returns zero if no image has been processed.

  Python prototype (for reference only):
  ```python3
  getNumberOfSuperpixels() -> retval
  ```
  """
  @spec getNumberOfSuperpixels(Evision.XImgProc.ScanSegment.t()) :: integer() | {:error, String.t()}
  def getNumberOfSuperpixels(self) do
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_ScanSegment_getNumberOfSuperpixels(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Calculates the superpixel segmentation on a given image with the initialized
  parameters in the ScanSegment object.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.ScanSegment.t()`
  - **img**: `Evision.Mat`.

    Input image. Supported format: CV_8UC3. Image size must match with the initialized
    image size with the function createScanSegment(). It MUST be in Lab color space.

  This function can be called again for other images without the need of initializing the algorithm with createScanSegment().
  This save the computational cost of allocating memory for all the structures of the algorithm.

  Python prototype (for reference only):
  ```python3
  iterate(img) -> None
  ```
  """
  @spec iterate(Evision.XImgProc.ScanSegment.t(), Evision.Mat.maybe_mat_in()) :: Evision.XImgProc.ScanSegment.t() | {:error, String.t()}
  def iterate(self, img) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img)
    ]
    :evision_nif.ximgproc_ximgproc_ScanSegment_iterate(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.ScanSegment.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.XImgProc.ScanSegment.t(), Evision.FileNode.t()) :: Evision.XImgProc.ScanSegment.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.ximgproc_ScanSegment_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.ScanSegment.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.XImgProc.ScanSegment.t(), binary()) :: Evision.XImgProc.ScanSegment.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.ximgproc_ScanSegment_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.ScanSegment.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.XImgProc.ScanSegment.t(), Evision.FileStorage.t(), binary()) :: Evision.XImgProc.ScanSegment.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.ximgproc_ScanSegment_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.ScanSegment.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.XImgProc.ScanSegment.t(), Evision.FileStorage.t()) :: Evision.XImgProc.ScanSegment.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.ximgproc_ScanSegment_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
