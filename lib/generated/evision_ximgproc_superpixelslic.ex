defmodule Evision.XImgProc.SuperpixelSLIC do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `XImgProc.SuperpixelSLIC` struct.

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
  def to_struct({:ok, %{class: Evision.XImgProc.SuperpixelSLIC, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.XImgProc.SuperpixelSLIC, ref: ref}) do
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
  - **self**: `Evision.XImgProc.SuperpixelSLIC.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.XImgProc.SuperpixelSLIC.t()) :: Evision.XImgProc.SuperpixelSLIC.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.ximgproc_SuperpixelSLIC_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.SuperpixelSLIC.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.XImgProc.SuperpixelSLIC.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.ximgproc_SuperpixelSLIC_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Enforce label connectivity.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.SuperpixelSLIC.t()`

  ##### Keyword Arguments
  - **min_element_size**: `integer()`.

    The minimum element size in percents that should be absorbed into a bigger
    superpixel. Given resulted average superpixel size valid value should be in 0-100 range, 25 means
    that less then a quarter sized superpixel should be absorbed, this is default.

  The function merge component that is too small, assigning the previously found adjacent label
  to this component. Calling this function may change the final number of superpixels.

  Python prototype (for reference only):
  ```python3
  enforceLabelConnectivity([, min_element_size]) -> None
  ```
  """
  @spec enforceLabelConnectivity(Evision.XImgProc.SuperpixelSLIC.t(), [{:min_element_size, term()}] | nil) :: Evision.XImgProc.SuperpixelSLIC.t() | {:error, String.t()}
  def enforceLabelConnectivity(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:min_element_size])
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_SuperpixelSLIC_enforceLabelConnectivity(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Enforce label connectivity.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.SuperpixelSLIC.t()`

  ##### Keyword Arguments
  - **min_element_size**: `integer()`.

    The minimum element size in percents that should be absorbed into a bigger
    superpixel. Given resulted average superpixel size valid value should be in 0-100 range, 25 means
    that less then a quarter sized superpixel should be absorbed, this is default.

  The function merge component that is too small, assigning the previously found adjacent label
  to this component. Calling this function may change the final number of superpixels.

  Python prototype (for reference only):
  ```python3
  enforceLabelConnectivity([, min_element_size]) -> None
  ```
  """
  @spec enforceLabelConnectivity(Evision.XImgProc.SuperpixelSLIC.t()) :: Evision.XImgProc.SuperpixelSLIC.t() | {:error, String.t()}
  def enforceLabelConnectivity(self) do
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_SuperpixelSLIC_enforceLabelConnectivity(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.SuperpixelSLIC.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.XImgProc.SuperpixelSLIC.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.ximgproc_SuperpixelSLIC_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns the mask of the superpixel segmentation stored in SuperpixelSLIC object.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.SuperpixelSLIC.t()`

  ##### Keyword Arguments
  - **thick_line**: `bool`.

    If false, the border is only one pixel wide, otherwise all pixels at the border
    are masked.

  ##### Return
  - **image**: `Evision.Mat.t()`.

    Return: CV_8U1 image mask where -1 indicates that the pixel is a superpixel border,
    and 0 otherwise.

  The function return the boundaries of the superpixel segmentation.

  Python prototype (for reference only):
  ```python3
  getLabelContourMask([, image[, thick_line]]) -> image
  ```
  """
  @spec getLabelContourMask(Evision.XImgProc.SuperpixelSLIC.t(), [{:thick_line, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def getLabelContourMask(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:thick_line])
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_SuperpixelSLIC_getLabelContourMask(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Returns the mask of the superpixel segmentation stored in SuperpixelSLIC object.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.SuperpixelSLIC.t()`

  ##### Keyword Arguments
  - **thick_line**: `bool`.

    If false, the border is only one pixel wide, otherwise all pixels at the border
    are masked.

  ##### Return
  - **image**: `Evision.Mat.t()`.

    Return: CV_8U1 image mask where -1 indicates that the pixel is a superpixel border,
    and 0 otherwise.

  The function return the boundaries of the superpixel segmentation.

  Python prototype (for reference only):
  ```python3
  getLabelContourMask([, image[, thick_line]]) -> image
  ```
  """
  @spec getLabelContourMask(Evision.XImgProc.SuperpixelSLIC.t()) :: Evision.Mat.t() | {:error, String.t()}
  def getLabelContourMask(self) do
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_SuperpixelSLIC_getLabelContourMask(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns the segmentation labeling of the image.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.SuperpixelSLIC.t()`

  ##### Return
  - **labels_out**: `Evision.Mat.t()`.

    Return: A CV_32SC1 integer array containing the labels of the superpixel
    segmentation. The labels are in the range [0, getNumberOfSuperpixels()].

  Each label represents a superpixel, and each pixel is assigned to one superpixel label.

  The function returns an image with the labels of the superpixel segmentation. The labels are in
  the range [0, getNumberOfSuperpixels()].

  Python prototype (for reference only):
  ```python3
  getLabels([, labels_out]) -> labels_out
  ```
  """
  @spec getLabels(Evision.XImgProc.SuperpixelSLIC.t(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def getLabels(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_SuperpixelSLIC_getLabels(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Returns the segmentation labeling of the image.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.SuperpixelSLIC.t()`

  ##### Return
  - **labels_out**: `Evision.Mat.t()`.

    Return: A CV_32SC1 integer array containing the labels of the superpixel
    segmentation. The labels are in the range [0, getNumberOfSuperpixels()].

  Each label represents a superpixel, and each pixel is assigned to one superpixel label.

  The function returns an image with the labels of the superpixel segmentation. The labels are in
  the range [0, getNumberOfSuperpixels()].

  Python prototype (for reference only):
  ```python3
  getLabels([, labels_out]) -> labels_out
  ```
  """
  @spec getLabels(Evision.XImgProc.SuperpixelSLIC.t()) :: Evision.Mat.t() | {:error, String.t()}
  def getLabels(self) do
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_SuperpixelSLIC_getLabels(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Calculates the actual amount of superpixels on a given segmentation computed
  and stored in SuperpixelSLIC object.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.SuperpixelSLIC.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getNumberOfSuperpixels() -> retval
  ```
  """
  @spec getNumberOfSuperpixels(Evision.XImgProc.SuperpixelSLIC.t()) :: integer() | {:error, String.t()}
  def getNumberOfSuperpixels(self) do
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_SuperpixelSLIC_getNumberOfSuperpixels(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Calculates the superpixel segmentation on a given image with the initialized
  parameters in the SuperpixelSLIC object.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.SuperpixelSLIC.t()`

  ##### Keyword Arguments
  - **num_iterations**: `integer()`.

    Number of iterations. Higher number improves the result.

  This function can be called again without the need of initializing the algorithm with
  createSuperpixelSLIC(). This save the computational cost of allocating memory for all the
  structures of the algorithm.

  The function computes the superpixels segmentation of an image with the parameters initialized
  with the function createSuperpixelSLIC(). The algorithms starts from a grid of superpixels and
  then refines the boundaries by proposing updates of edges boundaries.

  Python prototype (for reference only):
  ```python3
  iterate([, num_iterations]) -> None
  ```
  """
  @spec iterate(Evision.XImgProc.SuperpixelSLIC.t(), [{:num_iterations, term()}] | nil) :: Evision.XImgProc.SuperpixelSLIC.t() | {:error, String.t()}
  def iterate(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:num_iterations])
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_SuperpixelSLIC_iterate(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Calculates the superpixel segmentation on a given image with the initialized
  parameters in the SuperpixelSLIC object.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.SuperpixelSLIC.t()`

  ##### Keyword Arguments
  - **num_iterations**: `integer()`.

    Number of iterations. Higher number improves the result.

  This function can be called again without the need of initializing the algorithm with
  createSuperpixelSLIC(). This save the computational cost of allocating memory for all the
  structures of the algorithm.

  The function computes the superpixels segmentation of an image with the parameters initialized
  with the function createSuperpixelSLIC(). The algorithms starts from a grid of superpixels and
  then refines the boundaries by proposing updates of edges boundaries.

  Python prototype (for reference only):
  ```python3
  iterate([, num_iterations]) -> None
  ```
  """
  @spec iterate(Evision.XImgProc.SuperpixelSLIC.t()) :: Evision.XImgProc.SuperpixelSLIC.t() | {:error, String.t()}
  def iterate(self) do
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_SuperpixelSLIC_iterate(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.SuperpixelSLIC.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.XImgProc.SuperpixelSLIC.t(), Evision.FileNode.t()) :: Evision.XImgProc.SuperpixelSLIC.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.ximgproc_SuperpixelSLIC_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.SuperpixelSLIC.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.XImgProc.SuperpixelSLIC.t(), binary()) :: Evision.XImgProc.SuperpixelSLIC.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.ximgproc_SuperpixelSLIC_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.SuperpixelSLIC.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.XImgProc.SuperpixelSLIC.t(), Evision.FileStorage.t(), binary()) :: Evision.XImgProc.SuperpixelSLIC.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.ximgproc_SuperpixelSLIC_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.SuperpixelSLIC.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.XImgProc.SuperpixelSLIC.t(), Evision.FileStorage.t()) :: Evision.XImgProc.SuperpixelSLIC.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.ximgproc_SuperpixelSLIC_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
