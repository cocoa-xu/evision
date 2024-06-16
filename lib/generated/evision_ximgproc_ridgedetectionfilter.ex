defmodule Evision.XImgProc.RidgeDetectionFilter do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `XImgProc.RidgeDetectionFilter` struct.

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
  def to_struct({:ok, %{class: Evision.XImgProc.RidgeDetectionFilter, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.XImgProc.RidgeDetectionFilter, ref: ref}) do
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
  - **self**: `Evision.XImgProc.RidgeDetectionFilter.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.XImgProc.RidgeDetectionFilter.t()) :: Evision.XImgProc.RidgeDetectionFilter.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.ximgproc_RidgeDetectionFilter_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Create pointer to the Ridge detection filter.
  ##### Keyword Arguments
  - **ddepth**: `integer()`.

    Specifies output image depth. Defualt is CV_32FC1

  - **dx**: `integer()`.

    Order of derivative x, default is 1

  - **dy**: `integer()`.

    Order of derivative y, default is 1

  - **ksize**: `integer()`.

    Sobel kernel size , default is 3

  - **out_dtype**: `integer()`.

    Converted format for output, default is CV_8UC1

  - **scale**: `double`.

    Optional scale value for derivative values, default is 1

  - **delta**: `double`.

    Optional bias added to output, default is 0

  - **borderType**: `integer()`.

    Pixel extrapolation method, default is BORDER_DEFAULT

  ##### Return
  - **retval**: `RidgeDetectionFilter`

  @see Sobel, threshold, getStructuringElement, morphologyEx.( for additional refinement)

  Python prototype (for reference only):
  ```python3
  create([, ddepth[, dx[, dy[, ksize[, out_dtype[, scale[, delta[, borderType]]]]]]]]) -> retval
  ```
  """
  @spec create([{:borderType, term()} | {:ddepth, term()} | {:delta, term()} | {:dx, term()} | {:dy, term()} | {:ksize, term()} | {:out_dtype, term()} | {:scale, term()}] | nil) :: Evision.XImgProc.RidgeDetectionFilter.t() | {:error, String.t()}
  def create(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:borderType, :ddepth, :delta, :dx, :dy, :ksize, :out_dtype, :scale])
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_RidgeDetectionFilter_create_static(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Create pointer to the Ridge detection filter.
  ##### Keyword Arguments
  - **ddepth**: `integer()`.

    Specifies output image depth. Defualt is CV_32FC1

  - **dx**: `integer()`.

    Order of derivative x, default is 1

  - **dy**: `integer()`.

    Order of derivative y, default is 1

  - **ksize**: `integer()`.

    Sobel kernel size , default is 3

  - **out_dtype**: `integer()`.

    Converted format for output, default is CV_8UC1

  - **scale**: `double`.

    Optional scale value for derivative values, default is 1

  - **delta**: `double`.

    Optional bias added to output, default is 0

  - **borderType**: `integer()`.

    Pixel extrapolation method, default is BORDER_DEFAULT

  ##### Return
  - **retval**: `RidgeDetectionFilter`

  @see Sobel, threshold, getStructuringElement, morphologyEx.( for additional refinement)

  Python prototype (for reference only):
  ```python3
  create([, ddepth[, dx[, dy[, ksize[, out_dtype[, scale[, delta[, borderType]]]]]]]]) -> retval
  ```
  """
  @spec create() :: Evision.XImgProc.RidgeDetectionFilter.t() | {:error, String.t()}
  def create() do
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_RidgeDetectionFilter_create_static(positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.RidgeDetectionFilter.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.XImgProc.RidgeDetectionFilter.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.ximgproc_RidgeDetectionFilter_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.RidgeDetectionFilter.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.XImgProc.RidgeDetectionFilter.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.ximgproc_RidgeDetectionFilter_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Apply Ridge detection filter on input image.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.RidgeDetectionFilter.t()`
  - **img**: `Evision.Mat`.

    InputArray as supported by Sobel. img can be 1-Channel or 3-Channels.

  ##### Return
  - **out**: `Evision.Mat.t()`.

    OutputAray of structure as RidgeDetectionFilter::ddepth. Output image with ridges.

  Python prototype (for reference only):
  ```python3
  getRidgeFilteredImage(_img[, out]) -> out
  ```
  """
  @spec getRidgeFilteredImage(Evision.XImgProc.RidgeDetectionFilter.t(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def getRidgeFilteredImage(self, img, opts) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img)
    ]
    :evision_nif.ximgproc_ximgproc_RidgeDetectionFilter_getRidgeFilteredImage(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Apply Ridge detection filter on input image.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.RidgeDetectionFilter.t()`
  - **img**: `Evision.Mat`.

    InputArray as supported by Sobel. img can be 1-Channel or 3-Channels.

  ##### Return
  - **out**: `Evision.Mat.t()`.

    OutputAray of structure as RidgeDetectionFilter::ddepth. Output image with ridges.

  Python prototype (for reference only):
  ```python3
  getRidgeFilteredImage(_img[, out]) -> out
  ```
  """
  @spec getRidgeFilteredImage(Evision.XImgProc.RidgeDetectionFilter.t(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def getRidgeFilteredImage(self, img) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img)
    ]
    :evision_nif.ximgproc_ximgproc_RidgeDetectionFilter_getRidgeFilteredImage(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.RidgeDetectionFilter.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.XImgProc.RidgeDetectionFilter.t(), Evision.FileNode.t()) :: Evision.XImgProc.RidgeDetectionFilter.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.ximgproc_RidgeDetectionFilter_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.RidgeDetectionFilter.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.XImgProc.RidgeDetectionFilter.t(), binary()) :: Evision.XImgProc.RidgeDetectionFilter.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.ximgproc_RidgeDetectionFilter_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.RidgeDetectionFilter.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.XImgProc.RidgeDetectionFilter.t(), Evision.FileStorage.t(), binary()) :: Evision.XImgProc.RidgeDetectionFilter.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.ximgproc_RidgeDetectionFilter_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.RidgeDetectionFilter.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.XImgProc.RidgeDetectionFilter.t(), Evision.FileStorage.t()) :: Evision.XImgProc.RidgeDetectionFilter.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.ximgproc_RidgeDetectionFilter_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
