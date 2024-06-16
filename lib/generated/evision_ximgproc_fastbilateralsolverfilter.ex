defmodule Evision.XImgProc.FastBilateralSolverFilter do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `XImgProc.FastBilateralSolverFilter` struct.

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
  def to_struct({:ok, %{class: Evision.XImgProc.FastBilateralSolverFilter, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.XImgProc.FastBilateralSolverFilter, ref: ref}) do
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
  - **self**: `Evision.XImgProc.FastBilateralSolverFilter.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.XImgProc.FastBilateralSolverFilter.t()) :: Evision.XImgProc.FastBilateralSolverFilter.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.ximgproc_FastBilateralSolverFilter_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.FastBilateralSolverFilter.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.XImgProc.FastBilateralSolverFilter.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.ximgproc_FastBilateralSolverFilter_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Apply smoothing operation to the source image.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.FastBilateralSolverFilter.t()`
  - **src**: `Evision.Mat`.

    source image for filtering with unsigned 8-bit or signed 16-bit or floating-point 32-bit depth and up to 3 channels.

  - **confidence**: `Evision.Mat`.

    confidence image with unsigned 8-bit or floating-point 32-bit confidence and 1 channel.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    destination image.

  **Note**: Confidence images with CV_8U depth are expected to in [0, 255] and CV_32F in [0, 1] range.

  Python prototype (for reference only):
  ```python3
  filter(src, confidence[, dst]) -> dst
  ```
  """
  @spec filter(Evision.XImgProc.FastBilateralSolverFilter.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def filter(self, src, confidence, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (is_struct(confidence, Evision.Mat) or is_struct(confidence, Nx.Tensor) or is_number(confidence) or is_tuple(confidence)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      confidence: Evision.Internal.Structurise.from_struct(confidence)
    ]
    :evision_nif.ximgproc_ximgproc_FastBilateralSolverFilter_filter(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Apply smoothing operation to the source image.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.FastBilateralSolverFilter.t()`
  - **src**: `Evision.Mat`.

    source image for filtering with unsigned 8-bit or signed 16-bit or floating-point 32-bit depth and up to 3 channels.

  - **confidence**: `Evision.Mat`.

    confidence image with unsigned 8-bit or floating-point 32-bit confidence and 1 channel.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    destination image.

  **Note**: Confidence images with CV_8U depth are expected to in [0, 255] and CV_32F in [0, 1] range.

  Python prototype (for reference only):
  ```python3
  filter(src, confidence[, dst]) -> dst
  ```
  """
  @spec filter(Evision.XImgProc.FastBilateralSolverFilter.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def filter(self, src, confidence) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (is_struct(confidence, Evision.Mat) or is_struct(confidence, Nx.Tensor) or is_number(confidence) or is_tuple(confidence))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      confidence: Evision.Internal.Structurise.from_struct(confidence)
    ]
    :evision_nif.ximgproc_ximgproc_FastBilateralSolverFilter_filter(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.FastBilateralSolverFilter.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.XImgProc.FastBilateralSolverFilter.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.ximgproc_FastBilateralSolverFilter_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.FastBilateralSolverFilter.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.XImgProc.FastBilateralSolverFilter.t(), Evision.FileNode.t()) :: Evision.XImgProc.FastBilateralSolverFilter.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.ximgproc_FastBilateralSolverFilter_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.FastBilateralSolverFilter.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.XImgProc.FastBilateralSolverFilter.t(), binary()) :: Evision.XImgProc.FastBilateralSolverFilter.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.ximgproc_FastBilateralSolverFilter_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.FastBilateralSolverFilter.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.XImgProc.FastBilateralSolverFilter.t(), Evision.FileStorage.t(), binary()) :: Evision.XImgProc.FastBilateralSolverFilter.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.ximgproc_FastBilateralSolverFilter_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.FastBilateralSolverFilter.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.XImgProc.FastBilateralSolverFilter.t(), Evision.FileStorage.t()) :: Evision.XImgProc.FastBilateralSolverFilter.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.ximgproc_FastBilateralSolverFilter_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
