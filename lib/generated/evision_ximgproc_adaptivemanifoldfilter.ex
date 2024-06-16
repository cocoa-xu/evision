defmodule Evision.XImgProc.AdaptiveManifoldFilter do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `XImgProc.AdaptiveManifoldFilter` struct.

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
  def to_struct({:ok, %{class: Evision.XImgProc.AdaptiveManifoldFilter, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.XImgProc.AdaptiveManifoldFilter, ref: ref}) do
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
  - **self**: `Evision.XImgProc.AdaptiveManifoldFilter.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.XImgProc.AdaptiveManifoldFilter.t()) :: Evision.XImgProc.AdaptiveManifoldFilter.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.ximgproc_AdaptiveManifoldFilter_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  collectGarbage

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.AdaptiveManifoldFilter.t()`

  Python prototype (for reference only):
  ```python3
  collectGarbage() -> None
  ```
  """
  @spec collectGarbage(Evision.XImgProc.AdaptiveManifoldFilter.t()) :: Evision.XImgProc.AdaptiveManifoldFilter.t() | {:error, String.t()}
  def collectGarbage(self) do
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_AdaptiveManifoldFilter_collectGarbage(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  create
  ##### Return
  - **retval**: `Evision.XImgProc.AdaptiveManifoldFilter.t()`

  Python prototype (for reference only):
  ```python3
  create() -> retval
  ```
  """
  @spec create() :: Evision.XImgProc.AdaptiveManifoldFilter.t() | {:error, String.t()}
  def create() do
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_AdaptiveManifoldFilter_create_static(positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.AdaptiveManifoldFilter.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.XImgProc.AdaptiveManifoldFilter.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.ximgproc_AdaptiveManifoldFilter_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Apply high-dimensional filtering using adaptive manifolds.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.AdaptiveManifoldFilter.t()`
  - **src**: `Evision.Mat`.

    filtering image with any numbers of channels.

  ##### Keyword Arguments
  - **joint**: `Evision.Mat`.

    optional joint (also called as guided) image with any numbers of channels.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    output image.

  Python prototype (for reference only):
  ```python3
  filter(src[, dst[, joint]]) -> dst
  ```
  """
  @spec filter(Evision.XImgProc.AdaptiveManifoldFilter.t(), Evision.Mat.maybe_mat_in(), [{:joint, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def filter(self, src, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:joint])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.ximgproc_ximgproc_AdaptiveManifoldFilter_filter(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Apply high-dimensional filtering using adaptive manifolds.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.AdaptiveManifoldFilter.t()`
  - **src**: `Evision.Mat`.

    filtering image with any numbers of channels.

  ##### Keyword Arguments
  - **joint**: `Evision.Mat`.

    optional joint (also called as guided) image with any numbers of channels.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    output image.

  Python prototype (for reference only):
  ```python3
  filter(src[, dst[, joint]]) -> dst
  ```
  """
  @spec filter(Evision.XImgProc.AdaptiveManifoldFilter.t(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def filter(self, src) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.ximgproc_ximgproc_AdaptiveManifoldFilter_filter(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.AdaptiveManifoldFilter.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.XImgProc.AdaptiveManifoldFilter.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.ximgproc_AdaptiveManifoldFilter_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.AdaptiveManifoldFilter.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.XImgProc.AdaptiveManifoldFilter.t(), Evision.FileNode.t()) :: Evision.XImgProc.AdaptiveManifoldFilter.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.ximgproc_AdaptiveManifoldFilter_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.AdaptiveManifoldFilter.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.XImgProc.AdaptiveManifoldFilter.t(), binary()) :: Evision.XImgProc.AdaptiveManifoldFilter.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.ximgproc_AdaptiveManifoldFilter_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.AdaptiveManifoldFilter.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.XImgProc.AdaptiveManifoldFilter.t(), Evision.FileStorage.t(), binary()) :: Evision.XImgProc.AdaptiveManifoldFilter.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.ximgproc_AdaptiveManifoldFilter_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.AdaptiveManifoldFilter.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.XImgProc.AdaptiveManifoldFilter.t(), Evision.FileStorage.t()) :: Evision.XImgProc.AdaptiveManifoldFilter.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.ximgproc_AdaptiveManifoldFilter_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
