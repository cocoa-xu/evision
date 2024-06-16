defmodule Evision.Tonemap do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Tonemap` struct.

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
  def to_struct({:ok, %{class: Evision.Tonemap, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Tonemap, ref: ref}) do
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
  - **self**: `Evision.Tonemap.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.Tonemap.t()) :: Evision.Tonemap.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.tonemap_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.Tonemap.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.Tonemap.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.tonemap_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.Tonemap.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.Tonemap.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.tonemap_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getGamma

  ##### Positional Arguments
  - **self**: `Evision.Tonemap.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  getGamma() -> retval
  ```
  """
  @spec getGamma(Evision.Tonemap.t()) :: number() | {:error, String.t()}
  def getGamma(self) do
    positional = [
    ]
    :evision_nif.tonemap_getGamma(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Tonemaps image

  ##### Positional Arguments
  - **self**: `Evision.Tonemap.t()`
  - **src**: `Evision.Mat`.

    source image - CV_32FC3 Mat (float 32 bits 3 channels)

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    destination image - CV_32FC3 Mat with values in [0, 1] range

  Python prototype (for reference only):
  ```python3
  process(src[, dst]) -> dst
  ```
  """
  @spec process(Evision.Tonemap.t(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def process(self, src, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.tonemap_process(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Tonemaps image

  ##### Positional Arguments
  - **self**: `Evision.Tonemap.t()`
  - **src**: `Evision.Mat`.

    source image - CV_32FC3 Mat (float 32 bits 3 channels)

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    destination image - CV_32FC3 Mat with values in [0, 1] range

  Python prototype (for reference only):
  ```python3
  process(src[, dst]) -> dst
  ```
  """
  @spec process(Evision.Tonemap.t(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def process(self, src) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.tonemap_process(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.Tonemap.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.Tonemap.t(), Evision.FileNode.t()) :: Evision.Tonemap.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.tonemap_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.Tonemap.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.Tonemap.t(), binary()) :: Evision.Tonemap.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.tonemap_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setGamma

  ##### Positional Arguments
  - **self**: `Evision.Tonemap.t()`
  - **gamma**: `float`

  Python prototype (for reference only):
  ```python3
  setGamma(gamma) -> None
  ```
  """
  @spec setGamma(Evision.Tonemap.t(), number()) :: Evision.Tonemap.t() | {:error, String.t()}
  def setGamma(self, gamma) when is_float(gamma)
  do
    positional = [
      gamma: Evision.Internal.Structurise.from_struct(gamma)
    ]
    :evision_nif.tonemap_setGamma(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.Tonemap.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.Tonemap.t(), Evision.FileStorage.t(), binary()) :: Evision.Tonemap.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.tonemap_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.Tonemap.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.Tonemap.t(), Evision.FileStorage.t()) :: Evision.Tonemap.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.tonemap_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
