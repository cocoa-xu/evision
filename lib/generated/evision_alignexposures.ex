defmodule Evision.AlignExposures do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `AlignExposures` struct.

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
  def to_struct({:ok, %{class: Evision.AlignExposures, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.AlignExposures, ref: ref}) do
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
  - **self**: `Evision.AlignExposures.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.AlignExposures.t()) :: Evision.AlignExposures.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.alignExposures_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.AlignExposures.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.AlignExposures.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.alignExposures_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.AlignExposures.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.AlignExposures.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.alignExposures_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Aligns images

  ##### Positional Arguments
  - **self**: `Evision.AlignExposures.t()`
  - **src**: `[Evision.Mat]`.

    vector of input images

  - **dst**: `[Evision.Mat]`.

    vector of aligned images

  - **times**: `Evision.Mat`.

    vector of exposure time values for each image

  - **response**: `Evision.Mat`.

    256x1 matrix with inverse camera response function for each pixel value, it should
    have the same number of channels as images.

  Python prototype (for reference only):
  ```python3
  process(src, dst, times, response) -> None
  ```
  """
  @spec process(Evision.AlignExposures.t(), list(Evision.Mat.maybe_mat_in()), list(Evision.Mat.maybe_mat_in()), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.AlignExposures.t() | {:error, String.t()}
  def process(self, src, dst, times, response) when is_list(src) and is_list(dst) and (is_struct(times, Evision.Mat) or is_struct(times, Nx.Tensor) or is_number(times) or is_tuple(times)) and (is_struct(response, Evision.Mat) or is_struct(response, Nx.Tensor) or is_number(response) or is_tuple(response))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      dst: Evision.Internal.Structurise.from_struct(dst),
      times: Evision.Internal.Structurise.from_struct(times),
      response: Evision.Internal.Structurise.from_struct(response)
    ]
    :evision_nif.alignExposures_process(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.AlignExposures.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.AlignExposures.t(), Evision.FileNode.t()) :: Evision.AlignExposures.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.alignExposures_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.AlignExposures.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.AlignExposures.t(), binary()) :: Evision.AlignExposures.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.alignExposures_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.AlignExposures.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.AlignExposures.t(), Evision.FileStorage.t(), binary()) :: Evision.AlignExposures.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.alignExposures_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.AlignExposures.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.AlignExposures.t(), Evision.FileStorage.t()) :: Evision.AlignExposures.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.alignExposures_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
