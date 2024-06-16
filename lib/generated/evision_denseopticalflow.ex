defmodule Evision.DenseOpticalFlow do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `DenseOpticalFlow` struct.

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
  def to_struct({:ok, %{class: Evision.DenseOpticalFlow, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.DenseOpticalFlow, ref: ref}) do
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
  Calculates an optical flow.

  ##### Positional Arguments
  - **self**: `Evision.DenseOpticalFlow.t()`
  - **i0**: `Evision.Mat`.

    first 8-bit single-channel input image.

  - **i1**: `Evision.Mat`.

    second input image of the same size and the same type as prev.

  ##### Return
  - **flow**: `Evision.Mat.t()`.

    computed flow image that has the same size as prev and type CV_32FC2.

  Python prototype (for reference only):
  ```python3
  calc(I0, I1, flow) -> flow
  ```
  """
  @spec calc(Evision.DenseOpticalFlow.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def calc(self, i0, i1, flow) when (is_struct(i0, Evision.Mat) or is_struct(i0, Nx.Tensor) or is_number(i0) or is_tuple(i0)) and (is_struct(i1, Evision.Mat) or is_struct(i1, Nx.Tensor) or is_number(i1) or is_tuple(i1)) and (is_struct(flow, Evision.Mat) or is_struct(flow, Nx.Tensor) or is_number(flow) or is_tuple(flow))
  do
    positional = [
      i0: Evision.Internal.Structurise.from_struct(i0),
      i1: Evision.Internal.Structurise.from_struct(i1),
      flow: Evision.Internal.Structurise.from_struct(flow)
    ]
    :evision_nif.denseOpticalFlow_calc(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Clears the algorithm state

  ##### Positional Arguments
  - **self**: `Evision.DenseOpticalFlow.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.DenseOpticalFlow.t()) :: Evision.DenseOpticalFlow.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.denseOpticalFlow_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Releases all inner buffers.

  ##### Positional Arguments
  - **self**: `Evision.DenseOpticalFlow.t()`

  Python prototype (for reference only):
  ```python3
  collectGarbage() -> None
  ```
  """
  @spec collectGarbage(Evision.DenseOpticalFlow.t()) :: Evision.DenseOpticalFlow.t() | {:error, String.t()}
  def collectGarbage(self) do
    positional = [
    ]
    :evision_nif.denseOpticalFlow_collectGarbage(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.DenseOpticalFlow.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.DenseOpticalFlow.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.denseOpticalFlow_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.DenseOpticalFlow.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.DenseOpticalFlow.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.denseOpticalFlow_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.DenseOpticalFlow.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.DenseOpticalFlow.t(), Evision.FileNode.t()) :: Evision.DenseOpticalFlow.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.denseOpticalFlow_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.DenseOpticalFlow.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.DenseOpticalFlow.t(), binary()) :: Evision.DenseOpticalFlow.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.denseOpticalFlow_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.DenseOpticalFlow.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.DenseOpticalFlow.t(), Evision.FileStorage.t(), binary()) :: Evision.DenseOpticalFlow.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.denseOpticalFlow_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.DenseOpticalFlow.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.DenseOpticalFlow.t(), Evision.FileStorage.t()) :: Evision.DenseOpticalFlow.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.denseOpticalFlow_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
