defmodule Evision.ShapeDistanceExtractor do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `ShapeDistanceExtractor` struct.

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
  def to_struct({:ok, %{class: Evision.ShapeDistanceExtractor, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.ShapeDistanceExtractor, ref: ref}) do
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
  - **self**: `Evision.ShapeDistanceExtractor.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.ShapeDistanceExtractor.t()) :: Evision.ShapeDistanceExtractor.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.shapeDistanceExtractor_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Compute the shape distance between two shapes defined by its contours.

  ##### Positional Arguments
  - **self**: `Evision.ShapeDistanceExtractor.t()`
  - **contour1**: `Evision.Mat`.

    Contour defining first shape.

  - **contour2**: `Evision.Mat`.

    Contour defining second shape.

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  computeDistance(contour1, contour2) -> retval
  ```
  """
  @spec computeDistance(Evision.ShapeDistanceExtractor.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: number() | {:error, String.t()}
  def computeDistance(self, contour1, contour2) when (is_struct(contour1, Evision.Mat) or is_struct(contour1, Nx.Tensor) or is_number(contour1) or is_tuple(contour1)) and (is_struct(contour2, Evision.Mat) or is_struct(contour2, Nx.Tensor) or is_number(contour2) or is_tuple(contour2))
  do
    positional = [
      contour1: Evision.Internal.Structurise.from_struct(contour1),
      contour2: Evision.Internal.Structurise.from_struct(contour2)
    ]
    :evision_nif.shapeDistanceExtractor_computeDistance(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.ShapeDistanceExtractor.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.ShapeDistanceExtractor.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.shapeDistanceExtractor_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.ShapeDistanceExtractor.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.ShapeDistanceExtractor.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.shapeDistanceExtractor_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.ShapeDistanceExtractor.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.ShapeDistanceExtractor.t(), Evision.FileNode.t()) :: Evision.ShapeDistanceExtractor.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.shapeDistanceExtractor_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.ShapeDistanceExtractor.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.ShapeDistanceExtractor.t(), binary()) :: Evision.ShapeDistanceExtractor.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.shapeDistanceExtractor_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.ShapeDistanceExtractor.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.ShapeDistanceExtractor.t(), Evision.FileStorage.t(), binary()) :: Evision.ShapeDistanceExtractor.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.shapeDistanceExtractor_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.ShapeDistanceExtractor.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.ShapeDistanceExtractor.t(), Evision.FileStorage.t()) :: Evision.ShapeDistanceExtractor.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.shapeDistanceExtractor_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
