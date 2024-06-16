defmodule Evision.Face.BIF do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Face.BIF` struct.

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
  def to_struct({:ok, %{class: Evision.Face.BIF, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Face.BIF, ref: ref}) do
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
  - **self**: `Evision.Face.BIF.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.Face.BIF.t()) :: Evision.Face.BIF.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.face_BIF_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  compute

  ##### Positional Arguments
  - **self**: `Evision.Face.BIF.t()`
  - **image**: `Evision.Mat`.

    Input image (CV_32FC1).

  ##### Return
  - **features**: `Evision.Mat.t()`.

    Feature vector (CV_32FC1).

  Computes features sby input image.

  Python prototype (for reference only):
  ```python3
  compute(image[, features]) -> features
  ```
  """
  @spec compute(Evision.Face.BIF.t(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def compute(self, image, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.face_face_BIF_compute(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  compute

  ##### Positional Arguments
  - **self**: `Evision.Face.BIF.t()`
  - **image**: `Evision.Mat`.

    Input image (CV_32FC1).

  ##### Return
  - **features**: `Evision.Mat.t()`.

    Feature vector (CV_32FC1).

  Computes features sby input image.

  Python prototype (for reference only):
  ```python3
  compute(image[, features]) -> features
  ```
  """
  @spec compute(Evision.Face.BIF.t(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def compute(self, image) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.face_face_BIF_compute(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  create
  ##### Keyword Arguments
  - **num_bands**: `integer()`.

    The number of filter bands (<=8) used for computing BIF.

  - **num_rotations**: `integer()`.

    The number of image rotations for computing BIF.

  ##### Return
  - **retval**: `BIF`

  @returns Object for computing BIF.

  Python prototype (for reference only):
  ```python3
  create([, num_bands[, num_rotations]]) -> retval
  ```
  """
  @spec create([{:num_bands, term()} | {:num_rotations, term()}] | nil) :: Evision.Face.BIF.t() | {:error, String.t()}
  def create(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:num_bands, :num_rotations])
    positional = [
    ]
    :evision_nif.face_face_BIF_create_static(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  create
  ##### Keyword Arguments
  - **num_bands**: `integer()`.

    The number of filter bands (<=8) used for computing BIF.

  - **num_rotations**: `integer()`.

    The number of image rotations for computing BIF.

  ##### Return
  - **retval**: `BIF`

  @returns Object for computing BIF.

  Python prototype (for reference only):
  ```python3
  create([, num_bands[, num_rotations]]) -> retval
  ```
  """
  @spec create() :: Evision.Face.BIF.t() | {:error, String.t()}
  def create() do
    positional = [
    ]
    :evision_nif.face_face_BIF_create_static(positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.Face.BIF.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.Face.BIF.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.face_BIF_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.Face.BIF.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.Face.BIF.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.face_BIF_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getNumBands

  ##### Positional Arguments
  - **self**: `Evision.Face.BIF.t()`

  ##### Return
  - **retval**: `integer()`

  @returns The number of filter bands used for computing BIF.

  Python prototype (for reference only):
  ```python3
  getNumBands() -> retval
  ```
  """
  @spec getNumBands(Evision.Face.BIF.t()) :: integer() | {:error, String.t()}
  def getNumBands(self) do
    positional = [
    ]
    :evision_nif.face_face_BIF_getNumBands(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getNumRotations

  ##### Positional Arguments
  - **self**: `Evision.Face.BIF.t()`

  ##### Return
  - **retval**: `integer()`

  @returns The number of image rotations.

  Python prototype (for reference only):
  ```python3
  getNumRotations() -> retval
  ```
  """
  @spec getNumRotations(Evision.Face.BIF.t()) :: integer() | {:error, String.t()}
  def getNumRotations(self) do
    positional = [
    ]
    :evision_nif.face_face_BIF_getNumRotations(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.Face.BIF.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.Face.BIF.t(), Evision.FileNode.t()) :: Evision.Face.BIF.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.face_BIF_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.Face.BIF.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.Face.BIF.t(), binary()) :: Evision.Face.BIF.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.face_BIF_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.Face.BIF.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.Face.BIF.t(), Evision.FileStorage.t(), binary()) :: Evision.Face.BIF.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.face_BIF_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.Face.BIF.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.Face.BIF.t(), Evision.FileStorage.t()) :: Evision.Face.BIF.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.face_BIF_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
