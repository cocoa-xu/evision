defmodule Evision.Legacy.MultiTracker do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Legacy.MultiTracker` struct.

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
  def to_struct({:ok, %{class: Evision.Legacy.MultiTracker, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Legacy.MultiTracker, ref: ref}) do
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
  MultiTracker
  ##### Return
  - **self**: `MultiTracker`

   \\brief Constructor.

  Python prototype (for reference only):
  ```python3
  MultiTracker() -> <legacy_MultiTracker object>
  ```
  """
  @spec multiTracker() :: Evision.Legacy.MultiTracker.t() | {:error, String.t()}
  def multiTracker() do
    positional = [
    ]
    :evision_nif.legacy_legacy_MultiTracker_MultiTracker(positional)
    |> to_struct()
  end

  @doc """
  add

  ##### Positional Arguments
  - **self**: `Evision.Legacy.MultiTracker.t()`
  - **newTracker**: `Evision.Legacy.MultiTracker`.

    tracking algorithm to be used

  - **image**: `Evision.Mat`.

    input image

  - **boundingBox**: `Rect2d`.

    a rectangle represents ROI of the tracked object

  ##### Return
  - **retval**: `bool`

   \\brief Add a new object to be tracked.

  Python prototype (for reference only):
  ```python3
  add(newTracker, image, boundingBox) -> retval
  ```
  """
  @spec add(Evision.Legacy.MultiTracker.t(), Evision.Legacy.MultiTracker.t(), Evision.Mat.maybe_mat_in(), {number(), number(), number(), number()}) :: boolean() | {:error, String.t()}
  def add(self, newTracker, image, boundingBox) when is_struct(newTracker, Evision.Legacy.MultiTracker) and (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and is_tuple(boundingBox)
  do
    positional = [
      newTracker: Evision.Internal.Structurise.from_struct(newTracker),
      image: Evision.Internal.Structurise.from_struct(image),
      boundingBox: Evision.Internal.Structurise.from_struct(boundingBox)
    ]
    :evision_nif.legacy_legacy_MultiTracker_add(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Clears the algorithm state

  ##### Positional Arguments
  - **self**: `Evision.Legacy.MultiTracker.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.Legacy.MultiTracker.t()) :: Evision.Legacy.MultiTracker.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.legacy_MultiTracker_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  create
  ##### Return
  - **retval**: `MultiTracker`

   \\brief Returns a pointer to a new instance of MultiTracker

  Python prototype (for reference only):
  ```python3
  create() -> retval
  ```
  """
  @spec create() :: Evision.Legacy.MultiTracker.t() | {:error, String.t()}
  def create() do
    positional = [
    ]
    :evision_nif.legacy_legacy_MultiTracker_create_static(positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.Legacy.MultiTracker.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.Legacy.MultiTracker.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.legacy_MultiTracker_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.Legacy.MultiTracker.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.Legacy.MultiTracker.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.legacy_MultiTracker_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getObjects

  ##### Positional Arguments
  - **self**: `Evision.Legacy.MultiTracker.t()`

  ##### Return
  - **retval**: `[Rect2d]`

   \\brief Returns a reference to a storage for the tracked objects, each object corresponds to one tracker algorithm

  Python prototype (for reference only):
  ```python3
  getObjects() -> retval
  ```
  """
  @spec getObjects(Evision.Legacy.MultiTracker.t()) :: list({number(), number(), number(), number()}) | {:error, String.t()}
  def getObjects(self) do
    positional = [
    ]
    :evision_nif.legacy_legacy_MultiTracker_getObjects(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.Legacy.MultiTracker.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.Legacy.MultiTracker.t(), Evision.FileNode.t()) :: Evision.Legacy.MultiTracker.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.legacy_MultiTracker_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.Legacy.MultiTracker.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.Legacy.MultiTracker.t(), binary()) :: Evision.Legacy.MultiTracker.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.legacy_MultiTracker_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  update

  ##### Positional Arguments
  - **self**: `Evision.Legacy.MultiTracker.t()`
  - **image**: `Evision.Mat`.

    input image

  ##### Return
  - **retval**: `bool`
  - **boundingBox**: `[Rect2d]`.

    the tracking result, represent a list of ROIs of the tracked objects.

   \\brief Update the current tracking status.

  Python prototype (for reference only):
  ```python3
  update(image) -> retval, boundingBox
  ```
  """
  @spec update(Evision.Legacy.MultiTracker.t(), Evision.Mat.maybe_mat_in()) :: list({number(), number(), number(), number()}) | false | {:error, String.t()}
  def update(self, image) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.legacy_legacy_MultiTracker_update(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.Legacy.MultiTracker.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.Legacy.MultiTracker.t(), Evision.FileStorage.t(), binary()) :: Evision.Legacy.MultiTracker.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.legacy_MultiTracker_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.Legacy.MultiTracker.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.Legacy.MultiTracker.t(), Evision.FileStorage.t()) :: Evision.Legacy.MultiTracker.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.legacy_MultiTracker_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
