defmodule Evision.Legacy.Tracker do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Legacy.Tracker` struct.

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
  def to_struct({:ok, %{class: Evision.Legacy.Tracker, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Legacy.Tracker, ref: ref}) do
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
  - **self**: `Evision.Legacy.Tracker.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.Legacy.MultiTracker.t()) :: Evision.Legacy.MultiTracker.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.legacy_Tracker_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.Legacy.Tracker.t()`

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
    :evision_nif.legacy_Tracker_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.Legacy.Tracker.t()`

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
    :evision_nif.legacy_Tracker_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Initialize the tracker with a known bounding box that surrounded the target

  ##### Positional Arguments
  - **self**: `Evision.Legacy.Tracker.t()`
  - **image**: `Evision.Mat`.

    The initial frame

  - **boundingBox**: `Rect2d`.

    The initial bounding box

  ##### Return
  - **retval**: `bool`

  @return True if initialization went succesfully, false otherwise

  Python prototype (for reference only):
  ```python3
  init(image, boundingBox) -> retval
  ```
  """
  @spec init(Evision.Legacy.MultiTracker.t(), Evision.Mat.maybe_mat_in(), {number(), number(), number(), number()}) :: boolean() | {:error, String.t()}
  def init(self, image, boundingBox) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and is_tuple(boundingBox)
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      boundingBox: Evision.Internal.Structurise.from_struct(boundingBox)
    ]
    :evision_nif.legacy_legacy_Tracker_init(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.Legacy.Tracker.t()`
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
    :evision_nif.legacy_Tracker_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.Legacy.Tracker.t()`
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
    :evision_nif.legacy_Tracker_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Update the tracker, find the new most likely bounding box for the target

  ##### Positional Arguments
  - **self**: `Evision.Legacy.Tracker.t()`
  - **image**: `Evision.Mat`.

    The current frame

  ##### Return
  - **retval**: `bool`
  - **boundingBox**: `Rect2d`.

    The bounding box that represent the new target location, if true was returned, not
    modified otherwise

  @return True means that target was located and false means that tracker cannot locate target in
  current frame. Note, that latter *does not* imply that tracker has failed, maybe target is indeed
  missing from the frame (say, out of sight)

  Python prototype (for reference only):
  ```python3
  update(image) -> retval, boundingBox
  ```
  """
  @spec update(Evision.Legacy.MultiTracker.t(), Evision.Mat.maybe_mat_in()) :: {number(), number(), number(), number()} | false | {:error, String.t()}
  def update(self, image) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.legacy_legacy_Tracker_update(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.Legacy.Tracker.t()`
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
    :evision_nif.legacy_Tracker_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.Legacy.Tracker.t()`
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
    :evision_nif.legacy_Tracker_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
