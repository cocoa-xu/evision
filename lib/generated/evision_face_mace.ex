defmodule Evision.Face.MACE do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Face.MACE` struct.

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
  def to_struct({:ok, %{class: Evision.Face.MACE, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Face.MACE, ref: ref}) do
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
  - **self**: `Evision.Face.MACE.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.Face.MACE.t()) :: Evision.Face.MACE.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.face_MACE_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  constructor
  ##### Keyword Arguments
  - **iMGSIZE**: `integer()`.

    images will get resized to this (should be an even number)

  ##### Return
  - **retval**: `cv::Ptr<MACE>`

  Python prototype (for reference only):
  ```python3
  create([, IMGSIZE]) -> retval
  ```
  """
  @spec create([{:iMGSIZE, term()}] | nil) :: Evision.Face.MACE.t() | {:error, String.t()}
  def create(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:iMGSIZE])
    positional = [
    ]
    :evision_nif.face_face_MACE_create_static(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  constructor
  ##### Keyword Arguments
  - **iMGSIZE**: `integer()`.

    images will get resized to this (should be an even number)

  ##### Return
  - **retval**: `cv::Ptr<MACE>`

  Python prototype (for reference only):
  ```python3
  create([, IMGSIZE]) -> retval
  ```
  """
  @spec create() :: Evision.Face.MACE.t() | {:error, String.t()}
  def create() do
    positional = [
    ]
    :evision_nif.face_face_MACE_create_static(positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.Face.MACE.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.Face.MACE.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.face_MACE_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.Face.MACE.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.Face.MACE.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.face_MACE_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  constructor

  ##### Positional Arguments
  - **filename**: `String`.

    build a new MACE instance from a pre-serialized FileStorage

  ##### Keyword Arguments
  - **objname**: `String`.

    (optional) top-level node in the FileStorage

  ##### Return
  - **retval**: `cv::Ptr<MACE>`

  Python prototype (for reference only):
  ```python3
  load(filename[, objname]) -> retval
  ```
  """
  @spec load(binary(), [{:objname, term()}] | nil) :: Evision.Face.MACE.t() | {:error, String.t()}
  def load(filename, opts) when is_binary(filename) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:objname])
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.face_face_MACE_load_static(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  constructor

  ##### Positional Arguments
  - **filename**: `String`.

    build a new MACE instance from a pre-serialized FileStorage

  ##### Keyword Arguments
  - **objname**: `String`.

    (optional) top-level node in the FileStorage

  ##### Return
  - **retval**: `cv::Ptr<MACE>`

  Python prototype (for reference only):
  ```python3
  load(filename[, objname]) -> retval
  ```
  """
  @spec load(binary()) :: Evision.Face.MACE.t() | {:error, String.t()}
  def load(filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.face_face_MACE_load_static(positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.Face.MACE.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.Face.MACE.t(), Evision.FileNode.t()) :: Evision.Face.MACE.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.face_MACE_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  optionally encrypt images with random convolution

  ##### Positional Arguments
  - **self**: `Evision.Face.MACE.t()`
  - **passphrase**: `String`.

    a crc64 random seed will get generated from this

  Python prototype (for reference only):
  ```python3
  salt(passphrase) -> None
  ```
  """
  @spec salt(Evision.Face.MACE.t(), binary()) :: Evision.Face.MACE.t() | {:error, String.t()}
  def salt(self, passphrase) when is_binary(passphrase)
  do
    positional = [
      passphrase: Evision.Internal.Structurise.from_struct(passphrase)
    ]
    :evision_nif.face_face_MACE_salt(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  correlate query img and threshold to min class value

  ##### Positional Arguments
  - **self**: `Evision.Face.MACE.t()`
  - **query**: `Evision.Mat`.

    a Mat with query image

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  same(query) -> retval
  ```
  """
  @spec same(Evision.Face.MACE.t(), Evision.Mat.maybe_mat_in()) :: boolean() | {:error, String.t()}
  def same(self, query) when (is_struct(query, Evision.Mat) or is_struct(query, Nx.Tensor) or is_number(query) or is_tuple(query))
  do
    positional = [
      query: Evision.Internal.Structurise.from_struct(query)
    ]
    :evision_nif.face_face_MACE_same(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.Face.MACE.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.Face.MACE.t(), binary()) :: Evision.Face.MACE.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.face_MACE_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  train it on positive features
  compute the mace filter: `h = D(-1) * X * (X(+) * D(-1) * X)(-1) * C`
  also calculate a minimal threshold for this class, the smallest self-similarity from the train images

  ##### Positional Arguments
  - **self**: `Evision.Face.MACE.t()`
  - **images**: `[Evision.Mat]`.

    a vector<Mat> with the train images

  Python prototype (for reference only):
  ```python3
  train(images) -> None
  ```
  """
  @spec train(Evision.Face.MACE.t(), list(Evision.Mat.maybe_mat_in())) :: Evision.Face.MACE.t() | {:error, String.t()}
  def train(self, images) when is_list(images)
  do
    positional = [
      images: Evision.Internal.Structurise.from_struct(images)
    ]
    :evision_nif.face_face_MACE_train(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.Face.MACE.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.Face.MACE.t(), Evision.FileStorage.t(), binary()) :: Evision.Face.MACE.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.face_MACE_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.Face.MACE.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.Face.MACE.t(), Evision.FileStorage.t()) :: Evision.Face.MACE.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.face_MACE_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
