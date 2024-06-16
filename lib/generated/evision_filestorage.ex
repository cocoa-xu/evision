defmodule Evision.FileStorage do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `FileStorage` struct.

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
  def to_struct({:ok, %{class: Evision.FileStorage, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.FileStorage, ref: ref}) do
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
  FileStorage

  ##### Positional Arguments
  - **filename**: `String`
  - **flags**: `integer()`

  ##### Keyword Arguments
  - **encoding**: `String`.

  ##### Return
  - **self**: `Evision.FileStorage.t()`

  Has overloading in C++

  @copydoc open()

  Python prototype (for reference only):
  ```python3
  FileStorage(filename, flags[, encoding]) -> <FileStorage object>
  ```
  """
  @spec fileStorage(binary(), integer(), [{:encoding, term()}] | nil) :: Evision.FileStorage.t() | {:error, String.t()}
  def fileStorage(filename, flags, opts) when is_binary(filename) and is_integer(flags) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:encoding])
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename),
      flags: Evision.Internal.Structurise.from_struct(flags)
    ]
    :evision_nif.fileStorage_FileStorage(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  FileStorage

  ##### Positional Arguments
  - **filename**: `String`
  - **flags**: `integer()`

  ##### Keyword Arguments
  - **encoding**: `String`.

  ##### Return
  - **self**: `Evision.FileStorage.t()`

  Has overloading in C++

  @copydoc open()

  Python prototype (for reference only):
  ```python3
  FileStorage(filename, flags[, encoding]) -> <FileStorage object>
  ```
  """
  @spec fileStorage(binary(), integer()) :: Evision.FileStorage.t() | {:error, String.t()}
  def fileStorage(filename, flags) when is_binary(filename) and is_integer(flags)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename),
      flags: Evision.Internal.Structurise.from_struct(flags)
    ]
    :evision_nif.fileStorage_FileStorage(positional)
    |> to_struct()
  end

  @doc """
  The constructors.
  ##### Return
  - **self**: `Evision.FileStorage.t()`

  The full constructor opens the file. Alternatively you can use the default constructor and then
  call FileStorage::open.

  Python prototype (for reference only):
  ```python3
  FileStorage() -> <FileStorage object>
  ```
  """
  @spec fileStorage() :: Evision.FileStorage.t() | {:error, String.t()}
  def fileStorage() do
    positional = [
    ]
    :evision_nif.fileStorage_FileStorage(positional)
    |> to_struct()
  end

  @doc """
  Finishes writing nested structure (should pair startWriteStruct())

  ##### Positional Arguments
  - **self**: `Evision.FileStorage.t()`

  Python prototype (for reference only):
  ```python3
  endWriteStruct() -> None
  ```
  """
  @spec endWriteStruct(Evision.FileStorage.t()) :: Evision.FileStorage.t() | {:error, String.t()}
  def endWriteStruct(self) do
    positional = [
    ]
    :evision_nif.fileStorage_endWriteStruct(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns the first element of the top-level mapping.

  ##### Positional Arguments
  - **self**: `Evision.FileStorage.t()`

  ##### Return
  - **retval**: `Evision.FileNode.t()`

  @returns The first element of the top-level mapping.

  Python prototype (for reference only):
  ```python3
  getFirstTopLevelNode() -> retval
  ```
  """
  @spec getFirstTopLevelNode(Evision.FileStorage.t()) :: Evision.FileNode.t() | {:error, String.t()}
  def getFirstTopLevelNode(self) do
    positional = [
    ]
    :evision_nif.fileStorage_getFirstTopLevelNode(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns the current format.

  ##### Positional Arguments
  - **self**: `Evision.FileStorage.t()`

  ##### Return
  - **retval**: `integer()`

  @returns The current format, see FileStorage::Mode

  Python prototype (for reference only):
  ```python3
  getFormat() -> retval
  ```
  """
  @spec getFormat(Evision.FileStorage.t()) :: integer() | {:error, String.t()}
  def getFormat(self) do
    positional = [
    ]
    :evision_nif.fileStorage_getFormat(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getNode

  ##### Positional Arguments
  - **self**: `Evision.FileStorage.t()`
  - **nodename**: `c_string`

  ##### Return
  - **retval**: `Evision.FileNode.t()`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  getNode(nodename) -> retval
  ```
  """
  @spec getNode(Evision.FileStorage.t(), binary()) :: Evision.FileNode.t() | {:error, String.t()}
  def getNode(self, nodename) when is_binary(nodename)
  do
    positional = [
      nodename: Evision.Internal.Structurise.from_struct(nodename)
    ]
    :evision_nif.fileStorage_getNode(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Checks whether the file is opened.

  ##### Positional Arguments
  - **self**: `Evision.FileStorage.t()`

  ##### Return
  - **retval**: `bool`

  @returns true if the object is associated with the current file and false otherwise. It is a
  good practice to call this method after you tried to open a file.

  Python prototype (for reference only):
  ```python3
  isOpened() -> retval
  ```
  """
  @spec isOpened(Evision.FileStorage.t()) :: boolean() | {:error, String.t()}
  def isOpened(self) do
    positional = [
    ]
    :evision_nif.fileStorage_isOpened(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Opens a file.

  ##### Positional Arguments
  - **self**: `Evision.FileStorage.t()`
  - **filename**: `String`.

    Name of the file to open or the text string to read the data from.
    Extension of the file (.xml, .yml/.yaml or .json) determines its format (XML, YAML or JSON
    respectively). Also you can append .gz to work with compressed files, for example myHugeMatrix.xml.gz. If both
    FileStorage::WRITE and FileStorage::MEMORY flags are specified, source is used just to specify
    the output file format (e.g. mydata.xml, .yml etc.). A file name can also contain parameters.
    You can use this format, "*?base64" (e.g. "file.json?base64" (case sensitive)), as an alternative to
    FileStorage::BASE64 flag.

  - **flags**: `integer()`.

    Mode of operation. One of FileStorage::Mode

  ##### Keyword Arguments
  - **encoding**: `String`.

    Encoding of the file. Note that UTF-16 XML encoding is not supported currently and
    you should use 8-bit encoding instead of it.

  ##### Return
  - **retval**: `bool`

  See description of parameters in FileStorage::FileStorage. The method calls FileStorage::release
  before opening the file.

  Python prototype (for reference only):
  ```python3
  open(filename, flags[, encoding]) -> retval
  ```
  """
  @spec open(Evision.FileStorage.t(), binary(), integer(), [{:encoding, term()}] | nil) :: boolean() | {:error, String.t()}
  def open(self, filename, flags, opts) when is_binary(filename) and is_integer(flags) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:encoding])
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename),
      flags: Evision.Internal.Structurise.from_struct(flags)
    ]
    :evision_nif.fileStorage_open(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Opens a file.

  ##### Positional Arguments
  - **self**: `Evision.FileStorage.t()`
  - **filename**: `String`.

    Name of the file to open or the text string to read the data from.
    Extension of the file (.xml, .yml/.yaml or .json) determines its format (XML, YAML or JSON
    respectively). Also you can append .gz to work with compressed files, for example myHugeMatrix.xml.gz. If both
    FileStorage::WRITE and FileStorage::MEMORY flags are specified, source is used just to specify
    the output file format (e.g. mydata.xml, .yml etc.). A file name can also contain parameters.
    You can use this format, "*?base64" (e.g. "file.json?base64" (case sensitive)), as an alternative to
    FileStorage::BASE64 flag.

  - **flags**: `integer()`.

    Mode of operation. One of FileStorage::Mode

  ##### Keyword Arguments
  - **encoding**: `String`.

    Encoding of the file. Note that UTF-16 XML encoding is not supported currently and
    you should use 8-bit encoding instead of it.

  ##### Return
  - **retval**: `bool`

  See description of parameters in FileStorage::FileStorage. The method calls FileStorage::release
  before opening the file.

  Python prototype (for reference only):
  ```python3
  open(filename, flags[, encoding]) -> retval
  ```
  """
  @spec open(Evision.FileStorage.t(), binary(), integer()) :: boolean() | {:error, String.t()}
  def open(self, filename, flags) when is_binary(filename) and is_integer(flags)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename),
      flags: Evision.Internal.Structurise.from_struct(flags)
    ]
    :evision_nif.fileStorage_open(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Closes the file and releases all the memory buffers.

  ##### Positional Arguments
  - **self**: `Evision.FileStorage.t()`

  Call this method after all I/O operations with the storage are finished.

  Python prototype (for reference only):
  ```python3
  release() -> None
  ```
  """
  @spec release(Evision.FileStorage.t()) :: Evision.FileStorage.t() | {:error, String.t()}
  def release(self) do
    positional = [
    ]
    :evision_nif.fileStorage_release(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Closes the file and releases all the memory buffers.

  ##### Positional Arguments
  - **self**: `Evision.FileStorage.t()`

  ##### Return
  - **retval**: `String`

  Call this method after all I/O operations with the storage are finished. If the storage was
  opened for writing data and FileStorage::WRITE was specified

  Python prototype (for reference only):
  ```python3
  releaseAndGetString() -> retval
  ```
  """
  @spec releaseAndGetString(Evision.FileStorage.t()) :: binary() | {:error, String.t()}
  def releaseAndGetString(self) do
    positional = [
    ]
    :evision_nif.fileStorage_releaseAndGetString(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns the top-level mapping

  ##### Positional Arguments
  - **self**: `Evision.FileStorage.t()`

  ##### Keyword Arguments
  - **streamidx**: `integer()`.

    Zero-based index of the stream. In most cases there is only one stream in the file.
    However, YAML supports multiple streams and so there can be several.

  ##### Return
  - **retval**: `Evision.FileNode.t()`

  @returns The top-level mapping.

  Python prototype (for reference only):
  ```python3
  root([, streamidx]) -> retval
  ```
  """
  @spec root(Evision.FileStorage.t(), [{:streamidx, term()}] | nil) :: Evision.FileNode.t() | {:error, String.t()}
  def root(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:streamidx])
    positional = [
    ]
    :evision_nif.fileStorage_root(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Returns the top-level mapping

  ##### Positional Arguments
  - **self**: `Evision.FileStorage.t()`

  ##### Keyword Arguments
  - **streamidx**: `integer()`.

    Zero-based index of the stream. In most cases there is only one stream in the file.
    However, YAML supports multiple streams and so there can be several.

  ##### Return
  - **retval**: `Evision.FileNode.t()`

  @returns The top-level mapping.

  Python prototype (for reference only):
  ```python3
  root([, streamidx]) -> retval
  ```
  """
  @spec root(Evision.FileStorage.t()) :: Evision.FileNode.t() | {:error, String.t()}
  def root(self) do
    positional = [
    ]
    :evision_nif.fileStorage_root(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Starts to write a nested structure (sequence or a mapping).

  ##### Positional Arguments
  - **self**: `Evision.FileStorage.t()`
  - **name**: `String`.

    name of the structure. When writing to sequences (a.k.a. "arrays"), pass an empty string.

  - **flags**: `integer()`.

    type of the structure (FileNode::MAP or FileNode::SEQ (both with optional FileNode::FLOW)).

  ##### Keyword Arguments
  - **typeName**: `String`.

    optional name of the type you store. The effect of setting this depends on the storage format.
    I.e. if the format has a specification for storing type information, this parameter is used.

  Python prototype (for reference only):
  ```python3
  startWriteStruct(name, flags[, typeName]) -> None
  ```
  """
  @spec startWriteStruct(Evision.FileStorage.t(), binary(), integer(), [{:typeName, term()}] | nil) :: Evision.FileStorage.t() | {:error, String.t()}
  def startWriteStruct(self, name, flags, opts) when is_binary(name) and is_integer(flags) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:typeName])
    positional = [
      name: Evision.Internal.Structurise.from_struct(name),
      flags: Evision.Internal.Structurise.from_struct(flags)
    ]
    :evision_nif.fileStorage_startWriteStruct(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Starts to write a nested structure (sequence or a mapping).

  ##### Positional Arguments
  - **self**: `Evision.FileStorage.t()`
  - **name**: `String`.

    name of the structure. When writing to sequences (a.k.a. "arrays"), pass an empty string.

  - **flags**: `integer()`.

    type of the structure (FileNode::MAP or FileNode::SEQ (both with optional FileNode::FLOW)).

  ##### Keyword Arguments
  - **typeName**: `String`.

    optional name of the type you store. The effect of setting this depends on the storage format.
    I.e. if the format has a specification for storing type information, this parameter is used.

  Python prototype (for reference only):
  ```python3
  startWriteStruct(name, flags[, typeName]) -> None
  ```
  """
  @spec startWriteStruct(Evision.FileStorage.t(), binary(), integer()) :: Evision.FileStorage.t() | {:error, String.t()}
  def startWriteStruct(self, name, flags) when is_binary(name) and is_integer(flags)
  do
    positional = [
      name: Evision.Internal.Structurise.from_struct(name),
      flags: Evision.Internal.Structurise.from_struct(flags)
    ]
    :evision_nif.fileStorage_startWriteStruct(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  write

  ##### Positional Arguments
  - **self**: `Evision.FileStorage.t()`
  - **name**: `String`
  - **val**: `[String]`

  Python prototype (for reference only):
  ```python3
  write(name, val) -> None
  ```
  #### Variant 2:
  write

  ##### Positional Arguments
  - **self**: `Evision.FileStorage.t()`
  - **name**: `String`
  - **val**: `Evision.Mat`

  Python prototype (for reference only):
  ```python3
  write(name, val) -> None
  ```
  #### Variant 3:
  write

  ##### Positional Arguments
  - **self**: `Evision.FileStorage.t()`
  - **name**: `String`
  - **val**: `String`

  Python prototype (for reference only):
  ```python3
  write(name, val) -> None
  ```
  #### Variant 4:
  write

  ##### Positional Arguments
  - **self**: `Evision.FileStorage.t()`
  - **name**: `String`
  - **val**: `double`

  Python prototype (for reference only):
  ```python3
  write(name, val) -> None
  ```
  #### Variant 5:
  Simplified writing API to use with bindings.

  ##### Positional Arguments
  - **self**: `Evision.FileStorage.t()`
  - **name**: `String`.

    Name of the written object. When writing to sequences (a.k.a. "arrays"), pass an empty string.

  - **val**: `integer()`.

    Value of the written object.

  Python prototype (for reference only):
  ```python3
  write(name, val) -> None
  ```

  """
  @spec write(Evision.FileStorage.t(), binary(), list(binary())) :: Evision.FileStorage.t() | {:error, String.t()}
  def write(self, name, val) when is_binary(name) and is_list(val)
  do
    positional = [
      name: Evision.Internal.Structurise.from_struct(name),
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.fileStorage_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec write(Evision.FileStorage.t(), binary(), Evision.Mat.maybe_mat_in()) :: Evision.FileStorage.t() | {:error, String.t()}
  def write(self, name, val) when is_binary(name) and (is_struct(val, Evision.Mat) or is_struct(val, Nx.Tensor) or is_number(val) or is_tuple(val))
  do
    positional = [
      name: Evision.Internal.Structurise.from_struct(name),
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.fileStorage_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec write(Evision.FileStorage.t(), binary(), binary()) :: Evision.FileStorage.t() | {:error, String.t()}
  def write(self, name, val) when is_binary(name) and is_binary(val)
  do
    positional = [
      name: Evision.Internal.Structurise.from_struct(name),
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.fileStorage_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec write(Evision.FileStorage.t(), binary(), number()) :: Evision.FileStorage.t() | {:error, String.t()}
  def write(self, name, val) when is_binary(name) and is_number(val)
  do
    positional = [
      name: Evision.Internal.Structurise.from_struct(name),
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.fileStorage_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec write(Evision.FileStorage.t(), binary(), integer()) :: Evision.FileStorage.t() | {:error, String.t()}
  def write(self, name, val) when is_binary(name) and is_integer(val)
  do
    positional = [
      name: Evision.Internal.Structurise.from_struct(name),
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.fileStorage_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Writes a comment.

  ##### Positional Arguments
  - **self**: `Evision.FileStorage.t()`
  - **comment**: `String`.

    The written comment, single-line or multi-line

  ##### Keyword Arguments
  - **append**: `bool`.

    If true, the function tries to put the comment at the end of current line.
    Else if the comment is multi-line, or if it does not fit at the end of the current
    line, the comment starts a new line.

  The function writes a comment into file storage. The comments are skipped when the storage is read.

  Python prototype (for reference only):
  ```python3
  writeComment(comment[, append]) -> None
  ```
  """
  @spec writeComment(Evision.FileStorage.t(), binary(), [{:append, term()}] | nil) :: Evision.FileStorage.t() | {:error, String.t()}
  def writeComment(self, comment, opts) when is_binary(comment) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:append])
    positional = [
      comment: Evision.Internal.Structurise.from_struct(comment)
    ]
    :evision_nif.fileStorage_writeComment(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Writes a comment.

  ##### Positional Arguments
  - **self**: `Evision.FileStorage.t()`
  - **comment**: `String`.

    The written comment, single-line or multi-line

  ##### Keyword Arguments
  - **append**: `bool`.

    If true, the function tries to put the comment at the end of current line.
    Else if the comment is multi-line, or if it does not fit at the end of the current
    line, the comment starts a new line.

  The function writes a comment into file storage. The comments are skipped when the storage is read.

  Python prototype (for reference only):
  ```python3
  writeComment(comment[, append]) -> None
  ```
  """
  @spec writeComment(Evision.FileStorage.t(), binary()) :: Evision.FileStorage.t() | {:error, String.t()}
  def writeComment(self, comment) when is_binary(comment)
  do
    positional = [
      comment: Evision.Internal.Structurise.from_struct(comment)
    ]
    :evision_nif.fileStorage_writeComment(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
