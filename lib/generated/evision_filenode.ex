defmodule Evision.FileNode do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `FileNode` struct.

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
  def to_struct({:ok, %{class: Evision.FileNode, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.FileNode, ref: ref}) do
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
  @type enum :: integer()
  @doc enum: true
  def cv_NONE, do: 0
  @doc enum: true
  def cv_INT, do: 1
  @doc enum: true
  def cv_REAL, do: 2
  @doc enum: true
  def cv_FLOAT, do: cv_REAL()
  @doc enum: true
  def cv_STR, do: 3
  @doc enum: true
  def cv_STRING, do: cv_STR()
  @doc enum: true
  def cv_SEQ, do: 4
  @doc enum: true
  def cv_MAP, do: 5
  @doc enum: true
  def cv_TYPE_MASK, do: 7
  @doc enum: true
  def cv_FLOW, do: 8
  @doc enum: true
  def cv_UNIFORM, do: 8
  @doc enum: true
  def cv_EMPTY, do: 16
  @doc enum: true
  def cv_NAMED, do: 32


  @doc """
  The constructors.
  ##### Return
  - **self**: `Evision.FileNode.t()`

  These constructors are used to create a default file node, construct it from obsolete structures or
  from the another file node.

  Python prototype (for reference only):
  ```python3
  FileNode() -> <FileNode object>
  ```
  """
  @spec fileNode() :: Evision.FileNode.t() | {:error, String.t()}
  def fileNode() do
    positional = [
    ]
    :evision_nif.fileNode_FileNode(positional)
    |> to_struct()
  end

  @doc """
  at

  ##### Positional Arguments
  - **self**: `Evision.FileNode.t()`
  - **i**: `integer()`.

    Index of an element in the sequence node.

  ##### Return
  - **retval**: `Evision.FileNode.t()`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  at(i) -> retval
  ```
  """
  @spec at(Evision.FileNode.t(), integer()) :: Evision.FileNode.t() | {:error, String.t()}
  def at(self, i) when is_integer(i)
  do
    positional = [
      i: Evision.Internal.Structurise.from_struct(i)
    ]
    :evision_nif.fileNode_at(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  empty

  ##### Positional Arguments
  - **self**: `Evision.FileNode.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.FileNode.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.fileNode_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getNode

  ##### Positional Arguments
  - **self**: `Evision.FileNode.t()`
  - **nodename**: `c_string`.

    Name of an element in the mapping node.

  ##### Return
  - **retval**: `Evision.FileNode.t()`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  getNode(nodename) -> retval
  ```
  """
  @spec getNode(Evision.FileNode.t(), binary()) :: Evision.FileNode.t() | {:error, String.t()}
  def getNode(self, nodename) when is_binary(nodename)
  do
    positional = [
      nodename: Evision.Internal.Structurise.from_struct(nodename)
    ]
    :evision_nif.fileNode_getNode(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  isInt

  ##### Positional Arguments
  - **self**: `Evision.FileNode.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  isInt() -> retval
  ```
  """
  @spec isInt(Evision.FileNode.t()) :: boolean() | {:error, String.t()}
  def isInt(self) do
    positional = [
    ]
    :evision_nif.fileNode_isInt(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  isMap

  ##### Positional Arguments
  - **self**: `Evision.FileNode.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  isMap() -> retval
  ```
  """
  @spec isMap(Evision.FileNode.t()) :: boolean() | {:error, String.t()}
  def isMap(self) do
    positional = [
    ]
    :evision_nif.fileNode_isMap(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  isNamed

  ##### Positional Arguments
  - **self**: `Evision.FileNode.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  isNamed() -> retval
  ```
  """
  @spec isNamed(Evision.FileNode.t()) :: boolean() | {:error, String.t()}
  def isNamed(self) do
    positional = [
    ]
    :evision_nif.fileNode_isNamed(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  isNone

  ##### Positional Arguments
  - **self**: `Evision.FileNode.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  isNone() -> retval
  ```
  """
  @spec isNone(Evision.FileNode.t()) :: boolean() | {:error, String.t()}
  def isNone(self) do
    positional = [
    ]
    :evision_nif.fileNode_isNone(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  isReal

  ##### Positional Arguments
  - **self**: `Evision.FileNode.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  isReal() -> retval
  ```
  """
  @spec isReal(Evision.FileNode.t()) :: boolean() | {:error, String.t()}
  def isReal(self) do
    positional = [
    ]
    :evision_nif.fileNode_isReal(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  isSeq

  ##### Positional Arguments
  - **self**: `Evision.FileNode.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  isSeq() -> retval
  ```
  """
  @spec isSeq(Evision.FileNode.t()) :: boolean() | {:error, String.t()}
  def isSeq(self) do
    positional = [
    ]
    :evision_nif.fileNode_isSeq(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  isString

  ##### Positional Arguments
  - **self**: `Evision.FileNode.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  isString() -> retval
  ```
  """
  @spec isString(Evision.FileNode.t()) :: boolean() | {:error, String.t()}
  def isString(self) do
    positional = [
    ]
    :evision_nif.fileNode_isString(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns keys of a mapping node.

  ##### Positional Arguments
  - **self**: `Evision.FileNode.t()`

  ##### Return
  - **retval**: `[String]`

  @returns Keys of a mapping node.

  Python prototype (for reference only):
  ```python3
  keys() -> retval
  ```
  """
  @spec keys(Evision.FileNode.t()) :: list(binary()) | {:error, String.t()}
  def keys(self) do
    positional = [
    ]
    :evision_nif.fileNode_keys(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  mat

  ##### Positional Arguments
  - **self**: `Evision.FileNode.t()`

  ##### Return
  - **retval**: `Evision.Mat.t()`

  Python prototype (for reference only):
  ```python3
  mat() -> retval
  ```
  """
  @spec mat(Evision.FileNode.t()) :: Evision.Mat.t() | {:error, String.t()}
  def mat(self) do
    positional = [
    ]
    :evision_nif.fileNode_mat(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  name

  ##### Positional Arguments
  - **self**: `Evision.FileNode.t()`

  ##### Return
  - **retval**: `string`

  Python prototype (for reference only):
  ```python3
  name() -> retval
  ```
  """
  @spec name(Evision.FileNode.t()) :: binary() | {:error, String.t()}
  def name(self) do
    positional = [
    ]
    :evision_nif.fileNode_name(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  rawSize

  ##### Positional Arguments
  - **self**: `Evision.FileNode.t()`

  ##### Return
  - **retval**: `size_t`

  Python prototype (for reference only):
  ```python3
  rawSize() -> retval
  ```
  """
  @spec rawSize(Evision.FileNode.t()) :: integer() | {:error, String.t()}
  def rawSize(self) do
    positional = [
    ]
    :evision_nif.fileNode_rawSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  real

  ##### Positional Arguments
  - **self**: `Evision.FileNode.t()`

  ##### Return
  - **retval**: `double`

  Internal method used when reading FileStorage.
  Sets the type (int, real or string) and value of the previously created node.

  Python prototype (for reference only):
  ```python3
  real() -> retval
  ```
  """
  @spec real(Evision.FileNode.t()) :: number() | {:error, String.t()}
  def real(self) do
    positional = [
    ]
    :evision_nif.fileNode_real(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  size

  ##### Positional Arguments
  - **self**: `Evision.FileNode.t()`

  ##### Return
  - **retval**: `size_t`

  Python prototype (for reference only):
  ```python3
  size() -> retval
  ```
  """
  @spec size(Evision.FileNode.t()) :: integer() | {:error, String.t()}
  def size(self) do
    positional = [
    ]
    :evision_nif.fileNode_size(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  string

  ##### Positional Arguments
  - **self**: `Evision.FileNode.t()`

  ##### Return
  - **retval**: `string`

  Python prototype (for reference only):
  ```python3
  string() -> retval
  ```
  """
  @spec string(Evision.FileNode.t()) :: binary() | {:error, String.t()}
  def string(self) do
    positional = [
    ]
    :evision_nif.fileNode_string(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns type of the node.

  ##### Positional Arguments
  - **self**: `Evision.FileNode.t()`

  ##### Return
  - **retval**: `integer()`

  @returns Type of the node. See FileNode::Type

  Python prototype (for reference only):
  ```python3
  type() -> retval
  ```
  """
  @spec type(Evision.FileNode.t()) :: integer() | {:error, String.t()}
  def type(self) do
    positional = [
    ]
    :evision_nif.fileNode_type(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
