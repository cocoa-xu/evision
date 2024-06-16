defmodule Evision.Utils.Nested.OriginalClassName do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Utils.Nested.OriginalClassName` struct.

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
  def to_struct({:ok, %{class: Evision.Utils.Nested.OriginalClassName, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Utils.Nested.OriginalClassName, ref: ref}) do
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
  create
  ##### Keyword Arguments
  - **params**: `Evision.Utils.Nested.OriginalClassName.Params`.

  ##### Return
  - **retval**: `Evision.Utils.Nested.OriginalClassName.t()`

  Python prototype (for reference only):
  ```python3
  create([, params]) -> retval
  ```
  """
  @spec create([{:params, term()}] | nil) :: Evision.Utils.Nested.OriginalClassName.t() | {:error, String.t()}
  def create(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:params])
    positional = [
    ]
    :evision_nif.utils_nested_utils_nested_OriginalClassName_create_static(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  create
  ##### Keyword Arguments
  - **params**: `Evision.Utils.Nested.OriginalClassName.Params`.

  ##### Return
  - **retval**: `Evision.Utils.Nested.OriginalClassName.t()`

  Python prototype (for reference only):
  ```python3
  create([, params]) -> retval
  ```
  """
  @spec create() :: Evision.Utils.Nested.OriginalClassName.t() | {:error, String.t()}
  def create() do
    positional = [
    ]
    :evision_nif.utils_nested_utils_nested_OriginalClassName_create_static(positional)
    |> to_struct()
  end

  @doc """
  getFloatParam

  ##### Positional Arguments
  - **self**: `Evision.Utils.Nested.OriginalClassName.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  getFloatParam() -> retval
  ```
  """
  @spec getFloatParam(Evision.Utils.Nested.OriginalClassName.t()) :: number() | {:error, String.t()}
  def getFloatParam(self) do
    positional = [
    ]
    :evision_nif.utils_nested_utils_nested_OriginalClassName_getFloatParam(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getIntParam

  ##### Positional Arguments
  - **self**: `Evision.Utils.Nested.OriginalClassName.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getIntParam() -> retval
  ```
  """
  @spec getIntParam(Evision.Utils.Nested.OriginalClassName.t()) :: integer() | {:error, String.t()}
  def getIntParam(self) do
    positional = [
    ]
    :evision_nif.utils_nested_utils_nested_OriginalClassName_getIntParam(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  originalName
  ##### Return
  - **retval**: `string`

  Python prototype (for reference only):
  ```python3
  originalName() -> retval
  ```
  """
  @spec originalName() :: binary() | {:error, String.t()}
  def originalName() do
    positional = [
    ]
    :evision_nif.utils_nested_utils_nested_OriginalClassName_originalName_static(positional)
    |> to_struct()
  end
end
