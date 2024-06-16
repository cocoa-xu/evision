defmodule Evision.Utils.Nested do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Utils.Nested` struct.

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
  def to_struct({:ok, %{class: Evision.Utils.Nested, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Utils.Nested, ref: ref}) do
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
  testEchoBooleanFunction

  ##### Positional Arguments
  - **flag**: `bool`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  testEchoBooleanFunction(flag) -> retval
  ```
  """
  @spec testEchoBooleanFunction(boolean()) :: boolean() | {:error, String.t()}
  def testEchoBooleanFunction(flag) when is_boolean(flag)
  do
    positional = [
      flag: Evision.Internal.Structurise.from_struct(flag)
    ]
    :evision_nif.utils_nested_testEchoBooleanFunction(positional)
    |> to_struct()
  end
end
