defmodule Evision.Ipp do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Ipp` struct.

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
  def to_struct({:ok, %{class: Evision.Ipp, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Ipp, ref: ref}) do
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
  getIppVersion
  ##### Return
  - **retval**: `String`

  Python prototype (for reference only):
  ```python3
  getIppVersion() -> retval
  ```
  """
  @spec getIppVersion() :: binary() | {:error, String.t()}
  def getIppVersion() do
    positional = [
    ]
    :evision_nif.ipp_getIppVersion(positional)
    |> to_struct()
  end

  @doc """
  setUseIPP

  ##### Positional Arguments
  - **flag**: `bool`

  Python prototype (for reference only):
  ```python3
  setUseIPP(flag) -> None
  ```
  """
  @spec setUseIPP(boolean()) :: :ok | {:error, String.t()}
  def setUseIPP(flag) when is_boolean(flag)
  do
    positional = [
      flag: Evision.Internal.Structurise.from_struct(flag)
    ]
    :evision_nif.ipp_setUseIPP(positional)
    |> to_struct()
  end

  @doc """
  useIPP
  ##### Return
  - **retval**: `bool`

  proxy for hal::Cholesky

  Python prototype (for reference only):
  ```python3
  useIPP() -> retval
  ```
  """
  @spec useIPP() :: boolean() | {:error, String.t()}
  def useIPP() do
    positional = [
    ]
    :evision_nif.ipp_useIPP(positional)
    |> to_struct()
  end
end
