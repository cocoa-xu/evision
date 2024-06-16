defmodule Evision.TrackerMIL do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `TrackerMIL` struct.

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
  def to_struct({:ok, %{class: Evision.TrackerMIL, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.TrackerMIL, ref: ref}) do
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
  Create MIL tracker instance
  ##### Keyword Arguments
  - **parameters**: `Evision.TrackerMIL.Params`.

    MIL parameters TrackerMIL::Params

  ##### Return
  - **retval**: `Evision.TrackerMIL.t()`

  Python prototype (for reference only):
  ```python3
  create([, parameters]) -> retval
  ```
  """
  @spec create([{:parameters, term()}] | nil) :: Evision.TrackerMIL.t() | {:error, String.t()}
  def create(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:parameters])
    positional = [
    ]
    :evision_nif.trackerMIL_create_static(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Create MIL tracker instance
  ##### Keyword Arguments
  - **parameters**: `Evision.TrackerMIL.Params`.

    MIL parameters TrackerMIL::Params

  ##### Return
  - **retval**: `Evision.TrackerMIL.t()`

  Python prototype (for reference only):
  ```python3
  create([, parameters]) -> retval
  ```
  """
  @spec create() :: Evision.TrackerMIL.t() | {:error, String.t()}
  def create() do
    positional = [
    ]
    :evision_nif.trackerMIL_create_static(positional)
    |> to_struct()
  end
end
