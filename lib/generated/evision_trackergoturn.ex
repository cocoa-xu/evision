defmodule Evision.TrackerGOTURN do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `TrackerGOTURN` struct.

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
  def to_struct({:ok, %{class: Evision.TrackerGOTURN, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.TrackerGOTURN, ref: ref}) do
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
  Constructor
  ##### Keyword Arguments
  - **parameters**: `Evision.TrackerGOTURN.Params`.

    GOTURN parameters TrackerGOTURN::Params

  ##### Return
  - **retval**: `Evision.TrackerGOTURN.t()`

  Python prototype (for reference only):
  ```python3
  create([, parameters]) -> retval
  ```
  """
  @spec create([{:parameters, term()}] | nil) :: Evision.TrackerGOTURN.t() | {:error, String.t()}
  def create(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:parameters])
    positional = [
    ]
    :evision_nif.trackerGOTURN_create_static(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Constructor
  ##### Keyword Arguments
  - **parameters**: `Evision.TrackerGOTURN.Params`.

    GOTURN parameters TrackerGOTURN::Params

  ##### Return
  - **retval**: `Evision.TrackerGOTURN.t()`

  Python prototype (for reference only):
  ```python3
  create([, parameters]) -> retval
  ```
  """
  @spec create() :: Evision.TrackerGOTURN.t() | {:error, String.t()}
  def create() do
    positional = [
    ]
    :evision_nif.trackerGOTURN_create_static(positional)
    |> to_struct()
  end
end
