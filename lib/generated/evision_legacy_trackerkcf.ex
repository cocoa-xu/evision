defmodule Evision.Legacy.TrackerKCF do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Legacy.TrackerKCF` struct.

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
  def to_struct({:ok, %{class: Evision.Legacy.TrackerKCF, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Legacy.TrackerKCF, ref: ref}) do
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
  ##### Return
  - **retval**: `legacy::TrackerKCF`

  Python prototype (for reference only):
  ```python3
  create() -> retval
  ```
  """
  @spec create() :: Evision.Legacy.TrackerKCF.t() | {:error, String.t()}
  def create() do
    positional = [
    ]
    :evision_nif.legacy_legacy_TrackerKCF_create_static(positional)
    |> to_struct()
  end
end
