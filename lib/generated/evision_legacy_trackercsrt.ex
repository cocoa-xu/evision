defmodule Evision.Legacy.TrackerCSRT do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Legacy.TrackerCSRT` struct.

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
  def to_struct({:ok, %{class: Evision.Legacy.TrackerCSRT, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Legacy.TrackerCSRT, ref: ref}) do
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
  - **retval**: `legacy::TrackerCSRT`

  Python prototype (for reference only):
  ```python3
  create() -> retval
  ```
  """
  @spec create() :: Evision.Legacy.TrackerCSRT.t() | {:error, String.t()}
  def create() do
    positional = [
    ]
    :evision_nif.legacy_legacy_TrackerCSRT_create_static(positional)
    |> to_struct()
  end

  @doc """
  setInitialMask

  ##### Positional Arguments
  - **self**: `Evision.Legacy.TrackerCSRT.t()`
  - **mask**: `Evision.Mat`

  Python prototype (for reference only):
  ```python3
  setInitialMask(mask) -> None
  ```
  """
  @spec setInitialMask(Evision.Legacy.TrackerCSRT.t(), Evision.Mat.maybe_mat_in()) :: Evision.Legacy.TrackerCSRT.t() | {:error, String.t()}
  def setInitialMask(self, mask) when (is_struct(mask, Evision.Mat) or is_struct(mask, Nx.Tensor) or is_number(mask) or is_tuple(mask))
  do
    positional = [
      mask: Evision.Internal.Structurise.from_struct(mask)
    ]
    :evision_nif.legacy_legacy_TrackerCSRT_setInitialMask(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
