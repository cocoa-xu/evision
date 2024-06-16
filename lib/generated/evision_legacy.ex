defmodule Evision.Legacy do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Legacy` struct.

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
  def to_struct({:ok, %{class: Evision.Legacy, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Legacy, ref: ref}) do
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
  upgradeTrackingAPI

  ##### Positional Arguments
  - **legacy_tracker**: `Evision.Legacy.MultiTracker`

  ##### Return
  - **retval**: `Evision.Tracker.t()`

  Python prototype (for reference only):
  ```python3
  upgradeTrackingAPI(legacy_tracker) -> retval
  ```
  """
  @spec upgradeTrackingAPI(Evision.Legacy.MultiTracker.t()) :: Evision.Tracker.t() | {:error, String.t()}
  def upgradeTrackingAPI(legacy_tracker) when is_struct(legacy_tracker, Evision.Legacy.MultiTracker)
  do
    positional = [
      legacy_tracker: Evision.Internal.Structurise.from_struct(legacy_tracker)
    ]
    :evision_nif.legacy_upgradeTrackingAPI(positional)
    |> to_struct()
  end
end
