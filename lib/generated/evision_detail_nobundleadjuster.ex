defmodule Evision.Detail.NoBundleAdjuster do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Detail.NoBundleAdjuster` struct.

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
  def to_struct({:ok, %{class: Evision.Detail.NoBundleAdjuster, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Detail.NoBundleAdjuster, ref: ref}) do
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
  NoBundleAdjuster
  ##### Return
  - **self**: `Evision.Detail.NoBundleAdjuster.t()`

  Python prototype (for reference only):
  ```python3
  NoBundleAdjuster() -> <detail_NoBundleAdjuster object>
  ```
  """
  @spec noBundleAdjuster() :: Evision.Detail.NoBundleAdjuster.t() | {:error, String.t()}
  def noBundleAdjuster() do
    positional = [
    ]
    :evision_nif.detail_detail_NoBundleAdjuster_NoBundleAdjuster(positional)
    |> to_struct()
  end
end
