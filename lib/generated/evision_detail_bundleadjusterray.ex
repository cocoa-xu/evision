defmodule Evision.Detail.BundleAdjusterRay do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Detail.BundleAdjusterRay` struct.

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
  def to_struct({:ok, %{class: Evision.Detail.BundleAdjusterRay, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Detail.BundleAdjusterRay, ref: ref}) do
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
  BundleAdjusterRay
  ##### Return
  - **self**: `Evision.Detail.BundleAdjusterRay.t()`

  Python prototype (for reference only):
  ```python3
  BundleAdjusterRay() -> <detail_BundleAdjusterRay object>
  ```
  """
  @spec bundleAdjusterRay() :: Evision.Detail.BundleAdjusterRay.t() | {:error, String.t()}
  def bundleAdjusterRay() do
    positional = [
    ]
    :evision_nif.detail_detail_BundleAdjusterRay_BundleAdjusterRay(positional)
    |> to_struct()
  end
end
