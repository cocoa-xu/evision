defmodule Evision.Detail.BundleAdjusterAffinePartial do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Detail.BundleAdjusterAffinePartial` struct.

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
  def to_struct({:ok, %{class: Evision.Detail.BundleAdjusterAffinePartial, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Detail.BundleAdjusterAffinePartial, ref: ref}) do
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
  BundleAdjusterAffinePartial
  ##### Return
  - **self**: `Evision.Detail.BundleAdjusterAffinePartial.t()`

  Python prototype (for reference only):
  ```python3
  BundleAdjusterAffinePartial() -> <detail_BundleAdjusterAffinePartial object>
  ```
  """
  @spec bundleAdjusterAffinePartial() :: Evision.Detail.BundleAdjusterAffinePartial.t() | {:error, String.t()}
  def bundleAdjusterAffinePartial() do
    positional = [
    ]
    :evision_nif.detail_detail_BundleAdjusterAffinePartial_BundleAdjusterAffinePartial(positional)
    |> to_struct()
  end
end
