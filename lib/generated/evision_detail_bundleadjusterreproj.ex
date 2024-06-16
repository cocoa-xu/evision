defmodule Evision.Detail.BundleAdjusterReproj do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Detail.BundleAdjusterReproj` struct.

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
  def to_struct({:ok, %{class: Evision.Detail.BundleAdjusterReproj, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Detail.BundleAdjusterReproj, ref: ref}) do
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
  BundleAdjusterReproj
  ##### Return
  - **self**: `Evision.Detail.BundleAdjusterReproj.t()`

  Python prototype (for reference only):
  ```python3
  BundleAdjusterReproj() -> <detail_BundleAdjusterReproj object>
  ```
  """
  @spec bundleAdjusterReproj() :: Evision.Detail.BundleAdjusterReproj.t() | {:error, String.t()}
  def bundleAdjusterReproj() do
    positional = [
    ]
    :evision_nif.detail_detail_BundleAdjusterReproj_BundleAdjusterReproj(positional)
    |> to_struct()
  end
end
