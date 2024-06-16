defmodule Evision.TrackerGOTURN.Params do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `TrackerGOTURN.Params` struct.

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
  def to_struct({:ok, %{class: Evision.TrackerGOTURN.Params, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.TrackerGOTURN.Params, ref: ref}) do
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
  TrackerGOTURN_Params
  ##### Return
  - **self**: `Evision.TrackerGOTURN.Params.t()`

  Python prototype (for reference only):
  ```python3
  TrackerGOTURN_Params() -> <TrackerGOTURN_Params object>
  ```
  """
  @spec params() :: Evision.TrackerGOTURN.Params.t() | {:error, String.t()}
  def params() do
    positional = [
    ]
    :evision_nif.trackerGOTURN_Params_TrackerGOTURN_Params(positional)
    |> to_struct()
  end
  @spec get_modelBin(Evision.TrackerGOTURN.Params.t()) :: binary()
  def get_modelBin(self) do
    :evision_nif.trackerGOTURN_Params_get_modelBin(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_modelBin(Evision.TrackerGOTURN.Params.t(), binary()) :: Evision.TrackerGOTURN.Params.t()
  def set_modelBin(self, prop) do
    :evision_nif.trackerGOTURN_Params_set_modelBin(
        Evision.Internal.Structurise.from_struct(self),
        [modelBin: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_modelTxt(Evision.TrackerGOTURN.Params.t()) :: binary()
  def get_modelTxt(self) do
    :evision_nif.trackerGOTURN_Params_get_modelTxt(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_modelTxt(Evision.TrackerGOTURN.Params.t(), binary()) :: Evision.TrackerGOTURN.Params.t()
  def set_modelTxt(self, prop) do
    :evision_nif.trackerGOTURN_Params_set_modelTxt(
        Evision.Internal.Structurise.from_struct(self),
        [modelTxt: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
end
