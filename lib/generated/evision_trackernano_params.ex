defmodule Evision.TrackerNano.Params do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `TrackerNano.Params` struct.

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
  def to_struct({:ok, %{class: Evision.TrackerNano.Params, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.TrackerNano.Params, ref: ref}) do
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
  TrackerNano_Params
  ##### Return
  - **self**: `Evision.TrackerNano.Params.t()`

  Python prototype (for reference only):
  ```python3
  TrackerNano_Params() -> <TrackerNano_Params object>
  ```
  """
  @spec params() :: Evision.TrackerNano.Params.t() | {:error, String.t()}
  def params() do
    positional = [
    ]
    :evision_nif.trackerNano_Params_TrackerNano_Params(positional)
    |> to_struct()
  end
  @spec get_backbone(Evision.TrackerNano.Params.t()) :: binary()
  def get_backbone(self) do
    :evision_nif.trackerNano_Params_get_backbone(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_backbone(Evision.TrackerNano.Params.t(), binary()) :: Evision.TrackerNano.Params.t()
  def set_backbone(self, prop) do
    :evision_nif.trackerNano_Params_set_backbone(
        Evision.Internal.Structurise.from_struct(self),
        [backbone: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_backend(Evision.TrackerNano.Params.t()) :: integer()
  def get_backend(self) do
    :evision_nif.trackerNano_Params_get_backend(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_backend(Evision.TrackerNano.Params.t(), integer()) :: Evision.TrackerNano.Params.t()
  def set_backend(self, prop) do
    :evision_nif.trackerNano_Params_set_backend(
        Evision.Internal.Structurise.from_struct(self),
        [backend: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_neckhead(Evision.TrackerNano.Params.t()) :: binary()
  def get_neckhead(self) do
    :evision_nif.trackerNano_Params_get_neckhead(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_neckhead(Evision.TrackerNano.Params.t(), binary()) :: Evision.TrackerNano.Params.t()
  def set_neckhead(self, prop) do
    :evision_nif.trackerNano_Params_set_neckhead(
        Evision.Internal.Structurise.from_struct(self),
        [neckhead: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_target(Evision.TrackerNano.Params.t()) :: integer()
  def get_target(self) do
    :evision_nif.trackerNano_Params_get_target(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_target(Evision.TrackerNano.Params.t(), integer()) :: Evision.TrackerNano.Params.t()
  def set_target(self, prop) do
    :evision_nif.trackerNano_Params_set_target(
        Evision.Internal.Structurise.from_struct(self),
        [target: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
end
