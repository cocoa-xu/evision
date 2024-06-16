defmodule Evision.TrackerVit.Params do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `TrackerVit.Params` struct.

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
  def to_struct({:ok, %{class: Evision.TrackerVit.Params, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.TrackerVit.Params, ref: ref}) do
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
  TrackerVit_Params
  ##### Return
  - **self**: `Evision.TrackerVit.Params.t()`

  Python prototype (for reference only):
  ```python3
  TrackerVit_Params() -> <TrackerVit_Params object>
  ```
  """
  @spec params() :: Evision.TrackerVit.Params.t() | {:error, String.t()}
  def params() do
    positional = [
    ]
    :evision_nif.trackerVit_Params_TrackerVit_Params(positional)
    |> to_struct()
  end
  @spec get_backend(Evision.TrackerVit.Params.t()) :: integer()
  def get_backend(self) do
    :evision_nif.trackerVit_Params_get_backend(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_backend(Evision.TrackerVit.Params.t(), integer()) :: Evision.TrackerVit.Params.t()
  def set_backend(self, prop) do
    :evision_nif.trackerVit_Params_set_backend(
        Evision.Internal.Structurise.from_struct(self),
        [backend: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_meanvalue(Evision.TrackerVit.Params.t()) :: Evision.scalar()
  def get_meanvalue(self) do
    :evision_nif.trackerVit_Params_get_meanvalue(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_meanvalue(Evision.TrackerVit.Params.t(), Evision.scalar()) :: Evision.TrackerVit.Params.t()
  def set_meanvalue(self, prop) do
    :evision_nif.trackerVit_Params_set_meanvalue(
        Evision.Internal.Structurise.from_struct(self),
        [meanvalue: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_net(Evision.TrackerVit.Params.t()) :: binary()
  def get_net(self) do
    :evision_nif.trackerVit_Params_get_net(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_net(Evision.TrackerVit.Params.t(), binary()) :: Evision.TrackerVit.Params.t()
  def set_net(self, prop) do
    :evision_nif.trackerVit_Params_set_net(
        Evision.Internal.Structurise.from_struct(self),
        [net: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_stdvalue(Evision.TrackerVit.Params.t()) :: Evision.scalar()
  def get_stdvalue(self) do
    :evision_nif.trackerVit_Params_get_stdvalue(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_stdvalue(Evision.TrackerVit.Params.t(), Evision.scalar()) :: Evision.TrackerVit.Params.t()
  def set_stdvalue(self, prop) do
    :evision_nif.trackerVit_Params_set_stdvalue(
        Evision.Internal.Structurise.from_struct(self),
        [stdvalue: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_target(Evision.TrackerVit.Params.t()) :: integer()
  def get_target(self) do
    :evision_nif.trackerVit_Params_get_target(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_target(Evision.TrackerVit.Params.t(), integer()) :: Evision.TrackerVit.Params.t()
  def set_target(self, prop) do
    :evision_nif.trackerVit_Params_set_target(
        Evision.Internal.Structurise.from_struct(self),
        [target: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
end
