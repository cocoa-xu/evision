defmodule Evision.Stereo.MatchQuasiDense do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Stereo.MatchQuasiDense` struct.

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
  def to_struct({:ok, %{class: Evision.Stereo.MatchQuasiDense, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Stereo.MatchQuasiDense, ref: ref}) do
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
  MatchQuasiDense
  ##### Return
  - **self**: `MatchQuasiDense`

  Python prototype (for reference only):
  ```python3
  MatchQuasiDense() -> <stereo_MatchQuasiDense object>
  ```
  """
  @spec matchQuasiDense() :: Evision.Stereo.MatchQuasiDense.t() | {:error, String.t()}
  def matchQuasiDense() do
    positional = [
    ]
    :evision_nif.stereo_stereo_MatchQuasiDense_MatchQuasiDense(positional)
    |> to_struct()
  end

  @doc """
  apply

  ##### Positional Arguments
  - **self**: `Evision.Stereo.MatchQuasiDense.t()`
  - **rhs**: `MatchQuasiDense`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  apply(rhs) -> retval
  ```
  """
  @spec apply(Evision.Stereo.MatchQuasiDense.t(), Evision.Stereo.MatchQuasiDense.t()) :: boolean() | {:error, String.t()}
  def apply(self, rhs) when is_struct(rhs, Evision.Stereo.MatchQuasiDense)
  do
    positional = [
      rhs: Evision.Internal.Structurise.from_struct(rhs)
    ]
    :evision_nif.stereo_stereo_MatchQuasiDense_apply(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec get_corr(Evision.Stereo.MatchQuasiDense.t()) :: number()
  def get_corr(self) do
    :evision_nif.stereo_MatchQuasiDense_get_corr(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_corr(Evision.Stereo.MatchQuasiDense.t(), number()) :: Evision.Stereo.MatchQuasiDense.t()
  def set_corr(self, prop) do
    :evision_nif.stereo_MatchQuasiDense_set_corr(
        Evision.Internal.Structurise.from_struct(self),
        [corr: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_p0(Evision.Stereo.MatchQuasiDense.t()) :: {integer(), integer()}
  def get_p0(self) do
    :evision_nif.stereo_MatchQuasiDense_get_p0(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_p0(Evision.Stereo.MatchQuasiDense.t(), {integer(), integer()}) :: Evision.Stereo.MatchQuasiDense.t()
  def set_p0(self, prop) do
    :evision_nif.stereo_MatchQuasiDense_set_p0(
        Evision.Internal.Structurise.from_struct(self),
        [p0: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_p1(Evision.Stereo.MatchQuasiDense.t()) :: {integer(), integer()}
  def get_p1(self) do
    :evision_nif.stereo_MatchQuasiDense_get_p1(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_p1(Evision.Stereo.MatchQuasiDense.t(), {integer(), integer()}) :: Evision.Stereo.MatchQuasiDense.t()
  def set_p1(self, prop) do
    :evision_nif.stereo_MatchQuasiDense_set_p1(
        Evision.Internal.Structurise.from_struct(self),
        [p1: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
end
