defmodule Evision.PhaseUnwrapping.HistogramPhaseUnwrapping.Params do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `PhaseUnwrapping.HistogramPhaseUnwrapping.Params` struct.

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
  def to_struct({:ok, %{class: Evision.PhaseUnwrapping.HistogramPhaseUnwrapping.Params, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.PhaseUnwrapping.HistogramPhaseUnwrapping.Params, ref: ref}) do
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
  HistogramPhaseUnwrapping_Params
  ##### Return
  - **self**: `Evision.PhaseUnwrapping.HistogramPhaseUnwrapping.Params.t()`

  Python prototype (for reference only):
  ```python3
  HistogramPhaseUnwrapping_Params() -> <phase_unwrapping_HistogramPhaseUnwrapping_Params object>
  ```
  """
  @spec phase_unwrapping_HistogramPhaseUnwrapping_Params() :: Evision.PhaseUnwrapping.HistogramPhaseUnwrapping.Params.t() | {:error, String.t()}
  def phase_unwrapping_HistogramPhaseUnwrapping_Params() do
    positional = [
    ]
    :evision_nif.phase_unwrapping_phase_unwrapping_HistogramPhaseUnwrapping_Params_HistogramPhaseUnwrapping_Params(positional)
    |> to_struct()
  end
  @spec get_height(Evision.PhaseUnwrapping.HistogramPhaseUnwrapping.Params.t()) :: integer()
  def get_height(self) do
    :evision_nif.phase_unwrapping_HistogramPhaseUnwrapping_Params_get_height(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_height(Evision.PhaseUnwrapping.HistogramPhaseUnwrapping.Params.t(), integer()) :: Evision.PhaseUnwrapping.HistogramPhaseUnwrapping.Params.t()
  def set_height(self, prop) do
    :evision_nif.phase_unwrapping_HistogramPhaseUnwrapping_Params_set_height(
        Evision.Internal.Structurise.from_struct(self),
        [height: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_histThresh(Evision.PhaseUnwrapping.HistogramPhaseUnwrapping.Params.t()) :: number()
  def get_histThresh(self) do
    :evision_nif.phase_unwrapping_HistogramPhaseUnwrapping_Params_get_histThresh(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_histThresh(Evision.PhaseUnwrapping.HistogramPhaseUnwrapping.Params.t(), number()) :: Evision.PhaseUnwrapping.HistogramPhaseUnwrapping.Params.t()
  def set_histThresh(self, prop) do
    :evision_nif.phase_unwrapping_HistogramPhaseUnwrapping_Params_set_histThresh(
        Evision.Internal.Structurise.from_struct(self),
        [histThresh: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_nbrOfLargeBins(Evision.PhaseUnwrapping.HistogramPhaseUnwrapping.Params.t()) :: integer()
  def get_nbrOfLargeBins(self) do
    :evision_nif.phase_unwrapping_HistogramPhaseUnwrapping_Params_get_nbrOfLargeBins(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_nbrOfLargeBins(Evision.PhaseUnwrapping.HistogramPhaseUnwrapping.Params.t(), integer()) :: Evision.PhaseUnwrapping.HistogramPhaseUnwrapping.Params.t()
  def set_nbrOfLargeBins(self, prop) do
    :evision_nif.phase_unwrapping_HistogramPhaseUnwrapping_Params_set_nbrOfLargeBins(
        Evision.Internal.Structurise.from_struct(self),
        [nbrOfLargeBins: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_nbrOfSmallBins(Evision.PhaseUnwrapping.HistogramPhaseUnwrapping.Params.t()) :: integer()
  def get_nbrOfSmallBins(self) do
    :evision_nif.phase_unwrapping_HistogramPhaseUnwrapping_Params_get_nbrOfSmallBins(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_nbrOfSmallBins(Evision.PhaseUnwrapping.HistogramPhaseUnwrapping.Params.t(), integer()) :: Evision.PhaseUnwrapping.HistogramPhaseUnwrapping.Params.t()
  def set_nbrOfSmallBins(self, prop) do
    :evision_nif.phase_unwrapping_HistogramPhaseUnwrapping_Params_set_nbrOfSmallBins(
        Evision.Internal.Structurise.from_struct(self),
        [nbrOfSmallBins: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_width(Evision.PhaseUnwrapping.HistogramPhaseUnwrapping.Params.t()) :: integer()
  def get_width(self) do
    :evision_nif.phase_unwrapping_HistogramPhaseUnwrapping_Params_get_width(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_width(Evision.PhaseUnwrapping.HistogramPhaseUnwrapping.Params.t(), integer()) :: Evision.PhaseUnwrapping.HistogramPhaseUnwrapping.Params.t()
  def set_width(self, prop) do
    :evision_nif.phase_unwrapping_HistogramPhaseUnwrapping_Params_set_width(
        Evision.Internal.Structurise.from_struct(self),
        [width: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
end
