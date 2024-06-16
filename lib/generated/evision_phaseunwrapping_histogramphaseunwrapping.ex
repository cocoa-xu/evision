defmodule Evision.PhaseUnwrapping.HistogramPhaseUnwrapping do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `PhaseUnwrapping.HistogramPhaseUnwrapping` struct.

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
  def to_struct({:ok, %{class: Evision.PhaseUnwrapping.HistogramPhaseUnwrapping, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.PhaseUnwrapping.HistogramPhaseUnwrapping, ref: ref}) do
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
  Constructor
  ##### Keyword Arguments
  - **parameters**: `Evision.PhaseUnwrapping.HistogramPhaseUnwrapping.Params`.

    HistogramPhaseUnwrapping parameters HistogramPhaseUnwrapping::Params: width,height of the phase map and histogram characteristics.

  ##### Return
  - **retval**: `Evision.PhaseUnwrapping.HistogramPhaseUnwrapping.t()`

  Python prototype (for reference only):
  ```python3
  create([, parameters]) -> retval
  ```
  """
  @spec create([{:parameters, term()}] | nil) :: Evision.PhaseUnwrapping.HistogramPhaseUnwrapping.t() | {:error, String.t()}
  def create(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:parameters])
    positional = [
    ]
    :evision_nif.phase_unwrapping_phase_unwrapping_HistogramPhaseUnwrapping_create_static(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Constructor
  ##### Keyword Arguments
  - **parameters**: `Evision.PhaseUnwrapping.HistogramPhaseUnwrapping.Params`.

    HistogramPhaseUnwrapping parameters HistogramPhaseUnwrapping::Params: width,height of the phase map and histogram characteristics.

  ##### Return
  - **retval**: `Evision.PhaseUnwrapping.HistogramPhaseUnwrapping.t()`

  Python prototype (for reference only):
  ```python3
  create([, parameters]) -> retval
  ```
  """
  @spec create() :: Evision.PhaseUnwrapping.HistogramPhaseUnwrapping.t() | {:error, String.t()}
  def create() do
    positional = [
    ]
    :evision_nif.phase_unwrapping_phase_unwrapping_HistogramPhaseUnwrapping_create_static(positional)
    |> to_struct()
  end

  @doc """
  Get the reliability map computed from the wrapped phase map.

  ##### Positional Arguments
  - **self**: `Evision.PhaseUnwrapping.HistogramPhaseUnwrapping.t()`

  ##### Return
  - **reliabilityMap**: `Evision.Mat.t()`.

    Image where the reliability map is stored.

  Python prototype (for reference only):
  ```python3
  getInverseReliabilityMap([, reliabilityMap]) -> reliabilityMap
  ```
  """
  @spec getInverseReliabilityMap(Evision.PhaseUnwrapping.HistogramPhaseUnwrapping.t(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def getInverseReliabilityMap(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    positional = [
    ]
    :evision_nif.phase_unwrapping_phase_unwrapping_HistogramPhaseUnwrapping_getInverseReliabilityMap(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Get the reliability map computed from the wrapped phase map.

  ##### Positional Arguments
  - **self**: `Evision.PhaseUnwrapping.HistogramPhaseUnwrapping.t()`

  ##### Return
  - **reliabilityMap**: `Evision.Mat.t()`.

    Image where the reliability map is stored.

  Python prototype (for reference only):
  ```python3
  getInverseReliabilityMap([, reliabilityMap]) -> reliabilityMap
  ```
  """
  @spec getInverseReliabilityMap(Evision.PhaseUnwrapping.HistogramPhaseUnwrapping.t()) :: Evision.Mat.t() | {:error, String.t()}
  def getInverseReliabilityMap(self) do
    positional = [
    ]
    :evision_nif.phase_unwrapping_phase_unwrapping_HistogramPhaseUnwrapping_getInverseReliabilityMap(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
