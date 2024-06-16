defmodule Evision.ArUco.EstimateParameters do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `ArUco.EstimateParameters` struct.

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
  def to_struct({:ok, %{class: Evision.ArUco.EstimateParameters, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.ArUco.EstimateParameters, ref: ref}) do
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
  EstimateParameters
  ##### Return
  - **self**: `EstimateParameters`

  Python prototype (for reference only):
  ```python3
  EstimateParameters() -> <aruco_EstimateParameters object>
  ```
  """
  @spec estimateParameters() :: Evision.ArUco.EstimateParameters.t() | {:error, String.t()}
  def estimateParameters() do
    positional = [
    ]
    :evision_nif.aruco_aruco_EstimateParameters_EstimateParameters(positional)
    |> to_struct()
  end
  @spec get_pattern(Evision.ArUco.EstimateParameters.t()) :: Evision.ArUco.PatternPositionType.enum()
  def get_pattern(self) do
    :evision_nif.aruco_EstimateParameters_get_pattern(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_pattern(Evision.ArUco.EstimateParameters.t(), Evision.ArUco.PatternPositionType.enum()) :: Evision.ArUco.EstimateParameters.t()
  def set_pattern(self, prop) do
    :evision_nif.aruco_EstimateParameters_set_pattern(
        Evision.Internal.Structurise.from_struct(self),
        [pattern: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_solvePnPMethod(Evision.ArUco.EstimateParameters.t()) :: integer()
  def get_solvePnPMethod(self) do
    :evision_nif.aruco_EstimateParameters_get_solvePnPMethod(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_solvePnPMethod(Evision.ArUco.EstimateParameters.t(), integer()) :: Evision.ArUco.EstimateParameters.t()
  def set_solvePnPMethod(self, prop) do
    :evision_nif.aruco_EstimateParameters_set_solvePnPMethod(
        Evision.Internal.Structurise.from_struct(self),
        [solvePnPMethod: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_useExtrinsicGuess(Evision.ArUco.EstimateParameters.t()) :: boolean()
  def get_useExtrinsicGuess(self) do
    :evision_nif.aruco_EstimateParameters_get_useExtrinsicGuess(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_useExtrinsicGuess(Evision.ArUco.EstimateParameters.t(), boolean()) :: Evision.ArUco.EstimateParameters.t()
  def set_useExtrinsicGuess(self, prop) do
    :evision_nif.aruco_EstimateParameters_set_useExtrinsicGuess(
        Evision.Internal.Structurise.from_struct(self),
        [useExtrinsicGuess: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
end
