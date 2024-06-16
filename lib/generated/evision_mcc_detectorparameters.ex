defmodule Evision.MCC.DetectorParameters do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `MCC.DetectorParameters` struct.

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
  def to_struct({:ok, %{class: Evision.MCC.DetectorParameters, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.MCC.DetectorParameters, ref: ref}) do
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
  create
  ##### Return
  - **retval**: `DetectorParameters`

  Python prototype (for reference only):
  ```python3
  create() -> retval
  ```
  """
  @spec create() :: Evision.MCC.DetectorParameters.t() | {:error, String.t()}
  def create() do
    positional = [
    ]
    :evision_nif.mcc_mcc_DetectorParameters_create_static(positional)
    |> to_struct()
  end
  @spec get_B0factor(Evision.MCC.DetectorParameters.t()) :: number()
  def get_B0factor(self) do
    :evision_nif.mcc_DetectorParameters_get_B0factor(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_B0factor(Evision.MCC.DetectorParameters.t(), number()) :: Evision.MCC.DetectorParameters.t()
  def set_B0factor(self, prop) do
    :evision_nif.mcc_DetectorParameters_set_B0factor(
        Evision.Internal.Structurise.from_struct(self),
        [B0factor: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_adaptiveThreshConstant(Evision.MCC.DetectorParameters.t()) :: number()
  def get_adaptiveThreshConstant(self) do
    :evision_nif.mcc_DetectorParameters_get_adaptiveThreshConstant(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_adaptiveThreshConstant(Evision.MCC.DetectorParameters.t(), number()) :: Evision.MCC.DetectorParameters.t()
  def set_adaptiveThreshConstant(self, prop) do
    :evision_nif.mcc_DetectorParameters_set_adaptiveThreshConstant(
        Evision.Internal.Structurise.from_struct(self),
        [adaptiveThreshConstant: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_adaptiveThreshWinSizeMax(Evision.MCC.DetectorParameters.t()) :: integer()
  def get_adaptiveThreshWinSizeMax(self) do
    :evision_nif.mcc_DetectorParameters_get_adaptiveThreshWinSizeMax(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_adaptiveThreshWinSizeMax(Evision.MCC.DetectorParameters.t(), integer()) :: Evision.MCC.DetectorParameters.t()
  def set_adaptiveThreshWinSizeMax(self, prop) do
    :evision_nif.mcc_DetectorParameters_set_adaptiveThreshWinSizeMax(
        Evision.Internal.Structurise.from_struct(self),
        [adaptiveThreshWinSizeMax: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_adaptiveThreshWinSizeMin(Evision.MCC.DetectorParameters.t()) :: integer()
  def get_adaptiveThreshWinSizeMin(self) do
    :evision_nif.mcc_DetectorParameters_get_adaptiveThreshWinSizeMin(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_adaptiveThreshWinSizeMin(Evision.MCC.DetectorParameters.t(), integer()) :: Evision.MCC.DetectorParameters.t()
  def set_adaptiveThreshWinSizeMin(self, prop) do
    :evision_nif.mcc_DetectorParameters_set_adaptiveThreshWinSizeMin(
        Evision.Internal.Structurise.from_struct(self),
        [adaptiveThreshWinSizeMin: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_adaptiveThreshWinSizeStep(Evision.MCC.DetectorParameters.t()) :: integer()
  def get_adaptiveThreshWinSizeStep(self) do
    :evision_nif.mcc_DetectorParameters_get_adaptiveThreshWinSizeStep(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_adaptiveThreshWinSizeStep(Evision.MCC.DetectorParameters.t(), integer()) :: Evision.MCC.DetectorParameters.t()
  def set_adaptiveThreshWinSizeStep(self, prop) do
    :evision_nif.mcc_DetectorParameters_set_adaptiveThreshWinSizeStep(
        Evision.Internal.Structurise.from_struct(self),
        [adaptiveThreshWinSizeStep: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_borderWidth(Evision.MCC.DetectorParameters.t()) :: integer()
  def get_borderWidth(self) do
    :evision_nif.mcc_DetectorParameters_get_borderWidth(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_borderWidth(Evision.MCC.DetectorParameters.t(), integer()) :: Evision.MCC.DetectorParameters.t()
  def set_borderWidth(self, prop) do
    :evision_nif.mcc_DetectorParameters_set_borderWidth(
        Evision.Internal.Structurise.from_struct(self),
        [borderWidth: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_confidenceThreshold(Evision.MCC.DetectorParameters.t()) :: number()
  def get_confidenceThreshold(self) do
    :evision_nif.mcc_DetectorParameters_get_confidenceThreshold(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_confidenceThreshold(Evision.MCC.DetectorParameters.t(), number()) :: Evision.MCC.DetectorParameters.t()
  def set_confidenceThreshold(self, prop) do
    :evision_nif.mcc_DetectorParameters_set_confidenceThreshold(
        Evision.Internal.Structurise.from_struct(self),
        [confidenceThreshold: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_findCandidatesApproxPolyDPEpsMultiplier(Evision.MCC.DetectorParameters.t()) :: number()
  def get_findCandidatesApproxPolyDPEpsMultiplier(self) do
    :evision_nif.mcc_DetectorParameters_get_findCandidatesApproxPolyDPEpsMultiplier(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_findCandidatesApproxPolyDPEpsMultiplier(Evision.MCC.DetectorParameters.t(), number()) :: Evision.MCC.DetectorParameters.t()
  def set_findCandidatesApproxPolyDPEpsMultiplier(self, prop) do
    :evision_nif.mcc_DetectorParameters_set_findCandidatesApproxPolyDPEpsMultiplier(
        Evision.Internal.Structurise.from_struct(self),
        [findCandidatesApproxPolyDPEpsMultiplier: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_maxError(Evision.MCC.DetectorParameters.t()) :: number()
  def get_maxError(self) do
    :evision_nif.mcc_DetectorParameters_get_maxError(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_maxError(Evision.MCC.DetectorParameters.t(), number()) :: Evision.MCC.DetectorParameters.t()
  def set_maxError(self, prop) do
    :evision_nif.mcc_DetectorParameters_set_maxError(
        Evision.Internal.Structurise.from_struct(self),
        [maxError: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_minContourLengthAllowed(Evision.MCC.DetectorParameters.t()) :: integer()
  def get_minContourLengthAllowed(self) do
    :evision_nif.mcc_DetectorParameters_get_minContourLengthAllowed(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_minContourLengthAllowed(Evision.MCC.DetectorParameters.t(), integer()) :: Evision.MCC.DetectorParameters.t()
  def set_minContourLengthAllowed(self, prop) do
    :evision_nif.mcc_DetectorParameters_set_minContourLengthAllowed(
        Evision.Internal.Structurise.from_struct(self),
        [minContourLengthAllowed: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_minContourPointsAllowed(Evision.MCC.DetectorParameters.t()) :: integer()
  def get_minContourPointsAllowed(self) do
    :evision_nif.mcc_DetectorParameters_get_minContourPointsAllowed(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_minContourPointsAllowed(Evision.MCC.DetectorParameters.t(), integer()) :: Evision.MCC.DetectorParameters.t()
  def set_minContourPointsAllowed(self, prop) do
    :evision_nif.mcc_DetectorParameters_set_minContourPointsAllowed(
        Evision.Internal.Structurise.from_struct(self),
        [minContourPointsAllowed: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_minContourSolidity(Evision.MCC.DetectorParameters.t()) :: number()
  def get_minContourSolidity(self) do
    :evision_nif.mcc_DetectorParameters_get_minContourSolidity(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_minContourSolidity(Evision.MCC.DetectorParameters.t(), number()) :: Evision.MCC.DetectorParameters.t()
  def set_minContourSolidity(self, prop) do
    :evision_nif.mcc_DetectorParameters_set_minContourSolidity(
        Evision.Internal.Structurise.from_struct(self),
        [minContourSolidity: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_minContoursArea(Evision.MCC.DetectorParameters.t()) :: number()
  def get_minContoursArea(self) do
    :evision_nif.mcc_DetectorParameters_get_minContoursArea(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_minContoursArea(Evision.MCC.DetectorParameters.t(), number()) :: Evision.MCC.DetectorParameters.t()
  def set_minContoursArea(self, prop) do
    :evision_nif.mcc_DetectorParameters_set_minContoursArea(
        Evision.Internal.Structurise.from_struct(self),
        [minContoursArea: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_minContoursAreaRate(Evision.MCC.DetectorParameters.t()) :: number()
  def get_minContoursAreaRate(self) do
    :evision_nif.mcc_DetectorParameters_get_minContoursAreaRate(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_minContoursAreaRate(Evision.MCC.DetectorParameters.t(), number()) :: Evision.MCC.DetectorParameters.t()
  def set_minContoursAreaRate(self, prop) do
    :evision_nif.mcc_DetectorParameters_set_minContoursAreaRate(
        Evision.Internal.Structurise.from_struct(self),
        [minContoursAreaRate: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_minGroupSize(Evision.MCC.DetectorParameters.t()) :: integer()
  def get_minGroupSize(self) do
    :evision_nif.mcc_DetectorParameters_get_minGroupSize(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_minGroupSize(Evision.MCC.DetectorParameters.t(), integer()) :: Evision.MCC.DetectorParameters.t()
  def set_minGroupSize(self, prop) do
    :evision_nif.mcc_DetectorParameters_set_minGroupSize(
        Evision.Internal.Structurise.from_struct(self),
        [minGroupSize: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_minImageSize(Evision.MCC.DetectorParameters.t()) :: integer()
  def get_minImageSize(self) do
    :evision_nif.mcc_DetectorParameters_get_minImageSize(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_minImageSize(Evision.MCC.DetectorParameters.t(), integer()) :: Evision.MCC.DetectorParameters.t()
  def set_minImageSize(self, prop) do
    :evision_nif.mcc_DetectorParameters_set_minImageSize(
        Evision.Internal.Structurise.from_struct(self),
        [minImageSize: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_minInterCheckerDistance(Evision.MCC.DetectorParameters.t()) :: integer()
  def get_minInterCheckerDistance(self) do
    :evision_nif.mcc_DetectorParameters_get_minInterCheckerDistance(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_minInterCheckerDistance(Evision.MCC.DetectorParameters.t(), integer()) :: Evision.MCC.DetectorParameters.t()
  def set_minInterCheckerDistance(self, prop) do
    :evision_nif.mcc_DetectorParameters_set_minInterCheckerDistance(
        Evision.Internal.Structurise.from_struct(self),
        [minInterCheckerDistance: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_minInterContourDistance(Evision.MCC.DetectorParameters.t()) :: integer()
  def get_minInterContourDistance(self) do
    :evision_nif.mcc_DetectorParameters_get_minInterContourDistance(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_minInterContourDistance(Evision.MCC.DetectorParameters.t(), integer()) :: Evision.MCC.DetectorParameters.t()
  def set_minInterContourDistance(self, prop) do
    :evision_nif.mcc_DetectorParameters_set_minInterContourDistance(
        Evision.Internal.Structurise.from_struct(self),
        [minInterContourDistance: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
end
