defmodule Evision.CirclesGridFinderParameters do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `CirclesGridFinderParameters` struct.

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
  def to_struct({:ok, %{class: Evision.CirclesGridFinderParameters, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.CirclesGridFinderParameters, ref: ref}) do
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
  CirclesGridFinderParameters
  ##### Return
  - **self**: `Evision.CirclesGridFinderParameters.t()`

  Python prototype (for reference only):
  ```python3
  CirclesGridFinderParameters() -> <CirclesGridFinderParameters object>
  ```
  """
  @spec circlesGridFinderParameters() :: Evision.CirclesGridFinderParameters.t() | {:error, String.t()}
  def circlesGridFinderParameters() do
    positional = [
    ]
    :evision_nif.circlesGridFinderParameters_CirclesGridFinderParameters(positional)
    |> to_struct()
  end
  @spec get_convexHullFactor(Evision.CirclesGridFinderParameters.t()) :: number()
  def get_convexHullFactor(self) do
    :evision_nif.circlesGridFinderParameters_get_convexHullFactor(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_convexHullFactor(Evision.CirclesGridFinderParameters.t(), number()) :: Evision.CirclesGridFinderParameters.t()
  def set_convexHullFactor(self, prop) do
    :evision_nif.circlesGridFinderParameters_set_convexHullFactor(
        Evision.Internal.Structurise.from_struct(self),
        [convexHullFactor: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_densityNeighborhoodSize(Evision.CirclesGridFinderParameters.t()) :: {number(), number()}
  def get_densityNeighborhoodSize(self) do
    :evision_nif.circlesGridFinderParameters_get_densityNeighborhoodSize(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_densityNeighborhoodSize(Evision.CirclesGridFinderParameters.t(), {number(), number()}) :: Evision.CirclesGridFinderParameters.t()
  def set_densityNeighborhoodSize(self, prop) do
    :evision_nif.circlesGridFinderParameters_set_densityNeighborhoodSize(
        Evision.Internal.Structurise.from_struct(self),
        [densityNeighborhoodSize: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_edgeGain(Evision.CirclesGridFinderParameters.t()) :: number()
  def get_edgeGain(self) do
    :evision_nif.circlesGridFinderParameters_get_edgeGain(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_edgeGain(Evision.CirclesGridFinderParameters.t(), number()) :: Evision.CirclesGridFinderParameters.t()
  def set_edgeGain(self, prop) do
    :evision_nif.circlesGridFinderParameters_set_edgeGain(
        Evision.Internal.Structurise.from_struct(self),
        [edgeGain: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_edgePenalty(Evision.CirclesGridFinderParameters.t()) :: number()
  def get_edgePenalty(self) do
    :evision_nif.circlesGridFinderParameters_get_edgePenalty(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_edgePenalty(Evision.CirclesGridFinderParameters.t(), number()) :: Evision.CirclesGridFinderParameters.t()
  def set_edgePenalty(self, prop) do
    :evision_nif.circlesGridFinderParameters_set_edgePenalty(
        Evision.Internal.Structurise.from_struct(self),
        [edgePenalty: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_existingVertexGain(Evision.CirclesGridFinderParameters.t()) :: number()
  def get_existingVertexGain(self) do
    :evision_nif.circlesGridFinderParameters_get_existingVertexGain(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_existingVertexGain(Evision.CirclesGridFinderParameters.t(), number()) :: Evision.CirclesGridFinderParameters.t()
  def set_existingVertexGain(self, prop) do
    :evision_nif.circlesGridFinderParameters_set_existingVertexGain(
        Evision.Internal.Structurise.from_struct(self),
        [existingVertexGain: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_keypointScale(Evision.CirclesGridFinderParameters.t()) :: integer()
  def get_keypointScale(self) do
    :evision_nif.circlesGridFinderParameters_get_keypointScale(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_keypointScale(Evision.CirclesGridFinderParameters.t(), integer()) :: Evision.CirclesGridFinderParameters.t()
  def set_keypointScale(self, prop) do
    :evision_nif.circlesGridFinderParameters_set_keypointScale(
        Evision.Internal.Structurise.from_struct(self),
        [keypointScale: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_kmeansAttempts(Evision.CirclesGridFinderParameters.t()) :: integer()
  def get_kmeansAttempts(self) do
    :evision_nif.circlesGridFinderParameters_get_kmeansAttempts(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_kmeansAttempts(Evision.CirclesGridFinderParameters.t(), integer()) :: Evision.CirclesGridFinderParameters.t()
  def set_kmeansAttempts(self, prop) do
    :evision_nif.circlesGridFinderParameters_set_kmeansAttempts(
        Evision.Internal.Structurise.from_struct(self),
        [kmeansAttempts: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_maxRectifiedDistance(Evision.CirclesGridFinderParameters.t()) :: number()
  def get_maxRectifiedDistance(self) do
    :evision_nif.circlesGridFinderParameters_get_maxRectifiedDistance(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_maxRectifiedDistance(Evision.CirclesGridFinderParameters.t(), number()) :: Evision.CirclesGridFinderParameters.t()
  def set_maxRectifiedDistance(self, prop) do
    :evision_nif.circlesGridFinderParameters_set_maxRectifiedDistance(
        Evision.Internal.Structurise.from_struct(self),
        [maxRectifiedDistance: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_minDensity(Evision.CirclesGridFinderParameters.t()) :: number()
  def get_minDensity(self) do
    :evision_nif.circlesGridFinderParameters_get_minDensity(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_minDensity(Evision.CirclesGridFinderParameters.t(), number()) :: Evision.CirclesGridFinderParameters.t()
  def set_minDensity(self, prop) do
    :evision_nif.circlesGridFinderParameters_set_minDensity(
        Evision.Internal.Structurise.from_struct(self),
        [minDensity: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_minDistanceToAddKeypoint(Evision.CirclesGridFinderParameters.t()) :: integer()
  def get_minDistanceToAddKeypoint(self) do
    :evision_nif.circlesGridFinderParameters_get_minDistanceToAddKeypoint(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_minDistanceToAddKeypoint(Evision.CirclesGridFinderParameters.t(), integer()) :: Evision.CirclesGridFinderParameters.t()
  def set_minDistanceToAddKeypoint(self, prop) do
    :evision_nif.circlesGridFinderParameters_set_minDistanceToAddKeypoint(
        Evision.Internal.Structurise.from_struct(self),
        [minDistanceToAddKeypoint: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_minGraphConfidence(Evision.CirclesGridFinderParameters.t()) :: number()
  def get_minGraphConfidence(self) do
    :evision_nif.circlesGridFinderParameters_get_minGraphConfidence(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_minGraphConfidence(Evision.CirclesGridFinderParameters.t(), number()) :: Evision.CirclesGridFinderParameters.t()
  def set_minGraphConfidence(self, prop) do
    :evision_nif.circlesGridFinderParameters_set_minGraphConfidence(
        Evision.Internal.Structurise.from_struct(self),
        [minGraphConfidence: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_minRNGEdgeSwitchDist(Evision.CirclesGridFinderParameters.t()) :: number()
  def get_minRNGEdgeSwitchDist(self) do
    :evision_nif.circlesGridFinderParameters_get_minRNGEdgeSwitchDist(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_minRNGEdgeSwitchDist(Evision.CirclesGridFinderParameters.t(), number()) :: Evision.CirclesGridFinderParameters.t()
  def set_minRNGEdgeSwitchDist(self, prop) do
    :evision_nif.circlesGridFinderParameters_set_minRNGEdgeSwitchDist(
        Evision.Internal.Structurise.from_struct(self),
        [minRNGEdgeSwitchDist: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_squareSize(Evision.CirclesGridFinderParameters.t()) :: number()
  def get_squareSize(self) do
    :evision_nif.circlesGridFinderParameters_get_squareSize(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_squareSize(Evision.CirclesGridFinderParameters.t(), number()) :: Evision.CirclesGridFinderParameters.t()
  def set_squareSize(self, prop) do
    :evision_nif.circlesGridFinderParameters_set_squareSize(
        Evision.Internal.Structurise.from_struct(self),
        [squareSize: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_vertexGain(Evision.CirclesGridFinderParameters.t()) :: number()
  def get_vertexGain(self) do
    :evision_nif.circlesGridFinderParameters_get_vertexGain(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_vertexGain(Evision.CirclesGridFinderParameters.t(), number()) :: Evision.CirclesGridFinderParameters.t()
  def set_vertexGain(self, prop) do
    :evision_nif.circlesGridFinderParameters_set_vertexGain(
        Evision.Internal.Structurise.from_struct(self),
        [vertexGain: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_vertexPenalty(Evision.CirclesGridFinderParameters.t()) :: number()
  def get_vertexPenalty(self) do
    :evision_nif.circlesGridFinderParameters_get_vertexPenalty(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_vertexPenalty(Evision.CirclesGridFinderParameters.t(), number()) :: Evision.CirclesGridFinderParameters.t()
  def set_vertexPenalty(self, prop) do
    :evision_nif.circlesGridFinderParameters_set_vertexPenalty(
        Evision.Internal.Structurise.from_struct(self),
        [vertexPenalty: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
end
