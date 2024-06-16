defmodule Evision.UsacParams do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `UsacParams` struct.

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
  def to_struct({:ok, %{class: Evision.UsacParams, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.UsacParams, ref: ref}) do
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
  UsacParams
  ##### Return
  - **self**: `Evision.UsacParams.t()`

  Python prototype (for reference only):
  ```python3
  UsacParams() -> <UsacParams object>
  ```
  """
  @spec usacParams() :: Evision.UsacParams.t() | {:error, String.t()}
  def usacParams() do
    positional = [
    ]
    :evision_nif.usacParams_UsacParams(positional)
    |> to_struct()
  end
  @spec get_confidence(Evision.UsacParams.t()) :: number()
  def get_confidence(self) do
    :evision_nif.usacParams_get_confidence(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_confidence(Evision.UsacParams.t(), number()) :: Evision.UsacParams.t()
  def set_confidence(self, prop) do
    :evision_nif.usacParams_set_confidence(
        Evision.Internal.Structurise.from_struct(self),
        [confidence: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_final_polisher(Evision.UsacParams.t()) :: Evision.PolishingMethod.enum()
  def get_final_polisher(self) do
    :evision_nif.usacParams_get_final_polisher(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_final_polisher(Evision.UsacParams.t(), Evision.PolishingMethod.enum()) :: Evision.UsacParams.t()
  def set_final_polisher(self, prop) do
    :evision_nif.usacParams_set_final_polisher(
        Evision.Internal.Structurise.from_struct(self),
        [final_polisher: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_final_polisher_iterations(Evision.UsacParams.t()) :: integer()
  def get_final_polisher_iterations(self) do
    :evision_nif.usacParams_get_final_polisher_iterations(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_final_polisher_iterations(Evision.UsacParams.t(), integer()) :: Evision.UsacParams.t()
  def set_final_polisher_iterations(self, prop) do
    :evision_nif.usacParams_set_final_polisher_iterations(
        Evision.Internal.Structurise.from_struct(self),
        [final_polisher_iterations: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_isParallel(Evision.UsacParams.t()) :: boolean()
  def get_isParallel(self) do
    :evision_nif.usacParams_get_isParallel(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_isParallel(Evision.UsacParams.t(), boolean()) :: Evision.UsacParams.t()
  def set_isParallel(self, prop) do
    :evision_nif.usacParams_set_isParallel(
        Evision.Internal.Structurise.from_struct(self),
        [isParallel: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_loIterations(Evision.UsacParams.t()) :: integer()
  def get_loIterations(self) do
    :evision_nif.usacParams_get_loIterations(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_loIterations(Evision.UsacParams.t(), integer()) :: Evision.UsacParams.t()
  def set_loIterations(self, prop) do
    :evision_nif.usacParams_set_loIterations(
        Evision.Internal.Structurise.from_struct(self),
        [loIterations: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_loMethod(Evision.UsacParams.t()) :: Evision.LocalOptimMethod.enum()
  def get_loMethod(self) do
    :evision_nif.usacParams_get_loMethod(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_loMethod(Evision.UsacParams.t(), Evision.LocalOptimMethod.enum()) :: Evision.UsacParams.t()
  def set_loMethod(self, prop) do
    :evision_nif.usacParams_set_loMethod(
        Evision.Internal.Structurise.from_struct(self),
        [loMethod: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_loSampleSize(Evision.UsacParams.t()) :: integer()
  def get_loSampleSize(self) do
    :evision_nif.usacParams_get_loSampleSize(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_loSampleSize(Evision.UsacParams.t(), integer()) :: Evision.UsacParams.t()
  def set_loSampleSize(self, prop) do
    :evision_nif.usacParams_set_loSampleSize(
        Evision.Internal.Structurise.from_struct(self),
        [loSampleSize: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_maxIterations(Evision.UsacParams.t()) :: integer()
  def get_maxIterations(self) do
    :evision_nif.usacParams_get_maxIterations(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_maxIterations(Evision.UsacParams.t(), integer()) :: Evision.UsacParams.t()
  def set_maxIterations(self, prop) do
    :evision_nif.usacParams_set_maxIterations(
        Evision.Internal.Structurise.from_struct(self),
        [maxIterations: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_neighborsSearch(Evision.UsacParams.t()) :: Evision.NeighborSearchMethod.enum()
  def get_neighborsSearch(self) do
    :evision_nif.usacParams_get_neighborsSearch(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_neighborsSearch(Evision.UsacParams.t(), Evision.NeighborSearchMethod.enum()) :: Evision.UsacParams.t()
  def set_neighborsSearch(self, prop) do
    :evision_nif.usacParams_set_neighborsSearch(
        Evision.Internal.Structurise.from_struct(self),
        [neighborsSearch: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_randomGeneratorState(Evision.UsacParams.t()) :: integer()
  def get_randomGeneratorState(self) do
    :evision_nif.usacParams_get_randomGeneratorState(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_randomGeneratorState(Evision.UsacParams.t(), integer()) :: Evision.UsacParams.t()
  def set_randomGeneratorState(self, prop) do
    :evision_nif.usacParams_set_randomGeneratorState(
        Evision.Internal.Structurise.from_struct(self),
        [randomGeneratorState: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_sampler(Evision.UsacParams.t()) :: Evision.SamplingMethod.enum()
  def get_sampler(self) do
    :evision_nif.usacParams_get_sampler(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_sampler(Evision.UsacParams.t(), Evision.SamplingMethod.enum()) :: Evision.UsacParams.t()
  def set_sampler(self, prop) do
    :evision_nif.usacParams_set_sampler(
        Evision.Internal.Structurise.from_struct(self),
        [sampler: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_score(Evision.UsacParams.t()) :: Evision.ScoreMethod.enum()
  def get_score(self) do
    :evision_nif.usacParams_get_score(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_score(Evision.UsacParams.t(), Evision.ScoreMethod.enum()) :: Evision.UsacParams.t()
  def set_score(self, prop) do
    :evision_nif.usacParams_set_score(
        Evision.Internal.Structurise.from_struct(self),
        [score: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_threshold(Evision.UsacParams.t()) :: number()
  def get_threshold(self) do
    :evision_nif.usacParams_get_threshold(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_threshold(Evision.UsacParams.t(), number()) :: Evision.UsacParams.t()
  def set_threshold(self, prop) do
    :evision_nif.usacParams_set_threshold(
        Evision.Internal.Structurise.from_struct(self),
        [threshold: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
end
