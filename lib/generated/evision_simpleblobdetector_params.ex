defmodule Evision.SimpleBlobDetector.Params do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `SimpleBlobDetector.Params` struct.

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
  def to_struct({:ok, %{class: Evision.SimpleBlobDetector.Params, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.SimpleBlobDetector.Params, ref: ref}) do
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
  SimpleBlobDetector_Params
  ##### Return
  - **self**: `Evision.SimpleBlobDetector.Params.t()`

  Python prototype (for reference only):
  ```python3
  SimpleBlobDetector_Params() -> <SimpleBlobDetector_Params object>
  ```
  """
  @spec params() :: Evision.SimpleBlobDetector.Params.t() | {:error, String.t()}
  def params() do
    positional = [
    ]
    :evision_nif.simpleBlobDetector_Params_SimpleBlobDetector_Params(positional)
    |> to_struct()
  end
  @spec get_blobColor(Evision.SimpleBlobDetector.Params.t()) :: integer()
  def get_blobColor(self) do
    :evision_nif.simpleBlobDetector_Params_get_blobColor(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_blobColor(Evision.SimpleBlobDetector.Params.t(), integer()) :: Evision.SimpleBlobDetector.Params.t()
  def set_blobColor(self, prop) do
    :evision_nif.simpleBlobDetector_Params_set_blobColor(
        Evision.Internal.Structurise.from_struct(self),
        [blobColor: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_collectContours(Evision.SimpleBlobDetector.Params.t()) :: boolean()
  def get_collectContours(self) do
    :evision_nif.simpleBlobDetector_Params_get_collectContours(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_collectContours(Evision.SimpleBlobDetector.Params.t(), boolean()) :: Evision.SimpleBlobDetector.Params.t()
  def set_collectContours(self, prop) do
    :evision_nif.simpleBlobDetector_Params_set_collectContours(
        Evision.Internal.Structurise.from_struct(self),
        [collectContours: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_filterByArea(Evision.SimpleBlobDetector.Params.t()) :: boolean()
  def get_filterByArea(self) do
    :evision_nif.simpleBlobDetector_Params_get_filterByArea(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_filterByArea(Evision.SimpleBlobDetector.Params.t(), boolean()) :: Evision.SimpleBlobDetector.Params.t()
  def set_filterByArea(self, prop) do
    :evision_nif.simpleBlobDetector_Params_set_filterByArea(
        Evision.Internal.Structurise.from_struct(self),
        [filterByArea: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_filterByCircularity(Evision.SimpleBlobDetector.Params.t()) :: boolean()
  def get_filterByCircularity(self) do
    :evision_nif.simpleBlobDetector_Params_get_filterByCircularity(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_filterByCircularity(Evision.SimpleBlobDetector.Params.t(), boolean()) :: Evision.SimpleBlobDetector.Params.t()
  def set_filterByCircularity(self, prop) do
    :evision_nif.simpleBlobDetector_Params_set_filterByCircularity(
        Evision.Internal.Structurise.from_struct(self),
        [filterByCircularity: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_filterByColor(Evision.SimpleBlobDetector.Params.t()) :: boolean()
  def get_filterByColor(self) do
    :evision_nif.simpleBlobDetector_Params_get_filterByColor(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_filterByColor(Evision.SimpleBlobDetector.Params.t(), boolean()) :: Evision.SimpleBlobDetector.Params.t()
  def set_filterByColor(self, prop) do
    :evision_nif.simpleBlobDetector_Params_set_filterByColor(
        Evision.Internal.Structurise.from_struct(self),
        [filterByColor: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_filterByConvexity(Evision.SimpleBlobDetector.Params.t()) :: boolean()
  def get_filterByConvexity(self) do
    :evision_nif.simpleBlobDetector_Params_get_filterByConvexity(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_filterByConvexity(Evision.SimpleBlobDetector.Params.t(), boolean()) :: Evision.SimpleBlobDetector.Params.t()
  def set_filterByConvexity(self, prop) do
    :evision_nif.simpleBlobDetector_Params_set_filterByConvexity(
        Evision.Internal.Structurise.from_struct(self),
        [filterByConvexity: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_filterByInertia(Evision.SimpleBlobDetector.Params.t()) :: boolean()
  def get_filterByInertia(self) do
    :evision_nif.simpleBlobDetector_Params_get_filterByInertia(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_filterByInertia(Evision.SimpleBlobDetector.Params.t(), boolean()) :: Evision.SimpleBlobDetector.Params.t()
  def set_filterByInertia(self, prop) do
    :evision_nif.simpleBlobDetector_Params_set_filterByInertia(
        Evision.Internal.Structurise.from_struct(self),
        [filterByInertia: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_maxArea(Evision.SimpleBlobDetector.Params.t()) :: number()
  def get_maxArea(self) do
    :evision_nif.simpleBlobDetector_Params_get_maxArea(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_maxArea(Evision.SimpleBlobDetector.Params.t(), number()) :: Evision.SimpleBlobDetector.Params.t()
  def set_maxArea(self, prop) do
    :evision_nif.simpleBlobDetector_Params_set_maxArea(
        Evision.Internal.Structurise.from_struct(self),
        [maxArea: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_maxCircularity(Evision.SimpleBlobDetector.Params.t()) :: number()
  def get_maxCircularity(self) do
    :evision_nif.simpleBlobDetector_Params_get_maxCircularity(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_maxCircularity(Evision.SimpleBlobDetector.Params.t(), number()) :: Evision.SimpleBlobDetector.Params.t()
  def set_maxCircularity(self, prop) do
    :evision_nif.simpleBlobDetector_Params_set_maxCircularity(
        Evision.Internal.Structurise.from_struct(self),
        [maxCircularity: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_maxConvexity(Evision.SimpleBlobDetector.Params.t()) :: number()
  def get_maxConvexity(self) do
    :evision_nif.simpleBlobDetector_Params_get_maxConvexity(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_maxConvexity(Evision.SimpleBlobDetector.Params.t(), number()) :: Evision.SimpleBlobDetector.Params.t()
  def set_maxConvexity(self, prop) do
    :evision_nif.simpleBlobDetector_Params_set_maxConvexity(
        Evision.Internal.Structurise.from_struct(self),
        [maxConvexity: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_maxInertiaRatio(Evision.SimpleBlobDetector.Params.t()) :: number()
  def get_maxInertiaRatio(self) do
    :evision_nif.simpleBlobDetector_Params_get_maxInertiaRatio(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_maxInertiaRatio(Evision.SimpleBlobDetector.Params.t(), number()) :: Evision.SimpleBlobDetector.Params.t()
  def set_maxInertiaRatio(self, prop) do
    :evision_nif.simpleBlobDetector_Params_set_maxInertiaRatio(
        Evision.Internal.Structurise.from_struct(self),
        [maxInertiaRatio: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_maxThreshold(Evision.SimpleBlobDetector.Params.t()) :: number()
  def get_maxThreshold(self) do
    :evision_nif.simpleBlobDetector_Params_get_maxThreshold(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_maxThreshold(Evision.SimpleBlobDetector.Params.t(), number()) :: Evision.SimpleBlobDetector.Params.t()
  def set_maxThreshold(self, prop) do
    :evision_nif.simpleBlobDetector_Params_set_maxThreshold(
        Evision.Internal.Structurise.from_struct(self),
        [maxThreshold: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_minArea(Evision.SimpleBlobDetector.Params.t()) :: number()
  def get_minArea(self) do
    :evision_nif.simpleBlobDetector_Params_get_minArea(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_minArea(Evision.SimpleBlobDetector.Params.t(), number()) :: Evision.SimpleBlobDetector.Params.t()
  def set_minArea(self, prop) do
    :evision_nif.simpleBlobDetector_Params_set_minArea(
        Evision.Internal.Structurise.from_struct(self),
        [minArea: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_minCircularity(Evision.SimpleBlobDetector.Params.t()) :: number()
  def get_minCircularity(self) do
    :evision_nif.simpleBlobDetector_Params_get_minCircularity(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_minCircularity(Evision.SimpleBlobDetector.Params.t(), number()) :: Evision.SimpleBlobDetector.Params.t()
  def set_minCircularity(self, prop) do
    :evision_nif.simpleBlobDetector_Params_set_minCircularity(
        Evision.Internal.Structurise.from_struct(self),
        [minCircularity: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_minConvexity(Evision.SimpleBlobDetector.Params.t()) :: number()
  def get_minConvexity(self) do
    :evision_nif.simpleBlobDetector_Params_get_minConvexity(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_minConvexity(Evision.SimpleBlobDetector.Params.t(), number()) :: Evision.SimpleBlobDetector.Params.t()
  def set_minConvexity(self, prop) do
    :evision_nif.simpleBlobDetector_Params_set_minConvexity(
        Evision.Internal.Structurise.from_struct(self),
        [minConvexity: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_minDistBetweenBlobs(Evision.SimpleBlobDetector.Params.t()) :: number()
  def get_minDistBetweenBlobs(self) do
    :evision_nif.simpleBlobDetector_Params_get_minDistBetweenBlobs(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_minDistBetweenBlobs(Evision.SimpleBlobDetector.Params.t(), number()) :: Evision.SimpleBlobDetector.Params.t()
  def set_minDistBetweenBlobs(self, prop) do
    :evision_nif.simpleBlobDetector_Params_set_minDistBetweenBlobs(
        Evision.Internal.Structurise.from_struct(self),
        [minDistBetweenBlobs: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_minInertiaRatio(Evision.SimpleBlobDetector.Params.t()) :: number()
  def get_minInertiaRatio(self) do
    :evision_nif.simpleBlobDetector_Params_get_minInertiaRatio(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_minInertiaRatio(Evision.SimpleBlobDetector.Params.t(), number()) :: Evision.SimpleBlobDetector.Params.t()
  def set_minInertiaRatio(self, prop) do
    :evision_nif.simpleBlobDetector_Params_set_minInertiaRatio(
        Evision.Internal.Structurise.from_struct(self),
        [minInertiaRatio: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_minRepeatability(Evision.SimpleBlobDetector.Params.t()) :: integer()
  def get_minRepeatability(self) do
    :evision_nif.simpleBlobDetector_Params_get_minRepeatability(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_minRepeatability(Evision.SimpleBlobDetector.Params.t(), integer()) :: Evision.SimpleBlobDetector.Params.t()
  def set_minRepeatability(self, prop) do
    :evision_nif.simpleBlobDetector_Params_set_minRepeatability(
        Evision.Internal.Structurise.from_struct(self),
        [minRepeatability: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_minThreshold(Evision.SimpleBlobDetector.Params.t()) :: number()
  def get_minThreshold(self) do
    :evision_nif.simpleBlobDetector_Params_get_minThreshold(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_minThreshold(Evision.SimpleBlobDetector.Params.t(), number()) :: Evision.SimpleBlobDetector.Params.t()
  def set_minThreshold(self, prop) do
    :evision_nif.simpleBlobDetector_Params_set_minThreshold(
        Evision.Internal.Structurise.from_struct(self),
        [minThreshold: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_thresholdStep(Evision.SimpleBlobDetector.Params.t()) :: number()
  def get_thresholdStep(self) do
    :evision_nif.simpleBlobDetector_Params_get_thresholdStep(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_thresholdStep(Evision.SimpleBlobDetector.Params.t(), number()) :: Evision.SimpleBlobDetector.Params.t()
  def set_thresholdStep(self, prop) do
    :evision_nif.simpleBlobDetector_Params_set_thresholdStep(
        Evision.Internal.Structurise.from_struct(self),
        [thresholdStep: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
end
