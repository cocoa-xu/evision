defmodule Evision.XImgProc.SelectiveSearchSegmentationStrategyMultiple do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `XImgProc.SelectiveSearchSegmentationStrategyMultiple` struct.

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
  def to_struct({:ok, %{class: Evision.XImgProc.SelectiveSearchSegmentationStrategyMultiple, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.XImgProc.SelectiveSearchSegmentationStrategyMultiple, ref: ref}) do
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
  Add a new sub-strategy

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.SelectiveSearchSegmentationStrategyMultiple.t()`
  - **g**: `Evision.XImgProc.SelectiveSearchSegmentationStrategy.t()`.

    The strategy

  - **weight**: `float`.

    The weight of the strategy

  Python prototype (for reference only):
  ```python3
  addStrategy(g, weight) -> None
  ```
  """
  @spec addStrategy(Evision.XImgProc.SelectiveSearchSegmentationStrategyMultiple.t(), Evision.XImgProc.SelectiveSearchSegmentationStrategy.t(), number()) :: Evision.XImgProc.SelectiveSearchSegmentationStrategyMultiple.t() | {:error, String.t()}
  def addStrategy(self, g, weight) when is_struct(g, Evision.XImgProc.SelectiveSearchSegmentationStrategy) and is_float(weight)
  do
    positional = [
      g: Evision.Internal.Structurise.from_struct(g),
      weight: Evision.Internal.Structurise.from_struct(weight)
    ]
    :evision_nif.ximgproc_segmentation_ximgproc_segmentation_SelectiveSearchSegmentationStrategyMultiple_addStrategy(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Remove all sub-strategies

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.SelectiveSearchSegmentationStrategyMultiple.t()`

  Python prototype (for reference only):
  ```python3
  clearStrategies() -> None
  ```
  """
  @spec clearStrategies(Evision.XImgProc.SelectiveSearchSegmentationStrategyMultiple.t()) :: Evision.XImgProc.SelectiveSearchSegmentationStrategyMultiple.t() | {:error, String.t()}
  def clearStrategies(self) do
    positional = [
    ]
    :evision_nif.ximgproc_segmentation_ximgproc_segmentation_SelectiveSearchSegmentationStrategyMultiple_clearStrategies(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
