defmodule Evision.XImgProc.EdgeDrawing.Params do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `XImgProc.EdgeDrawing.Params` struct.

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
  def to_struct({:ok, %{class: Evision.XImgProc.EdgeDrawing.Params, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.XImgProc.EdgeDrawing.Params, ref: ref}) do
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
  EdgeDrawing_Params
  ##### Return
  - **self**: `Evision.XImgProc.EdgeDrawing.Params.t()`

  Python prototype (for reference only):
  ```python3
  EdgeDrawing_Params() -> <ximgproc_EdgeDrawing_Params object>
  ```
  """
  @spec ximgproc_EdgeDrawing_Params() :: Evision.XImgProc.EdgeDrawing.Params.t() | {:error, String.t()}
  def ximgproc_EdgeDrawing_Params() do
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_EdgeDrawing_Params_EdgeDrawing_Params(positional)
    |> to_struct()
  end
  @spec get_AnchorThresholdValue(Evision.XImgProc.EdgeDrawing.Params.t()) :: integer()
  def get_AnchorThresholdValue(self) do
    :evision_nif.ximgproc_EdgeDrawing_Params_get_AnchorThresholdValue(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_AnchorThresholdValue(Evision.XImgProc.EdgeDrawing.Params.t(), integer()) :: Evision.XImgProc.EdgeDrawing.Params.t()
  def set_AnchorThresholdValue(self, prop) do
    :evision_nif.ximgproc_EdgeDrawing_Params_set_AnchorThresholdValue(
        Evision.Internal.Structurise.from_struct(self),
        [AnchorThresholdValue: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_EdgeDetectionOperator(Evision.XImgProc.EdgeDrawing.Params.t()) :: integer()
  def get_EdgeDetectionOperator(self) do
    :evision_nif.ximgproc_EdgeDrawing_Params_get_EdgeDetectionOperator(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_EdgeDetectionOperator(Evision.XImgProc.EdgeDrawing.Params.t(), integer()) :: Evision.XImgProc.EdgeDrawing.Params.t()
  def set_EdgeDetectionOperator(self, prop) do
    :evision_nif.ximgproc_EdgeDrawing_Params_set_EdgeDetectionOperator(
        Evision.Internal.Structurise.from_struct(self),
        [EdgeDetectionOperator: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_GradientThresholdValue(Evision.XImgProc.EdgeDrawing.Params.t()) :: integer()
  def get_GradientThresholdValue(self) do
    :evision_nif.ximgproc_EdgeDrawing_Params_get_GradientThresholdValue(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_GradientThresholdValue(Evision.XImgProc.EdgeDrawing.Params.t(), integer()) :: Evision.XImgProc.EdgeDrawing.Params.t()
  def set_GradientThresholdValue(self, prop) do
    :evision_nif.ximgproc_EdgeDrawing_Params_set_GradientThresholdValue(
        Evision.Internal.Structurise.from_struct(self),
        [GradientThresholdValue: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_LineFitErrorThreshold(Evision.XImgProc.EdgeDrawing.Params.t()) :: number()
  def get_LineFitErrorThreshold(self) do
    :evision_nif.ximgproc_EdgeDrawing_Params_get_LineFitErrorThreshold(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_LineFitErrorThreshold(Evision.XImgProc.EdgeDrawing.Params.t(), number()) :: Evision.XImgProc.EdgeDrawing.Params.t()
  def set_LineFitErrorThreshold(self, prop) do
    :evision_nif.ximgproc_EdgeDrawing_Params_set_LineFitErrorThreshold(
        Evision.Internal.Structurise.from_struct(self),
        [LineFitErrorThreshold: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_MaxDistanceBetweenTwoLines(Evision.XImgProc.EdgeDrawing.Params.t()) :: number()
  def get_MaxDistanceBetweenTwoLines(self) do
    :evision_nif.ximgproc_EdgeDrawing_Params_get_MaxDistanceBetweenTwoLines(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_MaxDistanceBetweenTwoLines(Evision.XImgProc.EdgeDrawing.Params.t(), number()) :: Evision.XImgProc.EdgeDrawing.Params.t()
  def set_MaxDistanceBetweenTwoLines(self, prop) do
    :evision_nif.ximgproc_EdgeDrawing_Params_set_MaxDistanceBetweenTwoLines(
        Evision.Internal.Structurise.from_struct(self),
        [MaxDistanceBetweenTwoLines: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_MaxErrorThreshold(Evision.XImgProc.EdgeDrawing.Params.t()) :: number()
  def get_MaxErrorThreshold(self) do
    :evision_nif.ximgproc_EdgeDrawing_Params_get_MaxErrorThreshold(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_MaxErrorThreshold(Evision.XImgProc.EdgeDrawing.Params.t(), number()) :: Evision.XImgProc.EdgeDrawing.Params.t()
  def set_MaxErrorThreshold(self, prop) do
    :evision_nif.ximgproc_EdgeDrawing_Params_set_MaxErrorThreshold(
        Evision.Internal.Structurise.from_struct(self),
        [MaxErrorThreshold: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_MinLineLength(Evision.XImgProc.EdgeDrawing.Params.t()) :: integer()
  def get_MinLineLength(self) do
    :evision_nif.ximgproc_EdgeDrawing_Params_get_MinLineLength(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_MinLineLength(Evision.XImgProc.EdgeDrawing.Params.t(), integer()) :: Evision.XImgProc.EdgeDrawing.Params.t()
  def set_MinLineLength(self, prop) do
    :evision_nif.ximgproc_EdgeDrawing_Params_set_MinLineLength(
        Evision.Internal.Structurise.from_struct(self),
        [MinLineLength: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_MinPathLength(Evision.XImgProc.EdgeDrawing.Params.t()) :: integer()
  def get_MinPathLength(self) do
    :evision_nif.ximgproc_EdgeDrawing_Params_get_MinPathLength(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_MinPathLength(Evision.XImgProc.EdgeDrawing.Params.t(), integer()) :: Evision.XImgProc.EdgeDrawing.Params.t()
  def set_MinPathLength(self, prop) do
    :evision_nif.ximgproc_EdgeDrawing_Params_set_MinPathLength(
        Evision.Internal.Structurise.from_struct(self),
        [MinPathLength: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_NFAValidation(Evision.XImgProc.EdgeDrawing.Params.t()) :: boolean()
  def get_NFAValidation(self) do
    :evision_nif.ximgproc_EdgeDrawing_Params_get_NFAValidation(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_NFAValidation(Evision.XImgProc.EdgeDrawing.Params.t(), boolean()) :: Evision.XImgProc.EdgeDrawing.Params.t()
  def set_NFAValidation(self, prop) do
    :evision_nif.ximgproc_EdgeDrawing_Params_set_NFAValidation(
        Evision.Internal.Structurise.from_struct(self),
        [NFAValidation: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_PFmode(Evision.XImgProc.EdgeDrawing.Params.t()) :: boolean()
  def get_PFmode(self) do
    :evision_nif.ximgproc_EdgeDrawing_Params_get_PFmode(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_PFmode(Evision.XImgProc.EdgeDrawing.Params.t(), boolean()) :: Evision.XImgProc.EdgeDrawing.Params.t()
  def set_PFmode(self, prop) do
    :evision_nif.ximgproc_EdgeDrawing_Params_set_PFmode(
        Evision.Internal.Structurise.from_struct(self),
        [PFmode: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_ScanInterval(Evision.XImgProc.EdgeDrawing.Params.t()) :: integer()
  def get_ScanInterval(self) do
    :evision_nif.ximgproc_EdgeDrawing_Params_get_ScanInterval(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_ScanInterval(Evision.XImgProc.EdgeDrawing.Params.t(), integer()) :: Evision.XImgProc.EdgeDrawing.Params.t()
  def set_ScanInterval(self, prop) do
    :evision_nif.ximgproc_EdgeDrawing_Params_set_ScanInterval(
        Evision.Internal.Structurise.from_struct(self),
        [ScanInterval: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_Sigma(Evision.XImgProc.EdgeDrawing.Params.t()) :: number()
  def get_Sigma(self) do
    :evision_nif.ximgproc_EdgeDrawing_Params_get_Sigma(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_Sigma(Evision.XImgProc.EdgeDrawing.Params.t(), number()) :: Evision.XImgProc.EdgeDrawing.Params.t()
  def set_Sigma(self, prop) do
    :evision_nif.ximgproc_EdgeDrawing_Params_set_Sigma(
        Evision.Internal.Structurise.from_struct(self),
        [Sigma: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_SumFlag(Evision.XImgProc.EdgeDrawing.Params.t()) :: boolean()
  def get_SumFlag(self) do
    :evision_nif.ximgproc_EdgeDrawing_Params_get_SumFlag(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_SumFlag(Evision.XImgProc.EdgeDrawing.Params.t(), boolean()) :: Evision.XImgProc.EdgeDrawing.Params.t()
  def set_SumFlag(self, prop) do
    :evision_nif.ximgproc_EdgeDrawing_Params_set_SumFlag(
        Evision.Internal.Structurise.from_struct(self),
        [SumFlag: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
end
