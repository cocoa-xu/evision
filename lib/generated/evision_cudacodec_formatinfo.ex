defmodule Evision.CUDACodec.FormatInfo do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `CUDACodec.FormatInfo` struct.

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
  def to_struct({:ok, %{class: Evision.CUDACodec.FormatInfo, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.CUDACodec.FormatInfo, ref: ref}) do
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
  FormatInfo
  ##### Return
  - **self**: `FormatInfo`

  Python prototype (for reference only):
  ```python3
  FormatInfo() -> <cudacodec_FormatInfo object>
  ```
  """
  @spec formatInfo() :: Evision.CUDACodec.FormatInfo.t() | {:error, String.t()}
  def formatInfo() do
    positional = [
    ]
    :evision_nif.cudacodec_cudacodec_FormatInfo_FormatInfo(positional)
    |> to_struct()
  end
  @spec get_chromaFormat(Evision.CUDACodec.FormatInfo.t()) :: Evision.CUDACodec.ChromaFormat.t()
  def get_chromaFormat(self) do
    :evision_nif.cudacodec_FormatInfo_get_chromaFormat(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_chromaFormat(Evision.CUDACodec.FormatInfo.t(), Evision.CUDACodec.ChromaFormat.t()) :: Evision.CUDACodec.FormatInfo.t()
  def set_chromaFormat(self, prop) do
    :evision_nif.cudacodec_FormatInfo_set_chromaFormat(
        Evision.Internal.Structurise.from_struct(self),
        [chromaFormat: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_codec(Evision.CUDACodec.FormatInfo.t()) :: Evision.CUDACodec.Codec.t()
  def get_codec(self) do
    :evision_nif.cudacodec_FormatInfo_get_codec(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_codec(Evision.CUDACodec.FormatInfo.t(), Evision.CUDACodec.Codec.t()) :: Evision.CUDACodec.FormatInfo.t()
  def set_codec(self, prop) do
    :evision_nif.cudacodec_FormatInfo_set_codec(
        Evision.Internal.Structurise.from_struct(self),
        [codec: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_deinterlaceMode(Evision.CUDACodec.FormatInfo.t()) :: Evision.CUDACodec.DeinterlaceMode.t()
  def get_deinterlaceMode(self) do
    :evision_nif.cudacodec_FormatInfo_get_deinterlaceMode(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_deinterlaceMode(Evision.CUDACodec.FormatInfo.t(), Evision.CUDACodec.DeinterlaceMode.t()) :: Evision.CUDACodec.FormatInfo.t()
  def set_deinterlaceMode(self, prop) do
    :evision_nif.cudacodec_FormatInfo_set_deinterlaceMode(
        Evision.Internal.Structurise.from_struct(self),
        [deinterlaceMode: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_displayArea(Evision.CUDACodec.FormatInfo.t()) :: {number(), number(), number(), number()}
  def get_displayArea(self) do
    :evision_nif.cudacodec_FormatInfo_get_displayArea(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_displayArea(Evision.CUDACodec.FormatInfo.t(), {number(), number(), number(), number()}) :: Evision.CUDACodec.FormatInfo.t()
  def set_displayArea(self, prop) do
    :evision_nif.cudacodec_FormatInfo_set_displayArea(
        Evision.Internal.Structurise.from_struct(self),
        [displayArea: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_enableHistogram(Evision.CUDACodec.FormatInfo.t()) :: boolean()
  def get_enableHistogram(self) do
    :evision_nif.cudacodec_FormatInfo_get_enableHistogram(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_enableHistogram(Evision.CUDACodec.FormatInfo.t(), boolean()) :: Evision.CUDACodec.FormatInfo.t()
  def set_enableHistogram(self, prop) do
    :evision_nif.cudacodec_FormatInfo_set_enableHistogram(
        Evision.Internal.Structurise.from_struct(self),
        [enableHistogram: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_fps(Evision.CUDACodec.FormatInfo.t()) :: number()
  def get_fps(self) do
    :evision_nif.cudacodec_FormatInfo_get_fps(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_fps(Evision.CUDACodec.FormatInfo.t(), number()) :: Evision.CUDACodec.FormatInfo.t()
  def set_fps(self, prop) do
    :evision_nif.cudacodec_FormatInfo_set_fps(
        Evision.Internal.Structurise.from_struct(self),
        [fps: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_height(Evision.CUDACodec.FormatInfo.t()) :: integer()
  def get_height(self) do
    :evision_nif.cudacodec_FormatInfo_get_height(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_height(Evision.CUDACodec.FormatInfo.t(), integer()) :: Evision.CUDACodec.FormatInfo.t()
  def set_height(self, prop) do
    :evision_nif.cudacodec_FormatInfo_set_height(
        Evision.Internal.Structurise.from_struct(self),
        [height: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_nBitDepthChromaMinus8(Evision.CUDACodec.FormatInfo.t()) :: integer()
  def get_nBitDepthChromaMinus8(self) do
    :evision_nif.cudacodec_FormatInfo_get_nBitDepthChromaMinus8(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_nBitDepthChromaMinus8(Evision.CUDACodec.FormatInfo.t(), integer()) :: Evision.CUDACodec.FormatInfo.t()
  def set_nBitDepthChromaMinus8(self, prop) do
    :evision_nif.cudacodec_FormatInfo_set_nBitDepthChromaMinus8(
        Evision.Internal.Structurise.from_struct(self),
        [nBitDepthChromaMinus8: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_nBitDepthMinus8(Evision.CUDACodec.FormatInfo.t()) :: integer()
  def get_nBitDepthMinus8(self) do
    :evision_nif.cudacodec_FormatInfo_get_nBitDepthMinus8(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_nBitDepthMinus8(Evision.CUDACodec.FormatInfo.t(), integer()) :: Evision.CUDACodec.FormatInfo.t()
  def set_nBitDepthMinus8(self, prop) do
    :evision_nif.cudacodec_FormatInfo_set_nBitDepthMinus8(
        Evision.Internal.Structurise.from_struct(self),
        [nBitDepthMinus8: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_nCounterBitDepth(Evision.CUDACodec.FormatInfo.t()) :: integer()
  def get_nCounterBitDepth(self) do
    :evision_nif.cudacodec_FormatInfo_get_nCounterBitDepth(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_nCounterBitDepth(Evision.CUDACodec.FormatInfo.t(), integer()) :: Evision.CUDACodec.FormatInfo.t()
  def set_nCounterBitDepth(self, prop) do
    :evision_nif.cudacodec_FormatInfo_set_nCounterBitDepth(
        Evision.Internal.Structurise.from_struct(self),
        [nCounterBitDepth: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_nMaxHistogramBins(Evision.CUDACodec.FormatInfo.t()) :: integer()
  def get_nMaxHistogramBins(self) do
    :evision_nif.cudacodec_FormatInfo_get_nMaxHistogramBins(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_nMaxHistogramBins(Evision.CUDACodec.FormatInfo.t(), integer()) :: Evision.CUDACodec.FormatInfo.t()
  def set_nMaxHistogramBins(self, prop) do
    :evision_nif.cudacodec_FormatInfo_set_nMaxHistogramBins(
        Evision.Internal.Structurise.from_struct(self),
        [nMaxHistogramBins: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_srcRoi(Evision.CUDACodec.FormatInfo.t()) :: {number(), number(), number(), number()}
  def get_srcRoi(self) do
    :evision_nif.cudacodec_FormatInfo_get_srcRoi(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_srcRoi(Evision.CUDACodec.FormatInfo.t(), {number(), number(), number(), number()}) :: Evision.CUDACodec.FormatInfo.t()
  def set_srcRoi(self, prop) do
    :evision_nif.cudacodec_FormatInfo_set_srcRoi(
        Evision.Internal.Structurise.from_struct(self),
        [srcRoi: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_targetRoi(Evision.CUDACodec.FormatInfo.t()) :: {number(), number(), number(), number()}
  def get_targetRoi(self) do
    :evision_nif.cudacodec_FormatInfo_get_targetRoi(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_targetRoi(Evision.CUDACodec.FormatInfo.t(), {number(), number(), number(), number()}) :: Evision.CUDACodec.FormatInfo.t()
  def set_targetRoi(self, prop) do
    :evision_nif.cudacodec_FormatInfo_set_targetRoi(
        Evision.Internal.Structurise.from_struct(self),
        [targetRoi: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_targetSz(Evision.CUDACodec.FormatInfo.t()) :: {number(), number()}
  def get_targetSz(self) do
    :evision_nif.cudacodec_FormatInfo_get_targetSz(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_targetSz(Evision.CUDACodec.FormatInfo.t(), {number(), number()}) :: Evision.CUDACodec.FormatInfo.t()
  def set_targetSz(self, prop) do
    :evision_nif.cudacodec_FormatInfo_set_targetSz(
        Evision.Internal.Structurise.from_struct(self),
        [targetSz: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_ulHeight(Evision.CUDACodec.FormatInfo.t()) :: integer()
  def get_ulHeight(self) do
    :evision_nif.cudacodec_FormatInfo_get_ulHeight(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_ulHeight(Evision.CUDACodec.FormatInfo.t(), integer()) :: Evision.CUDACodec.FormatInfo.t()
  def set_ulHeight(self, prop) do
    :evision_nif.cudacodec_FormatInfo_set_ulHeight(
        Evision.Internal.Structurise.from_struct(self),
        [ulHeight: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_ulNumDecodeSurfaces(Evision.CUDACodec.FormatInfo.t()) :: integer()
  def get_ulNumDecodeSurfaces(self) do
    :evision_nif.cudacodec_FormatInfo_get_ulNumDecodeSurfaces(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_ulNumDecodeSurfaces(Evision.CUDACodec.FormatInfo.t(), integer()) :: Evision.CUDACodec.FormatInfo.t()
  def set_ulNumDecodeSurfaces(self, prop) do
    :evision_nif.cudacodec_FormatInfo_set_ulNumDecodeSurfaces(
        Evision.Internal.Structurise.from_struct(self),
        [ulNumDecodeSurfaces: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_ulWidth(Evision.CUDACodec.FormatInfo.t()) :: integer()
  def get_ulWidth(self) do
    :evision_nif.cudacodec_FormatInfo_get_ulWidth(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_ulWidth(Evision.CUDACodec.FormatInfo.t(), integer()) :: Evision.CUDACodec.FormatInfo.t()
  def set_ulWidth(self, prop) do
    :evision_nif.cudacodec_FormatInfo_set_ulWidth(
        Evision.Internal.Structurise.from_struct(self),
        [ulWidth: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_valid(Evision.CUDACodec.FormatInfo.t()) :: boolean()
  def get_valid(self) do
    :evision_nif.cudacodec_FormatInfo_get_valid(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_valid(Evision.CUDACodec.FormatInfo.t(), boolean()) :: Evision.CUDACodec.FormatInfo.t()
  def set_valid(self, prop) do
    :evision_nif.cudacodec_FormatInfo_set_valid(
        Evision.Internal.Structurise.from_struct(self),
        [valid: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_videoFullRangeFlag(Evision.CUDACodec.FormatInfo.t()) :: boolean()
  def get_videoFullRangeFlag(self) do
    :evision_nif.cudacodec_FormatInfo_get_videoFullRangeFlag(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_videoFullRangeFlag(Evision.CUDACodec.FormatInfo.t(), boolean()) :: Evision.CUDACodec.FormatInfo.t()
  def set_videoFullRangeFlag(self, prop) do
    :evision_nif.cudacodec_FormatInfo_set_videoFullRangeFlag(
        Evision.Internal.Structurise.from_struct(self),
        [videoFullRangeFlag: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_width(Evision.CUDACodec.FormatInfo.t()) :: integer()
  def get_width(self) do
    :evision_nif.cudacodec_FormatInfo_get_width(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_width(Evision.CUDACodec.FormatInfo.t(), integer()) :: Evision.CUDACodec.FormatInfo.t()
  def set_width(self, prop) do
    :evision_nif.cudacodec_FormatInfo_set_width(
        Evision.Internal.Structurise.from_struct(self),
        [width: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
end
