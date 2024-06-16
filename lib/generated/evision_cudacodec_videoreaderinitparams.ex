defmodule Evision.CUDACodec.VideoReaderInitParams do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `CUDACodec.VideoReaderInitParams` struct.

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
  def to_struct({:ok, %{class: Evision.CUDACodec.VideoReaderInitParams, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.CUDACodec.VideoReaderInitParams, ref: ref}) do
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
  VideoReaderInitParams
  ##### Return
  - **self**: `VideoReaderInitParams`

  Python prototype (for reference only):
  ```python3
  VideoReaderInitParams() -> <cudacodec_VideoReaderInitParams object>
  ```
  """
  @spec videoReaderInitParams() :: Evision.CUDACodec.VideoReaderInitParams.t() | {:error, String.t()}
  def videoReaderInitParams() do
    positional = [
    ]
    :evision_nif.cudacodec_cudacodec_VideoReaderInitParams_VideoReaderInitParams(positional)
    |> to_struct()
  end
  @spec get_allowFrameDrop(Evision.CUDACodec.VideoReaderInitParams.t()) :: boolean()
  def get_allowFrameDrop(self) do
    :evision_nif.cudacodec_VideoReaderInitParams_get_allowFrameDrop(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_allowFrameDrop(Evision.CUDACodec.VideoReaderInitParams.t(), boolean()) :: Evision.CUDACodec.VideoReaderInitParams.t()
  def set_allowFrameDrop(self, prop) do
    :evision_nif.cudacodec_VideoReaderInitParams_set_allowFrameDrop(
        Evision.Internal.Structurise.from_struct(self),
        [allowFrameDrop: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_enableHistogram(Evision.CUDACodec.VideoReaderInitParams.t()) :: boolean()
  def get_enableHistogram(self) do
    :evision_nif.cudacodec_VideoReaderInitParams_get_enableHistogram(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_enableHistogram(Evision.CUDACodec.VideoReaderInitParams.t(), boolean()) :: Evision.CUDACodec.VideoReaderInitParams.t()
  def set_enableHistogram(self, prop) do
    :evision_nif.cudacodec_VideoReaderInitParams_set_enableHistogram(
        Evision.Internal.Structurise.from_struct(self),
        [enableHistogram: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_firstFrameIdx(Evision.CUDACodec.VideoReaderInitParams.t()) :: integer()
  def get_firstFrameIdx(self) do
    :evision_nif.cudacodec_VideoReaderInitParams_get_firstFrameIdx(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_firstFrameIdx(Evision.CUDACodec.VideoReaderInitParams.t(), integer()) :: Evision.CUDACodec.VideoReaderInitParams.t()
  def set_firstFrameIdx(self, prop) do
    :evision_nif.cudacodec_VideoReaderInitParams_set_firstFrameIdx(
        Evision.Internal.Structurise.from_struct(self),
        [firstFrameIdx: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_minNumDecodeSurfaces(Evision.CUDACodec.VideoReaderInitParams.t()) :: integer()
  def get_minNumDecodeSurfaces(self) do
    :evision_nif.cudacodec_VideoReaderInitParams_get_minNumDecodeSurfaces(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_minNumDecodeSurfaces(Evision.CUDACodec.VideoReaderInitParams.t(), integer()) :: Evision.CUDACodec.VideoReaderInitParams.t()
  def set_minNumDecodeSurfaces(self, prop) do
    :evision_nif.cudacodec_VideoReaderInitParams_set_minNumDecodeSurfaces(
        Evision.Internal.Structurise.from_struct(self),
        [minNumDecodeSurfaces: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_rawMode(Evision.CUDACodec.VideoReaderInitParams.t()) :: boolean()
  def get_rawMode(self) do
    :evision_nif.cudacodec_VideoReaderInitParams_get_rawMode(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_rawMode(Evision.CUDACodec.VideoReaderInitParams.t(), boolean()) :: Evision.CUDACodec.VideoReaderInitParams.t()
  def set_rawMode(self, prop) do
    :evision_nif.cudacodec_VideoReaderInitParams_set_rawMode(
        Evision.Internal.Structurise.from_struct(self),
        [rawMode: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_srcRoi(Evision.CUDACodec.VideoReaderInitParams.t()) :: {number(), number(), number(), number()}
  def get_srcRoi(self) do
    :evision_nif.cudacodec_VideoReaderInitParams_get_srcRoi(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_srcRoi(Evision.CUDACodec.VideoReaderInitParams.t(), {number(), number(), number(), number()}) :: Evision.CUDACodec.VideoReaderInitParams.t()
  def set_srcRoi(self, prop) do
    :evision_nif.cudacodec_VideoReaderInitParams_set_srcRoi(
        Evision.Internal.Structurise.from_struct(self),
        [srcRoi: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_targetRoi(Evision.CUDACodec.VideoReaderInitParams.t()) :: {number(), number(), number(), number()}
  def get_targetRoi(self) do
    :evision_nif.cudacodec_VideoReaderInitParams_get_targetRoi(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_targetRoi(Evision.CUDACodec.VideoReaderInitParams.t(), {number(), number(), number(), number()}) :: Evision.CUDACodec.VideoReaderInitParams.t()
  def set_targetRoi(self, prop) do
    :evision_nif.cudacodec_VideoReaderInitParams_set_targetRoi(
        Evision.Internal.Structurise.from_struct(self),
        [targetRoi: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_targetSz(Evision.CUDACodec.VideoReaderInitParams.t()) :: {number(), number()}
  def get_targetSz(self) do
    :evision_nif.cudacodec_VideoReaderInitParams_get_targetSz(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_targetSz(Evision.CUDACodec.VideoReaderInitParams.t(), {number(), number()}) :: Evision.CUDACodec.VideoReaderInitParams.t()
  def set_targetSz(self, prop) do
    :evision_nif.cudacodec_VideoReaderInitParams_set_targetSz(
        Evision.Internal.Structurise.from_struct(self),
        [targetSz: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_udpSource(Evision.CUDACodec.VideoReaderInitParams.t()) :: boolean()
  def get_udpSource(self) do
    :evision_nif.cudacodec_VideoReaderInitParams_get_udpSource(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_udpSource(Evision.CUDACodec.VideoReaderInitParams.t(), boolean()) :: Evision.CUDACodec.VideoReaderInitParams.t()
  def set_udpSource(self, prop) do
    :evision_nif.cudacodec_VideoReaderInitParams_set_udpSource(
        Evision.Internal.Structurise.from_struct(self),
        [udpSource: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
end
