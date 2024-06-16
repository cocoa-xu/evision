defmodule Evision.CUDACodec.EncoderParams do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `CUDACodec.EncoderParams` struct.

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
  def to_struct({:ok, %{class: Evision.CUDACodec.EncoderParams, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.CUDACodec.EncoderParams, ref: ref}) do
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
  EncoderParams
  ##### Return
  - **self**: `EncoderParams`

  Python prototype (for reference only):
  ```python3
  EncoderParams() -> <cudacodec_EncoderParams object>
  ```
  """
  @spec encoderParams() :: Evision.CUDACodec.EncoderParams.t() | {:error, String.t()}
  def encoderParams() do
    positional = [
    ]
    :evision_nif.cudacodec_cudacodec_EncoderParams_EncoderParams(positional)
    |> to_struct()
  end
  @spec get_averageBitRate(Evision.CUDACodec.EncoderParams.t()) :: integer()
  def get_averageBitRate(self) do
    :evision_nif.cudacodec_EncoderParams_get_averageBitRate(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_averageBitRate(Evision.CUDACodec.EncoderParams.t(), integer()) :: Evision.CUDACodec.EncoderParams.t()
  def set_averageBitRate(self, prop) do
    :evision_nif.cudacodec_EncoderParams_set_averageBitRate(
        Evision.Internal.Structurise.from_struct(self),
        [averageBitRate: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_constQp(Evision.CUDACodec.EncoderParams.t()) :: Evision.CUDACodec.EncodeQp.t()
  def get_constQp(self) do
    :evision_nif.cudacodec_EncoderParams_get_constQp(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_constQp(Evision.CUDACodec.EncoderParams.t(), Evision.CUDACodec.EncodeQp.t()) :: Evision.CUDACodec.EncoderParams.t()
  def set_constQp(self, prop) do
    :evision_nif.cudacodec_EncoderParams_set_constQp(
        Evision.Internal.Structurise.from_struct(self),
        [constQp: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_encodingProfile(Evision.CUDACodec.EncoderParams.t()) :: Evision.CUDACodec.EncodeProfile.t()
  def get_encodingProfile(self) do
    :evision_nif.cudacodec_EncoderParams_get_encodingProfile(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_encodingProfile(Evision.CUDACodec.EncoderParams.t(), Evision.CUDACodec.EncodeProfile.t()) :: Evision.CUDACodec.EncoderParams.t()
  def set_encodingProfile(self, prop) do
    :evision_nif.cudacodec_EncoderParams_set_encodingProfile(
        Evision.Internal.Structurise.from_struct(self),
        [encodingProfile: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_gopLength(Evision.CUDACodec.EncoderParams.t()) :: integer()
  def get_gopLength(self) do
    :evision_nif.cudacodec_EncoderParams_get_gopLength(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_gopLength(Evision.CUDACodec.EncoderParams.t(), integer()) :: Evision.CUDACodec.EncoderParams.t()
  def set_gopLength(self, prop) do
    :evision_nif.cudacodec_EncoderParams_set_gopLength(
        Evision.Internal.Structurise.from_struct(self),
        [gopLength: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_idrPeriod(Evision.CUDACodec.EncoderParams.t()) :: integer()
  def get_idrPeriod(self) do
    :evision_nif.cudacodec_EncoderParams_get_idrPeriod(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_idrPeriod(Evision.CUDACodec.EncoderParams.t(), integer()) :: Evision.CUDACodec.EncoderParams.t()
  def set_idrPeriod(self, prop) do
    :evision_nif.cudacodec_EncoderParams_set_idrPeriod(
        Evision.Internal.Structurise.from_struct(self),
        [idrPeriod: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_maxBitRate(Evision.CUDACodec.EncoderParams.t()) :: integer()
  def get_maxBitRate(self) do
    :evision_nif.cudacodec_EncoderParams_get_maxBitRate(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_maxBitRate(Evision.CUDACodec.EncoderParams.t(), integer()) :: Evision.CUDACodec.EncoderParams.t()
  def set_maxBitRate(self, prop) do
    :evision_nif.cudacodec_EncoderParams_set_maxBitRate(
        Evision.Internal.Structurise.from_struct(self),
        [maxBitRate: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_multiPassEncoding(Evision.CUDACodec.EncoderParams.t()) :: Evision.CUDACodec.EncodeMultiPass.t()
  def get_multiPassEncoding(self) do
    :evision_nif.cudacodec_EncoderParams_get_multiPassEncoding(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_multiPassEncoding(Evision.CUDACodec.EncoderParams.t(), Evision.CUDACodec.EncodeMultiPass.t()) :: Evision.CUDACodec.EncoderParams.t()
  def set_multiPassEncoding(self, prop) do
    :evision_nif.cudacodec_EncoderParams_set_multiPassEncoding(
        Evision.Internal.Structurise.from_struct(self),
        [multiPassEncoding: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_nvPreset(Evision.CUDACodec.EncoderParams.t()) :: Evision.CUDACodec.EncodePreset.t()
  def get_nvPreset(self) do
    :evision_nif.cudacodec_EncoderParams_get_nvPreset(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_nvPreset(Evision.CUDACodec.EncoderParams.t(), Evision.CUDACodec.EncodePreset.t()) :: Evision.CUDACodec.EncoderParams.t()
  def set_nvPreset(self, prop) do
    :evision_nif.cudacodec_EncoderParams_set_nvPreset(
        Evision.Internal.Structurise.from_struct(self),
        [nvPreset: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_rateControlMode(Evision.CUDACodec.EncoderParams.t()) :: Evision.CUDACodec.EncodeParamsRcMode.t()
  def get_rateControlMode(self) do
    :evision_nif.cudacodec_EncoderParams_get_rateControlMode(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_rateControlMode(Evision.CUDACodec.EncoderParams.t(), Evision.CUDACodec.EncodeParamsRcMode.t()) :: Evision.CUDACodec.EncoderParams.t()
  def set_rateControlMode(self, prop) do
    :evision_nif.cudacodec_EncoderParams_set_rateControlMode(
        Evision.Internal.Structurise.from_struct(self),
        [rateControlMode: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_targetQuality(Evision.CUDACodec.EncoderParams.t()) :: integer()
  def get_targetQuality(self) do
    :evision_nif.cudacodec_EncoderParams_get_targetQuality(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_targetQuality(Evision.CUDACodec.EncoderParams.t(), integer()) :: Evision.CUDACodec.EncoderParams.t()
  def set_targetQuality(self, prop) do
    :evision_nif.cudacodec_EncoderParams_set_targetQuality(
        Evision.Internal.Structurise.from_struct(self),
        [targetQuality: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_tuningInfo(Evision.CUDACodec.EncoderParams.t()) :: Evision.CUDACodec.EncodeTuningInfo.t()
  def get_tuningInfo(self) do
    :evision_nif.cudacodec_EncoderParams_get_tuningInfo(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_tuningInfo(Evision.CUDACodec.EncoderParams.t(), Evision.CUDACodec.EncodeTuningInfo.t()) :: Evision.CUDACodec.EncoderParams.t()
  def set_tuningInfo(self, prop) do
    :evision_nif.cudacodec_EncoderParams_set_tuningInfo(
        Evision.Internal.Structurise.from_struct(self),
        [tuningInfo: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
end
