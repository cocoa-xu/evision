defmodule Evision.CUDACodec.EncodeTuningInfo do
  @type enum :: integer()
  @doc enum: true
  def cv_ENC_TUNING_INFO_UNDEFINED, do: 0
  @doc enum: true
  def cv_ENC_TUNING_INFO_HIGH_QUALITY, do: 1
  @doc enum: true
  def cv_ENC_TUNING_INFO_LOW_LATENCY, do: 2
  @doc enum: true
  def cv_ENC_TUNING_INFO_ULTRA_LOW_LATENCY, do: 3
  @doc enum: true
  def cv_ENC_TUNING_INFO_LOSSLESS, do: 4
  @doc enum: true
  def cv_ENC_TUNING_INFO_COUNT, do: (4 + 1)
end
