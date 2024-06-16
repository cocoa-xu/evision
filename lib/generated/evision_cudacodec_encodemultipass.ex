defmodule Evision.CUDACodec.EncodeMultiPass do
  @type enum :: integer()
  @doc enum: true
  def cv_ENC_MULTI_PASS_DISABLED, do: 0
  @doc enum: true
  def cv_ENC_TWO_PASS_QUARTER_RESOLUTION, do: 1
  @doc enum: true
  def cv_ENC_TWO_PASS_FULL_RESOLUTION, do: 2
end
