defmodule Evision.CUDACodec.EncodeParamsRcMode do
  @type enum :: integer()
  @doc enum: true
  def cv_ENC_PARAMS_RC_CONSTQP, do: 0
  @doc enum: true
  def cv_ENC_PARAMS_RC_VBR, do: 1
  @doc enum: true
  def cv_ENC_PARAMS_RC_CBR, do: 2
end
