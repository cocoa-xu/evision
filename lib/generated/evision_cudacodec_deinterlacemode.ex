defmodule Evision.CUDACodec.DeinterlaceMode do
  @type enum :: integer()
  @doc enum: true
  def cv_Weave, do: 0
  @doc enum: true
  def cv_Bob, do: 1
  @doc enum: true
  def cv_Adaptive, do: 2
end
