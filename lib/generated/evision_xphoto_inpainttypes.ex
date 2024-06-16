defmodule Evision.XPhoto.InpaintTypes do
  @type enum :: integer()
  @doc enum: true
  def cv_INPAINT_SHIFTMAP, do: 0
  @doc enum: true
  def cv_INPAINT_FSR_BEST, do: 1
  @doc enum: true
  def cv_INPAINT_FSR_FAST, do: 2
end
