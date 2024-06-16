defmodule Evision.XImgProc.EdgeDrawing.GradientOperator do
  @type enum :: integer()
  @doc enum: true
  def cv_PREWITT, do: 0
  @doc enum: true
  def cv_SOBEL, do: 1
  @doc enum: true
  def cv_SCHARR, do: 2
  @doc enum: true
  def cv_LSD, do: 3
end
