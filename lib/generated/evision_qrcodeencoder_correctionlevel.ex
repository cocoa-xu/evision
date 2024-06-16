defmodule Evision.QRCodeEncoder.CorrectionLevel do
  @type enum :: integer()
  @doc enum: true
  def cv_CORRECT_LEVEL_L, do: 0
  @doc enum: true
  def cv_CORRECT_LEVEL_M, do: 1
  @doc enum: true
  def cv_CORRECT_LEVEL_Q, do: 2
  @doc enum: true
  def cv_CORRECT_LEVEL_H, do: 3
end
