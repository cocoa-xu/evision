defmodule Evision.FaceRecognizerSF.DisType do
  @type enum :: integer()
  @doc enum: true
  def cv_FR_COSINE, do: 0
  @doc enum: true
  def cv_FR_NORM_L2, do: 1
end
