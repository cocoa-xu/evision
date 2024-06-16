defmodule Evision.ImwriteTiffPredictorFlags do
  @type enum :: integer()
  @doc enum: true
  def cv_IMWRITE_TIFF_PREDICTOR_NONE, do: 1
  @doc enum: true
  def cv_IMWRITE_TIFF_PREDICTOR_HORIZONTAL, do: 2
  @doc enum: true
  def cv_IMWRITE_TIFF_PREDICTOR_FLOATINGPOINT, do: 3
end
