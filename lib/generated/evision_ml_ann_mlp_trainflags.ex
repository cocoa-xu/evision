defmodule Evision.ML.ANN_MLP.TrainFlags do
  @type enum :: integer()
  @doc enum: true
  def cv_UPDATE_WEIGHTS, do: 1
  @doc enum: true
  def cv_NO_INPUT_SCALE, do: 2
  @doc enum: true
  def cv_NO_OUTPUT_SCALE, do: 4
end
