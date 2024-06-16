defmodule Evision.ML.ANN_MLP.TrainingMethods do
  @type enum :: integer()
  @doc enum: true
  def cv_BACKPROP, do: 0
  @doc enum: true
  def cv_RPROP, do: 1
  @doc enum: true
  def cv_ANNEAL, do: 2
end
