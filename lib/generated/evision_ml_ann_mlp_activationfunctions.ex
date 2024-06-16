defmodule Evision.ML.ANN_MLP.ActivationFunctions do
  @type enum :: integer()
  @doc enum: true
  def cv_IDENTITY, do: 0
  @doc enum: true
  def cv_SIGMOID_SYM, do: 1
  @doc enum: true
  def cv_GAUSSIAN, do: 2
  @doc enum: true
  def cv_RELU, do: 3
  @doc enum: true
  def cv_LEAKYRELU, do: 4
end
