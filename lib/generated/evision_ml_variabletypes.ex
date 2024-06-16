defmodule Evision.ML.VariableTypes do
  @type enum :: integer()
  @doc enum: true
  def cv_VAR_NUMERICAL, do: 0
  @doc enum: true
  def cv_VAR_ORDERED, do: 0
  @doc enum: true
  def cv_VAR_CATEGORICAL, do: 1
end
