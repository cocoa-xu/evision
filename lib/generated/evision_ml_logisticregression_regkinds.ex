defmodule Evision.ML.LogisticRegression.RegKinds do
  @type enum :: integer()
  @doc enum: true
  def cv_REG_DISABLE, do: -1
  @doc enum: true
  def cv_REG_L1, do: 0
  @doc enum: true
  def cv_REG_L2, do: 1
end
