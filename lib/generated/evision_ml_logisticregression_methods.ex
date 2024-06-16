defmodule Evision.ML.LogisticRegression.Methods do
  @type enum :: integer()
  @doc enum: true
  def cv_BATCH, do: 0
  @doc enum: true
  def cv_MINI_BATCH, do: 1
end
