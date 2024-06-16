defmodule Evision.ML.Boost.Types do
  @type enum :: integer()
  @doc enum: true
  def cv_DISCRETE, do: 0
  @doc enum: true
  def cv_REAL, do: 1
  @doc enum: true
  def cv_LOGIT, do: 2
  @doc enum: true
  def cv_GENTLE, do: 3
end
