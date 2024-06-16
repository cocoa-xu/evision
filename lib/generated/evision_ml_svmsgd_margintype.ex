defmodule Evision.ML.SVMSGD.MarginType do
  @type enum :: integer()
  @doc enum: true
  def cv_SOFT_MARGIN, do: 0
  @doc enum: true
  def cv_HARD_MARGIN, do: 1
end
