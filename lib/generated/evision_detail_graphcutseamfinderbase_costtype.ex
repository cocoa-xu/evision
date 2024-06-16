defmodule Evision.Detail.GraphCutSeamFinderBase.CostType do
  @type enum :: integer()
  @doc enum: true
  def cv_COST_COLOR, do: 0
  @doc enum: true
  def cv_COST_COLOR_GRAD, do: 1
end
