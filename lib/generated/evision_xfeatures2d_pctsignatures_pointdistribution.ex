defmodule Evision.XFeatures2D.PCTSignatures.PointDistribution do
  @type enum :: integer()
  @doc enum: true
  def cv_UNIFORM, do: 0
  @doc enum: true
  def cv_REGULAR, do: 1
  @doc enum: true
  def cv_NORMAL, do: 2
end
