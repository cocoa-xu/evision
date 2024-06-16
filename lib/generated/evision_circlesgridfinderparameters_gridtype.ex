defmodule Evision.CirclesGridFinderParameters.GridType do
  @type enum :: integer()
  @doc enum: true
  def cv_SYMMETRIC_GRID, do: 0
  @doc enum: true
  def cv_ASYMMETRIC_GRID, do: 1
end
