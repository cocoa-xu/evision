defmodule Evision.DistanceTypes do
  @type enum :: integer()
  @doc enum: true
  def cv_DIST_USER, do: -1
  @doc enum: true
  def cv_DIST_L1, do: 1
  @doc enum: true
  def cv_DIST_L2, do: 2
  @doc enum: true
  def cv_DIST_C, do: 3
  @doc enum: true
  def cv_DIST_L12, do: 4
  @doc enum: true
  def cv_DIST_FAIR, do: 5
  @doc enum: true
  def cv_DIST_WELSCH, do: 6
  @doc enum: true
  def cv_DIST_HUBER, do: 7
end
