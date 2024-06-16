defmodule Evision.XFeatures2D.PCTSignatures.DistanceFunction do
  @type enum :: integer()
  @doc enum: true
  def cv_L0_25, do: 0
  @doc enum: true
  def cv_L0_5, do: 1
  @doc enum: true
  def cv_L1, do: 2
  @doc enum: true
  def cv_L2, do: 3
  @doc enum: true
  def cv_L2SQUARED, do: 4
  @doc enum: true
  def cv_L5, do: 5
  @doc enum: true
  def cv_L_INFINITY, do: 6
end
