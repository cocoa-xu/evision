defmodule Evision.XFeatures2D.DAISY.NormalizationType do
  @type enum :: integer()
  @doc enum: true
  def cv_NRM_NONE, do: 100
  @doc enum: true
  def cv_NRM_PARTIAL, do: 101
  @doc enum: true
  def cv_NRM_FULL, do: 102
  @doc enum: true
  def cv_NRM_SIFT, do: 103
end
