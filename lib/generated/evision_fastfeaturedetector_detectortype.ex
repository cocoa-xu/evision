defmodule Evision.FastFeatureDetector.DetectorType do
  @type enum :: integer()
  @doc enum: true
  def cv_TYPE_5_8, do: 0
  @doc enum: true
  def cv_TYPE_7_12, do: 1
  @doc enum: true
  def cv_TYPE_9_16, do: 2
end
