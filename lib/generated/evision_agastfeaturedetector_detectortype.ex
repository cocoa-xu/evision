defmodule Evision.AgastFeatureDetector.DetectorType do
  @type enum :: integer()
  @doc enum: true
  def cv_AGAST_5_8, do: 0
  @doc enum: true
  def cv_AGAST_7_12d, do: 1
  @doc enum: true
  def cv_AGAST_7_12s, do: 2
  @doc enum: true
  def cv_OAST_9_16, do: 3
end
