defmodule Evision.Detail.CvFeatureParams.FeatureType do
  @type enum :: integer()
  @doc enum: true
  def cv_HAAR, do: 0
  @doc enum: true
  def cv_LBP, do: 1
  @doc enum: true
  def cv_HOG, do: 2
end
