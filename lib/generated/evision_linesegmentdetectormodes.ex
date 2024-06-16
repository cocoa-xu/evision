defmodule Evision.LineSegmentDetectorModes do
  @type enum :: integer()
  @doc enum: true
  def cv_LSD_REFINE_NONE, do: 0
  @doc enum: true
  def cv_LSD_REFINE_STD, do: 1
  @doc enum: true
  def cv_LSD_REFINE_ADV, do: 2
end
