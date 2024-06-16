defmodule Evision.ArUco.CornerRefineMethod do
  @type enum :: integer()
  @doc enum: true
  def cv_CORNER_REFINE_NONE, do: 0
  @doc enum: true
  def cv_CORNER_REFINE_SUBPIX, do: 1
  @doc enum: true
  def cv_CORNER_REFINE_CONTOUR, do: 2
  @doc enum: true
  def cv_CORNER_REFINE_APRILTAG, do: 3
end
