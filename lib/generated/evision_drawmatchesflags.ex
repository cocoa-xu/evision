defmodule Evision.DrawMatchesFlags do
  @type enum :: integer()
  @doc enum: true
  def cv_DEFAULT, do: 0
  @doc enum: true
  def cv_DRAW_OVER_OUTIMG, do: 1
  @doc enum: true
  def cv_NOT_DRAW_SINGLE_POINTS, do: 2
  @doc enum: true
  def cv_DRAW_RICH_KEYPOINTS, do: 4
end
