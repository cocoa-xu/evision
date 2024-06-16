defmodule Evision.RectanglesIntersectTypes do
  @type enum :: integer()
  @doc enum: true
  def cv_INTERSECT_NONE, do: 0
  @doc enum: true
  def cv_INTERSECT_PARTIAL, do: 1
  @doc enum: true
  def cv_INTERSECT_FULL, do: 2
end
