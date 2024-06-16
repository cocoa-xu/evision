defmodule Evision.MarkerTypes do
  @type enum :: integer()
  @doc enum: true
  def cv_MARKER_CROSS, do: 0
  @doc enum: true
  def cv_MARKER_TILTED_CROSS, do: 1
  @doc enum: true
  def cv_MARKER_STAR, do: 2
  @doc enum: true
  def cv_MARKER_DIAMOND, do: 3
  @doc enum: true
  def cv_MARKER_SQUARE, do: 4
  @doc enum: true
  def cv_MARKER_TRIANGLE_UP, do: 5
  @doc enum: true
  def cv_MARKER_TRIANGLE_DOWN, do: 6
end
