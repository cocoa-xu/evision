defmodule Evision.OGL.RenderModes do
  @type enum :: integer()
  @doc enum: true
  def cv_POINTS, do: 0
  @doc enum: true
  def cv_LINES, do: 1
  @doc enum: true
  def cv_LINE_LOOP, do: 2
  @doc enum: true
  def cv_LINE_STRIP, do: 3
  @doc enum: true
  def cv_TRIANGLES, do: 4
  @doc enum: true
  def cv_TRIANGLE_STRIP, do: 5
  @doc enum: true
  def cv_TRIANGLE_FAN, do: 6
  @doc enum: true
  def cv_QUADS, do: 7
  @doc enum: true
  def cv_QUAD_STRIP, do: 8
  @doc enum: true
  def cv_POLYGON, do: 9
end
