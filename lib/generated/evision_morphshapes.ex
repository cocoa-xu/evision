defmodule Evision.MorphShapes do
  @type enum :: integer()
  @doc enum: true
  def cv_MORPH_RECT, do: 0
  @doc enum: true
  def cv_MORPH_CROSS, do: 1
  @doc enum: true
  def cv_MORPH_ELLIPSE, do: 2
end
