defmodule Evision.QtFontStyles do
  @type enum :: integer()
  @doc enum: true
  def cv_QT_STYLE_NORMAL, do: 0
  @doc enum: true
  def cv_QT_STYLE_ITALIC, do: 1
  @doc enum: true
  def cv_QT_STYLE_OBLIQUE, do: 2
end
