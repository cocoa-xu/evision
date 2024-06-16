defmodule Evision.WindowPropertyFlags do
  @type enum :: integer()
  @doc enum: true
  def cv_WND_PROP_FULLSCREEN, do: 0
  @doc enum: true
  def cv_WND_PROP_AUTOSIZE, do: 1
  @doc enum: true
  def cv_WND_PROP_ASPECT_RATIO, do: 2
  @doc enum: true
  def cv_WND_PROP_OPENGL, do: 3
  @doc enum: true
  def cv_WND_PROP_VISIBLE, do: 4
  @doc enum: true
  def cv_WND_PROP_TOPMOST, do: 5
  @doc enum: true
  def cv_WND_PROP_VSYNC, do: 6
end
