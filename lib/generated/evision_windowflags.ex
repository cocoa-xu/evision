defmodule Evision.WindowFlags do
  @type enum :: integer()
  @doc enum: true
  def cv_WINDOW_NORMAL, do: 0
  @doc enum: true
  def cv_WINDOW_AUTOSIZE, do: 1
  @doc enum: true
  def cv_WINDOW_OPENGL, do: 4096
  @doc enum: true
  def cv_WINDOW_FULLSCREEN, do: 1
  @doc enum: true
  def cv_WINDOW_FREERATIO, do: 256
  @doc enum: true
  def cv_WINDOW_KEEPRATIO, do: 0
  @doc enum: true
  def cv_WINDOW_GUI_EXPANDED, do: 0
  @doc enum: true
  def cv_WINDOW_GUI_NORMAL, do: 16
end
