defmodule Evision.OGL.Texture2D.Format do
  @type enum :: integer()
  @doc enum: true
  def cv_NONE, do: 0
  @doc enum: true
  def cv_DEPTH_COMPONENT, do: 6402
  @doc enum: true
  def cv_RGB, do: 6407
  @doc enum: true
  def cv_RGBA, do: 6408
end
