defmodule Evision.RotateFlags do
  @type enum :: integer()
  @doc enum: true
  def cv_ROTATE_90_CLOCKWISE, do: 0
  @doc enum: true
  def cv_ROTATE_180, do: 1
  @doc enum: true
  def cv_ROTATE_90_COUNTERCLOCKWISE, do: 2
end
