defmodule Evision.MouseEventFlags do
  @type enum :: integer()
  @doc enum: true
  def cv_EVENT_FLAG_LBUTTON, do: 1
  @doc enum: true
  def cv_EVENT_FLAG_RBUTTON, do: 2
  @doc enum: true
  def cv_EVENT_FLAG_MBUTTON, do: 4
  @doc enum: true
  def cv_EVENT_FLAG_CTRLKEY, do: 8
  @doc enum: true
  def cv_EVENT_FLAG_SHIFTKEY, do: 16
  @doc enum: true
  def cv_EVENT_FLAG_ALTKEY, do: 32
end
