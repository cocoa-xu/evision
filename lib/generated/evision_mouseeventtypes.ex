defmodule Evision.MouseEventTypes do
  @type enum :: integer()
  @doc enum: true
  def cv_EVENT_MOUSEMOVE, do: 0
  @doc enum: true
  def cv_EVENT_LBUTTONDOWN, do: 1
  @doc enum: true
  def cv_EVENT_RBUTTONDOWN, do: 2
  @doc enum: true
  def cv_EVENT_MBUTTONDOWN, do: 3
  @doc enum: true
  def cv_EVENT_LBUTTONUP, do: 4
  @doc enum: true
  def cv_EVENT_RBUTTONUP, do: 5
  @doc enum: true
  def cv_EVENT_MBUTTONUP, do: 6
  @doc enum: true
  def cv_EVENT_LBUTTONDBLCLK, do: 7
  @doc enum: true
  def cv_EVENT_RBUTTONDBLCLK, do: 8
  @doc enum: true
  def cv_EVENT_MBUTTONDBLCLK, do: 9
  @doc enum: true
  def cv_EVENT_MOUSEWHEEL, do: 10
  @doc enum: true
  def cv_EVENT_MOUSEHWHEEL, do: 11
end
