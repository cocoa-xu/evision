defmodule Evision.ConnectedComponentsTypes do
  @type enum :: integer()
  @doc enum: true
  def cv_CC_STAT_LEFT, do: 0
  @doc enum: true
  def cv_CC_STAT_TOP, do: 1
  @doc enum: true
  def cv_CC_STAT_WIDTH, do: 2
  @doc enum: true
  def cv_CC_STAT_HEIGHT, do: 3
  @doc enum: true
  def cv_CC_STAT_AREA, do: 4
  @doc enum: true
  def cv_CC_STAT_MAX, do: 5
end
