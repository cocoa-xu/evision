defmodule Evision.ArUco.PatternPositionType do
  @type enum :: integer()
  @doc enum: true
  def cv_ARUCO_CCW_CENTER, do: 0
  @doc enum: true
  def cv_ARUCO_CW_TOP_LEFT_CORNER, do: 1
end
