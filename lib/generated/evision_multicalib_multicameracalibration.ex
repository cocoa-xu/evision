defmodule Evision.MultiCalib.MultiCameraCalibration do
  @type enum :: integer()
  @doc enum: true
  def cv_PINHOLE, do: 0
  @doc enum: true
  def cv_OMNIDIRECTIONAL, do: 1
end
