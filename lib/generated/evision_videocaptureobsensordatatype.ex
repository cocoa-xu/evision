defmodule Evision.VideoCaptureOBSensorDataType do
  @type enum :: integer()
  @doc enum: true
  def cv_CAP_OBSENSOR_DEPTH_MAP, do: 0
  @doc enum: true
  def cv_CAP_OBSENSOR_BGR_IMAGE, do: 1
  @doc enum: true
  def cv_CAP_OBSENSOR_IR_IMAGE, do: 2
end
