defmodule Evision.VideoCaptureOBSensorGenerators do
  import Bitwise

  @type enum :: integer()
  @doc enum: true
  def cv_CAP_OBSENSOR_DEPTH_GENERATOR, do: bsl(1, 29)
  @doc enum: true
  def cv_CAP_OBSENSOR_IMAGE_GENERATOR, do: bsl(1, 28)
  @doc enum: true
  def cv_CAP_OBSENSOR_IR_GENERATOR, do: bsl(1, 27)
  @doc enum: true
  def cv_CAP_OBSENSOR_GENERATORS_MASK, do: ((cv_CAP_OBSENSOR_DEPTH_GENERATOR() + cv_CAP_OBSENSOR_IMAGE_GENERATOR()) + cv_CAP_OBSENSOR_IR_GENERATOR())
end
