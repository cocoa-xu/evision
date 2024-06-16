defmodule Evision.BgSegm.LSBPCameraMotionCompensation do
  @type enum :: integer()
  @doc enum: true
  def cv_LSBP_CAMERA_MOTION_COMPENSATION_NONE, do: 0
  @doc enum: true
  def cv_LSBP_CAMERA_MOTION_COMPENSATION_LK, do: (0 + 1)
end
