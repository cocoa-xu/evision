defmodule Evision.HandEyeCalibrationMethod do
  @type enum :: integer()
  @doc enum: true
  def cv_CALIB_HAND_EYE_TSAI, do: 0
  @doc enum: true
  def cv_CALIB_HAND_EYE_PARK, do: 1
  @doc enum: true
  def cv_CALIB_HAND_EYE_HORAUD, do: 2
  @doc enum: true
  def cv_CALIB_HAND_EYE_ANDREFF, do: 3
  @doc enum: true
  def cv_CALIB_HAND_EYE_DANIILIDIS, do: 4
end
