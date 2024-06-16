defmodule Evision.VideoWriterProperties do
  @type enum :: integer()
  @doc enum: true
  def cv_VIDEOWRITER_PROP_QUALITY, do: 1
  @doc enum: true
  def cv_VIDEOWRITER_PROP_FRAMEBYTES, do: 2
  @doc enum: true
  def cv_VIDEOWRITER_PROP_NSTRIPES, do: 3
  @doc enum: true
  def cv_VIDEOWRITER_PROP_IS_COLOR, do: 4
  @doc enum: true
  def cv_VIDEOWRITER_PROP_DEPTH, do: 5
  @doc enum: true
  def cv_VIDEOWRITER_PROP_HW_ACCELERATION, do: 6
  @doc enum: true
  def cv_VIDEOWRITER_PROP_HW_DEVICE, do: 7
  @doc enum: true
  def cv_VIDEOWRITER_PROP_HW_ACCELERATION_USE_OPENCL, do: 8
  @doc enum: true
  def cv_VIDEOWRITER_PROP_RAW_VIDEO, do: 9
  @doc enum: true
  def cv_VIDEOWRITER_PROP_KEY_INTERVAL, do: 10
  @doc enum: true
  def cv_VIDEOWRITER_PROP_KEY_FLAG, do: 11
end
