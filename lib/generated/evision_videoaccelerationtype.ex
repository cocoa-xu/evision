defmodule Evision.VideoAccelerationType do
  @type enum :: integer()
  @doc enum: true
  def cv_VIDEO_ACCELERATION_NONE, do: 0
  @doc enum: true
  def cv_VIDEO_ACCELERATION_ANY, do: 1
  @doc enum: true
  def cv_VIDEO_ACCELERATION_D3D11, do: 2
  @doc enum: true
  def cv_VIDEO_ACCELERATION_VAAPI, do: 3
  @doc enum: true
  def cv_VIDEO_ACCELERATION_MFX, do: 4
end
