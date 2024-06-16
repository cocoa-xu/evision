defmodule Evision.CUDACodec.ColorFormat do
  @type enum :: integer()
  @doc enum: true
  def cv_UNDEFINED, do: 0
  @doc enum: true
  def cv_BGRA, do: 1
  @doc enum: true
  def cv_BGR, do: 2
  @doc enum: true
  def cv_GRAY, do: 3
  @doc enum: true
  def cv_NV_NV12, do: 4
  @doc enum: true
  def cv_RGB, do: 5
  @doc enum: true
  def cv_RGBA, do: 6
  @doc enum: true
  def cv_NV_YV12, do: 8
  @doc enum: true
  def cv_NV_IYUV, do: 9
  @doc enum: true
  def cv_NV_YUV444, do: 10
  @doc enum: true
  def cv_NV_AYUV, do: 11
  @doc enum: true
  def cv_PROP_NOT_SUPPORTED, do: (11 + 1)
end
