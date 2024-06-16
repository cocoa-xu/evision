defmodule Evision.CUDACodec.EncodeProfile do
  @type enum :: integer()
  @doc enum: true
  def cv_ENC_CODEC_PROFILE_AUTOSELECT, do: 0
  @doc enum: true
  def cv_ENC_H264_PROFILE_BASELINE, do: 1
  @doc enum: true
  def cv_ENC_H264_PROFILE_MAIN, do: 2
  @doc enum: true
  def cv_ENC_H264_PROFILE_HIGH, do: 3
  @doc enum: true
  def cv_ENC_H264_PROFILE_HIGH_444, do: 4
  @doc enum: true
  def cv_ENC_H264_PROFILE_STEREO, do: 5
  @doc enum: true
  def cv_ENC_H264_PROFILE_PROGRESSIVE_HIGH, do: 6
  @doc enum: true
  def cv_ENC_H264_PROFILE_CONSTRAINED_HIGH, do: 7
  @doc enum: true
  def cv_ENC_HEVC_PROFILE_MAIN, do: 8
  @doc enum: true
  def cv_ENC_HEVC_PROFILE_MAIN10, do: 9
  @doc enum: true
  def cv_ENC_HEVC_PROFILE_FREXT, do: 10
end
