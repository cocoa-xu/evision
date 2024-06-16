defmodule Evision.CUDACodec.Codec do
  import Bitwise

  @type enum :: integer()
  @doc enum: true
  def cv_MPEG1, do: 0
  @doc enum: true
  def cv_MPEG2, do: (0 + 1)
  @doc enum: true
  def cv_MPEG4, do: (0 + 2)
  @doc enum: true
  def cv_VC1, do: (0 + 3)
  @doc enum: true
  def cv_H264, do: (0 + 4)
  @doc enum: true
  def cv_JPEG, do: (0 + 5)
  @doc enum: true
  def cv_H264_SVC, do: (0 + 6)
  @doc enum: true
  def cv_H264_MVC, do: (0 + 7)
  @doc enum: true
  def cv_HEVC, do: (0 + 8)
  @doc enum: true
  def cv_VP8, do: (0 + 9)
  @doc enum: true
  def cv_VP9, do: (0 + 10)
  @doc enum: true
  def cv_AV1, do: (0 + 11)
  @doc enum: true
  def cv_NumCodecs, do: (0 + 12)
  @doc enum: true
  def cv_Uncompressed_YUV420, do: bor(bor(bor(bsl(73, 24), bsl(89, 16)), bsl(85, 8)), 86)
  @doc enum: true
  def cv_Uncompressed_YV12, do: bor(bor(bor(bsl(89, 24), bsl(86, 16)), bsl(1, 8)), 2)
  @doc enum: true
  def cv_Uncompressed_NV12, do: bor(bor(bor(bsl(78, 24), bsl(86, 16)), bsl(1, 8)), 2)
  @doc enum: true
  def cv_Uncompressed_YUYV, do: bor(bor(bor(bsl(89, 24), bsl(85, 16)), bsl(89, 8)), 86)
  @doc enum: true
  def cv_Uncompressed_UYVY, do: bor(bor(bor(bsl(85, 24), bsl(89, 16)), bsl(86, 8)), 89)
end
