defmodule Evision.CUDACodec.ChromaFormat do
  @type enum :: integer()
  @doc enum: true
  def cv_Monochrome, do: 0
  @doc enum: true
  def cv_YUV420, do: (0 + 1)
  @doc enum: true
  def cv_YUV422, do: (0 + 2)
  @doc enum: true
  def cv_YUV444, do: (0 + 3)
  @doc enum: true
  def cv_NumFormats, do: (0 + 4)
end
