defmodule Evision.ImwriteHDRCompressionFlags do
  @type enum :: integer()
  @doc enum: true
  def cv_IMWRITE_HDR_COMPRESSION_NONE, do: 0
  @doc enum: true
  def cv_IMWRITE_HDR_COMPRESSION_RLE, do: 1
end
