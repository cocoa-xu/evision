defmodule Evision.ImwriteEXRCompressionFlags do
  @type enum :: integer()
  @doc enum: true
  def cv_IMWRITE_EXR_COMPRESSION_NO, do: 0
  @doc enum: true
  def cv_IMWRITE_EXR_COMPRESSION_RLE, do: 1
  @doc enum: true
  def cv_IMWRITE_EXR_COMPRESSION_ZIPS, do: 2
  @doc enum: true
  def cv_IMWRITE_EXR_COMPRESSION_ZIP, do: 3
  @doc enum: true
  def cv_IMWRITE_EXR_COMPRESSION_PIZ, do: 4
  @doc enum: true
  def cv_IMWRITE_EXR_COMPRESSION_PXR24, do: 5
  @doc enum: true
  def cv_IMWRITE_EXR_COMPRESSION_B44, do: 6
  @doc enum: true
  def cv_IMWRITE_EXR_COMPRESSION_B44A, do: 7
  @doc enum: true
  def cv_IMWRITE_EXR_COMPRESSION_DWAA, do: 8
  @doc enum: true
  def cv_IMWRITE_EXR_COMPRESSION_DWAB, do: 9
end
