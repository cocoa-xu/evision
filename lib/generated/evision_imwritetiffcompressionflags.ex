defmodule Evision.ImwriteTiffCompressionFlags do
  @type enum :: integer()
  @doc enum: true
  def cv_IMWRITE_TIFF_COMPRESSION_NONE, do: 1
  @doc enum: true
  def cv_IMWRITE_TIFF_COMPRESSION_CCITTRLE, do: 2
  @doc enum: true
  def cv_IMWRITE_TIFF_COMPRESSION_CCITTFAX3, do: 3
  @doc enum: true
  def cv_IMWRITE_TIFF_COMPRESSION_CCITT_T4, do: 3
  @doc enum: true
  def cv_IMWRITE_TIFF_COMPRESSION_CCITTFAX4, do: 4
  @doc enum: true
  def cv_IMWRITE_TIFF_COMPRESSION_CCITT_T6, do: 4
  @doc enum: true
  def cv_IMWRITE_TIFF_COMPRESSION_LZW, do: 5
  @doc enum: true
  def cv_IMWRITE_TIFF_COMPRESSION_OJPEG, do: 6
  @doc enum: true
  def cv_IMWRITE_TIFF_COMPRESSION_JPEG, do: 7
  @doc enum: true
  def cv_IMWRITE_TIFF_COMPRESSION_T85, do: 9
  @doc enum: true
  def cv_IMWRITE_TIFF_COMPRESSION_T43, do: 10
  @doc enum: true
  def cv_IMWRITE_TIFF_COMPRESSION_NEXT, do: 32766
  @doc enum: true
  def cv_IMWRITE_TIFF_COMPRESSION_CCITTRLEW, do: 32771
  @doc enum: true
  def cv_IMWRITE_TIFF_COMPRESSION_PACKBITS, do: 32773
  @doc enum: true
  def cv_IMWRITE_TIFF_COMPRESSION_THUNDERSCAN, do: 32809
  @doc enum: true
  def cv_IMWRITE_TIFF_COMPRESSION_IT8CTPAD, do: 32895
  @doc enum: true
  def cv_IMWRITE_TIFF_COMPRESSION_IT8LW, do: 32896
  @doc enum: true
  def cv_IMWRITE_TIFF_COMPRESSION_IT8MP, do: 32897
  @doc enum: true
  def cv_IMWRITE_TIFF_COMPRESSION_IT8BL, do: 32898
  @doc enum: true
  def cv_IMWRITE_TIFF_COMPRESSION_PIXARFILM, do: 32908
  @doc enum: true
  def cv_IMWRITE_TIFF_COMPRESSION_PIXARLOG, do: 32909
  @doc enum: true
  def cv_IMWRITE_TIFF_COMPRESSION_DEFLATE, do: 32946
  @doc enum: true
  def cv_IMWRITE_TIFF_COMPRESSION_ADOBE_DEFLATE, do: 8
  @doc enum: true
  def cv_IMWRITE_TIFF_COMPRESSION_DCS, do: 32947
  @doc enum: true
  def cv_IMWRITE_TIFF_COMPRESSION_JBIG, do: 34661
  @doc enum: true
  def cv_IMWRITE_TIFF_COMPRESSION_SGILOG, do: 34676
  @doc enum: true
  def cv_IMWRITE_TIFF_COMPRESSION_SGILOG24, do: 34677
  @doc enum: true
  def cv_IMWRITE_TIFF_COMPRESSION_JP2000, do: 34712
  @doc enum: true
  def cv_IMWRITE_TIFF_COMPRESSION_LERC, do: 34887
  @doc enum: true
  def cv_IMWRITE_TIFF_COMPRESSION_LZMA, do: 34925
  @doc enum: true
  def cv_IMWRITE_TIFF_COMPRESSION_ZSTD, do: 50000
  @doc enum: true
  def cv_IMWRITE_TIFF_COMPRESSION_WEBP, do: 50001
  @doc enum: true
  def cv_IMWRITE_TIFF_COMPRESSION_JXL, do: 50002
end
