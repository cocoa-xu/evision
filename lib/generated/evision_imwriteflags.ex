defmodule Evision.ImwriteFlags do
  import Bitwise

  @type enum :: integer()
  @doc enum: true
  def cv_IMWRITE_JPEG_QUALITY, do: 1
  @doc enum: true
  def cv_IMWRITE_JPEG_PROGRESSIVE, do: 2
  @doc enum: true
  def cv_IMWRITE_JPEG_OPTIMIZE, do: 3
  @doc enum: true
  def cv_IMWRITE_JPEG_RST_INTERVAL, do: 4
  @doc enum: true
  def cv_IMWRITE_JPEG_LUMA_QUALITY, do: 5
  @doc enum: true
  def cv_IMWRITE_JPEG_CHROMA_QUALITY, do: 6
  @doc enum: true
  def cv_IMWRITE_JPEG_SAMPLING_FACTOR, do: 7
  @doc enum: true
  def cv_IMWRITE_PNG_COMPRESSION, do: 16
  @doc enum: true
  def cv_IMWRITE_PNG_STRATEGY, do: 17
  @doc enum: true
  def cv_IMWRITE_PNG_BILEVEL, do: 18
  @doc enum: true
  def cv_IMWRITE_PXM_BINARY, do: 32
  @doc enum: true
  def cv_IMWRITE_EXR_TYPE, do: (bsl(3, 4) + 0)
  @doc enum: true
  def cv_IMWRITE_EXR_COMPRESSION, do: (bsl(3, 4) + 1)
  @doc enum: true
  def cv_IMWRITE_EXR_DWA_COMPRESSION_LEVEL, do: (bsl(3, 4) + 2)
  @doc enum: true
  def cv_IMWRITE_WEBP_QUALITY, do: 64
  @doc enum: true
  def cv_IMWRITE_HDR_COMPRESSION, do: (bsl(5, 4) + 0)
  @doc enum: true
  def cv_IMWRITE_PAM_TUPLETYPE, do: 128
  @doc enum: true
  def cv_IMWRITE_TIFF_RESUNIT, do: 256
  @doc enum: true
  def cv_IMWRITE_TIFF_XDPI, do: 257
  @doc enum: true
  def cv_IMWRITE_TIFF_YDPI, do: 258
  @doc enum: true
  def cv_IMWRITE_TIFF_COMPRESSION, do: 259
  @doc enum: true
  def cv_IMWRITE_TIFF_ROWSPERSTRIP, do: 278
  @doc enum: true
  def cv_IMWRITE_TIFF_PREDICTOR, do: 317
  @doc enum: true
  def cv_IMWRITE_JPEG2000_COMPRESSION_X1000, do: 272
  @doc enum: true
  def cv_IMWRITE_AVIF_QUALITY, do: 512
  @doc enum: true
  def cv_IMWRITE_AVIF_DEPTH, do: 513
  @doc enum: true
  def cv_IMWRITE_AVIF_SPEED, do: 514
end
