defmodule Evision.QRCodeEncoder.EncodeMode do
  @type enum :: integer()
  @doc enum: true
  def cv_MODE_AUTO, do: -1
  @doc enum: true
  def cv_MODE_NUMERIC, do: 1
  @doc enum: true
  def cv_MODE_ALPHANUMERIC, do: 2
  @doc enum: true
  def cv_MODE_BYTE, do: 4
  @doc enum: true
  def cv_MODE_ECI, do: 7
  @doc enum: true
  def cv_MODE_KANJI, do: 8
  @doc enum: true
  def cv_MODE_STRUCTURED_APPEND, do: 3
end
