defmodule Evision.NormTypes do
  @type enum :: integer()
  @doc enum: true
  def cv_NORM_INF, do: 1
  @doc enum: true
  def cv_NORM_L1, do: 2
  @doc enum: true
  def cv_NORM_L2, do: 4
  @doc enum: true
  def cv_NORM_L2SQR, do: 5
  @doc enum: true
  def cv_NORM_HAMMING, do: 6
  @doc enum: true
  def cv_NORM_HAMMING2, do: 7
  @doc enum: true
  def cv_NORM_TYPE_MASK, do: 7
  @doc enum: true
  def cv_NORM_RELATIVE, do: 8
  @doc enum: true
  def cv_NORM_MINMAX, do: 32
end
