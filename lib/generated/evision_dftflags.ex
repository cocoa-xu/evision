defmodule Evision.DftFlags do
  @type enum :: integer()
  @doc enum: true
  def cv_DFT_INVERSE, do: 1
  @doc enum: true
  def cv_DFT_SCALE, do: 2
  @doc enum: true
  def cv_DFT_ROWS, do: 4
  @doc enum: true
  def cv_DFT_COMPLEX_OUTPUT, do: 16
  @doc enum: true
  def cv_DFT_REAL_OUTPUT, do: 32
  @doc enum: true
  def cv_DFT_COMPLEX_INPUT, do: 64
  @doc enum: true
  def cv_DCT_INVERSE, do: cv_DFT_INVERSE()
  @doc enum: true
  def cv_DCT_ROWS, do: cv_DFT_ROWS()
end
