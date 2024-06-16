defmodule Evision.CUDA.AlphaCompTypes do
  @type enum :: integer()
  @doc enum: true
  def cv_ALPHA_OVER, do: 0
  @doc enum: true
  def cv_ALPHA_IN, do: 1
  @doc enum: true
  def cv_ALPHA_OUT, do: 2
  @doc enum: true
  def cv_ALPHA_ATOP, do: 3
  @doc enum: true
  def cv_ALPHA_XOR, do: 4
  @doc enum: true
  def cv_ALPHA_PLUS, do: 5
  @doc enum: true
  def cv_ALPHA_OVER_PREMUL, do: 6
  @doc enum: true
  def cv_ALPHA_IN_PREMUL, do: 7
  @doc enum: true
  def cv_ALPHA_OUT_PREMUL, do: 8
  @doc enum: true
  def cv_ALPHA_ATOP_PREMUL, do: 9
  @doc enum: true
  def cv_ALPHA_XOR_PREMUL, do: 10
  @doc enum: true
  def cv_ALPHA_PLUS_PREMUL, do: 11
  @doc enum: true
  def cv_ALPHA_PREMUL, do: 12
end
