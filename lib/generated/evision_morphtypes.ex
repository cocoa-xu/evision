defmodule Evision.MorphTypes do
  @type enum :: integer()
  @doc enum: true
  def cv_MORPH_ERODE, do: 0
  @doc enum: true
  def cv_MORPH_DILATE, do: 1
  @doc enum: true
  def cv_MORPH_OPEN, do: 2
  @doc enum: true
  def cv_MORPH_CLOSE, do: 3
  @doc enum: true
  def cv_MORPH_GRADIENT, do: 4
  @doc enum: true
  def cv_MORPH_TOPHAT, do: 5
  @doc enum: true
  def cv_MORPH_BLACKHAT, do: 6
  @doc enum: true
  def cv_MORPH_HITMISS, do: 7
end
