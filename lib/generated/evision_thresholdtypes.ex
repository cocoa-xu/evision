defmodule Evision.ThresholdTypes do
  @type enum :: integer()
  @doc enum: true
  def cv_THRESH_BINARY, do: 0
  @doc enum: true
  def cv_THRESH_BINARY_INV, do: 1
  @doc enum: true
  def cv_THRESH_TRUNC, do: 2
  @doc enum: true
  def cv_THRESH_TOZERO, do: 3
  @doc enum: true
  def cv_THRESH_TOZERO_INV, do: 4
  @doc enum: true
  def cv_THRESH_MASK, do: 7
  @doc enum: true
  def cv_THRESH_OTSU, do: 8
  @doc enum: true
  def cv_THRESH_TRIANGLE, do: 16
end
