defmodule Evision.HoughModes do
  @type enum :: integer()
  @doc enum: true
  def cv_HOUGH_STANDARD, do: 0
  @doc enum: true
  def cv_HOUGH_PROBABILISTIC, do: 1
  @doc enum: true
  def cv_HOUGH_MULTI_SCALE, do: 2
  @doc enum: true
  def cv_HOUGH_GRADIENT, do: 3
  @doc enum: true
  def cv_HOUGH_GRADIENT_ALT, do: 4
end
