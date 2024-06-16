defmodule Evision.ImwritePAMFlags do
  @type enum :: integer()
  @doc enum: true
  def cv_IMWRITE_PAM_FORMAT_NULL, do: 0
  @doc enum: true
  def cv_IMWRITE_PAM_FORMAT_BLACKANDWHITE, do: 1
  @doc enum: true
  def cv_IMWRITE_PAM_FORMAT_GRAYSCALE, do: 2
  @doc enum: true
  def cv_IMWRITE_PAM_FORMAT_GRAYSCALE_ALPHA, do: 3
  @doc enum: true
  def cv_IMWRITE_PAM_FORMAT_RGB, do: 4
  @doc enum: true
  def cv_IMWRITE_PAM_FORMAT_RGB_ALPHA, do: 5
end
