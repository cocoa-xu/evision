defmodule Evision.ImreadModes do
  @type enum :: integer()
  @doc enum: true
  def cv_IMREAD_UNCHANGED, do: -1
  @doc enum: true
  def cv_IMREAD_GRAYSCALE, do: 0
  @doc enum: true
  def cv_IMREAD_COLOR, do: 1
  @doc enum: true
  def cv_IMREAD_ANYDEPTH, do: 2
  @doc enum: true
  def cv_IMREAD_ANYCOLOR, do: 4
  @doc enum: true
  def cv_IMREAD_LOAD_GDAL, do: 8
  @doc enum: true
  def cv_IMREAD_REDUCED_GRAYSCALE_2, do: 16
  @doc enum: true
  def cv_IMREAD_REDUCED_COLOR_2, do: 17
  @doc enum: true
  def cv_IMREAD_REDUCED_GRAYSCALE_4, do: 32
  @doc enum: true
  def cv_IMREAD_REDUCED_COLOR_4, do: 33
  @doc enum: true
  def cv_IMREAD_REDUCED_GRAYSCALE_8, do: 64
  @doc enum: true
  def cv_IMREAD_REDUCED_COLOR_8, do: 65
  @doc enum: true
  def cv_IMREAD_IGNORE_ORIENTATION, do: 128
end
