defmodule Evision.InterpolationFlags do
  @type enum :: integer()
  @doc enum: true
  def cv_INTER_NEAREST, do: 0
  @doc enum: true
  def cv_INTER_LINEAR, do: 1
  @doc enum: true
  def cv_INTER_CUBIC, do: 2
  @doc enum: true
  def cv_INTER_AREA, do: 3
  @doc enum: true
  def cv_INTER_LANCZOS4, do: 4
  @doc enum: true
  def cv_INTER_LINEAR_EXACT, do: 5
  @doc enum: true
  def cv_INTER_NEAREST_EXACT, do: 6
  @doc enum: true
  def cv_INTER_MAX, do: 7
  @doc enum: true
  def cv_WARP_FILL_OUTLIERS, do: 8
  @doc enum: true
  def cv_WARP_INVERSE_MAP, do: 16
  @doc enum: true
  def cv_WARP_RELATIVE_MAP, do: 32
end
