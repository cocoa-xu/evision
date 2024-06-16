defmodule Evision.KAZE.DiffusivityType do
  @type enum :: integer()
  @doc enum: true
  def cv_DIFF_PM_G1, do: 0
  @doc enum: true
  def cv_DIFF_PM_G2, do: 1
  @doc enum: true
  def cv_DIFF_WEICKERT, do: 2
  @doc enum: true
  def cv_DIFF_CHARBONNIER, do: 3
end
