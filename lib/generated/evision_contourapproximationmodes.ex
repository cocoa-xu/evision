defmodule Evision.ContourApproximationModes do
  @type enum :: integer()
  @doc enum: true
  def cv_CHAIN_APPROX_NONE, do: 1
  @doc enum: true
  def cv_CHAIN_APPROX_SIMPLE, do: 2
  @doc enum: true
  def cv_CHAIN_APPROX_TC89_L1, do: 3
  @doc enum: true
  def cv_CHAIN_APPROX_TC89_KCOS, do: 4
end
