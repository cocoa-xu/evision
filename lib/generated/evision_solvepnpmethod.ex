defmodule Evision.SolvePnPMethod do
  @type enum :: integer()
  @doc enum: true
  def cv_SOLVEPNP_ITERATIVE, do: 0
  @doc enum: true
  def cv_SOLVEPNP_EPNP, do: 1
  @doc enum: true
  def cv_SOLVEPNP_P3P, do: 2
  @doc enum: true
  def cv_SOLVEPNP_DLS, do: 3
  @doc enum: true
  def cv_SOLVEPNP_UPNP, do: 4
  @doc enum: true
  def cv_SOLVEPNP_AP3P, do: 5
  @doc enum: true
  def cv_SOLVEPNP_IPPE, do: 6
  @doc enum: true
  def cv_SOLVEPNP_IPPE_SQUARE, do: 7
  @doc enum: true
  def cv_SOLVEPNP_SQPNP, do: 8
  @doc enum: true
  def cv_SOLVEPNP_MAX_COUNT, do: (8 + 1)
end
