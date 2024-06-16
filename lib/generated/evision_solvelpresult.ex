defmodule Evision.SolveLPResult do
  @type enum :: integer()
  @doc enum: true
  def cv_SOLVELP_LOST, do: -3
  @doc enum: true
  def cv_SOLVELP_UNBOUNDED, do: -2
  @doc enum: true
  def cv_SOLVELP_UNFEASIBLE, do: -1
  @doc enum: true
  def cv_SOLVELP_SINGLE, do: 0
  @doc enum: true
  def cv_SOLVELP_MULTI, do: 1
end
