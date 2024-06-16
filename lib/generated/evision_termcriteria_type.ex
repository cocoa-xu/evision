defmodule Evision.TermCriteria.Type do
  @type enum :: integer()
  @doc enum: true
  def cv_COUNT, do: 1
  @doc enum: true
  def cv_MAX_ITER, do: cv_COUNT()
  @doc enum: true
  def cv_EPS, do: 2
end
