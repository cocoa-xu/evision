defmodule Evision.ReduceTypes do
  @type enum :: integer()
  @doc enum: true
  def cv_REDUCE_SUM, do: 0
  @doc enum: true
  def cv_REDUCE_AVG, do: 1
  @doc enum: true
  def cv_REDUCE_MAX, do: 2
  @doc enum: true
  def cv_REDUCE_MIN, do: 3
  @doc enum: true
  def cv_REDUCE_SUM2, do: 4
end
