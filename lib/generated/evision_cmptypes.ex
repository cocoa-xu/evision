defmodule Evision.CmpTypes do
  @type enum :: integer()
  @doc enum: true
  def cv_CMP_EQ, do: 0
  @doc enum: true
  def cv_CMP_GT, do: 1
  @doc enum: true
  def cv_CMP_GE, do: 2
  @doc enum: true
  def cv_CMP_LT, do: 3
  @doc enum: true
  def cv_CMP_LE, do: 4
  @doc enum: true
  def cv_CMP_NE, do: 5
end
