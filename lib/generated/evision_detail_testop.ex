defmodule Evision.Detail.TestOp do
  @type enum :: integer()
  @doc enum: true
  def cv_TEST_CUSTOM, do: 0
  @doc enum: true
  def cv_TEST_EQ, do: 1
  @doc enum: true
  def cv_TEST_NE, do: 2
  @doc enum: true
  def cv_TEST_LE, do: 3
  @doc enum: true
  def cv_TEST_LT, do: 4
  @doc enum: true
  def cv_TEST_GE, do: 5
  @doc enum: true
  def cv_TEST_GT, do: 6
end
