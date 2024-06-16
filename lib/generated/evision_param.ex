defmodule Evision.Param do
  @type enum :: integer()
  @doc enum: true
  def cv_INT, do: 0
  @doc enum: true
  def cv_BOOLEAN, do: 1
  @doc enum: true
  def cv_REAL, do: 2
  @doc enum: true
  def cv_STRING, do: 3
  @doc enum: true
  def cv_MAT, do: 4
  @doc enum: true
  def cv_MAT_VECTOR, do: 5
  @doc enum: true
  def cv_ALGORITHM, do: 6
  @doc enum: true
  def cv_FLOAT, do: 7
  @doc enum: true
  def cv_UNSIGNED_INT, do: 8
  @doc enum: true
  def cv_UINT64, do: 9
  @doc enum: true
  def cv_UCHAR, do: 11
  @doc enum: true
  def cv_SCALAR, do: 12
end
