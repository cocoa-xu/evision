defmodule Evision.AccessFlag do
  import Bitwise

  @type enum :: integer()
  @doc enum: true
  def cv_ACCESS_READ, do: bsl(1, 24)
  @doc enum: true
  def cv_ACCESS_WRITE, do: bsl(1, 25)
  @doc enum: true
  def cv_ACCESS_RW, do: bsl(3, 24)
  @doc enum: true
  def cv_ACCESS_MASK, do: cv_ACCESS_RW()
  @doc enum: true
  def cv_ACCESS_FAST, do: bsl(1, 26)
end
