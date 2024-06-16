defmodule Evision.OCL.KernelArg do
  @type enum :: integer()
  @doc enum: true
  def cv_LOCAL, do: 1
  @doc enum: true
  def cv_READ_ONLY, do: 2
  @doc enum: true
  def cv_WRITE_ONLY, do: 4
  @doc enum: true
  def cv_READ_WRITE, do: 6
  @doc enum: true
  def cv_CONSTANT, do: 8
  @doc enum: true
  def cv_PTR_ONLY, do: 16
  @doc enum: true
  def cv_NO_SIZE, do: 256
end
