defmodule Evision.UMatUsageFlags do
  import Bitwise

  @type enum :: integer()
  @doc enum: true
  def cv_USAGE_DEFAULT, do: 0
  @doc enum: true
  def cv_USAGE_ALLOCATE_HOST_MEMORY, do: bsl(1, 0)
  @doc enum: true
  def cv_USAGE_ALLOCATE_DEVICE_MEMORY, do: bsl(1, 1)
  @doc enum: true
  def cv_USAGE_ALLOCATE_SHARED_MEMORY, do: bsl(1, 2)
  @doc enum: true
  def cv_UMAT_USAGE_FLAGS_32BIT, do: 2147483647
end
