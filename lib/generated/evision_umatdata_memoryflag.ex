defmodule Evision.UMatData.MemoryFlag do
  @type enum :: integer()
  @doc enum: true
  def cv_COPY_ON_MAP, do: 1
  @doc enum: true
  def cv_HOST_COPY_OBSOLETE, do: 2
  @doc enum: true
  def cv_DEVICE_COPY_OBSOLETE, do: 4
  @doc enum: true
  def cv_TEMP_UMAT, do: 8
  @doc enum: true
  def cv_TEMP_COPIED_UMAT, do: 24
  @doc enum: true
  def cv_USER_ALLOCATED, do: 32
  @doc enum: true
  def cv_DEVICE_MEM_MAPPED, do: 64
  @doc enum: true
  def cv_ASYNC_CLEANUP, do: 128
end
