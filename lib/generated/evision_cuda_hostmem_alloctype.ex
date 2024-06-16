defmodule Evision.CUDA.HostMem.AllocType do
  @type enum :: integer()
  @doc enum: true
  def cv_PAGE_LOCKED, do: 1
  @doc enum: true
  def cv_SHARED, do: 2
  @doc enum: true
  def cv_WRITE_COMBINED, do: 4
end
