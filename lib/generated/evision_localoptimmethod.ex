defmodule Evision.LocalOptimMethod do
  @type enum :: integer()
  @doc enum: true
  def cv_LOCAL_OPTIM_NULL, do: 0
  @doc enum: true
  def cv_LOCAL_OPTIM_INNER_LO, do: 1
  @doc enum: true
  def cv_LOCAL_OPTIM_INNER_AND_ITER_LO, do: 2
  @doc enum: true
  def cv_LOCAL_OPTIM_GC, do: 3
  @doc enum: true
  def cv_LOCAL_OPTIM_SIGMA, do: 4
end
