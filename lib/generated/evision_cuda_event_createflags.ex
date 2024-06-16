defmodule Evision.CUDA.Event.CreateFlags do
  @type enum :: integer()
  @doc enum: true
  def cv_DEFAULT, do: 0
  @doc enum: true
  def cv_BLOCKING_SYNC, do: 1
  @doc enum: true
  def cv_DISABLE_TIMING, do: 2
  @doc enum: true
  def cv_INTERPROCESS, do: 4
end
