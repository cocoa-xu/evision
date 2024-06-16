defmodule Evision.CUDA.DeviceInfo.ComputeMode do
  @type enum :: integer()
  @doc enum: true
  def cv_ComputeModeDefault, do: 0
  @doc enum: true
  def cv_ComputeModeExclusive, do: 1
  @doc enum: true
  def cv_ComputeModeProhibited, do: 2
  @doc enum: true
  def cv_ComputeModeExclusiveProcess, do: 3
end
