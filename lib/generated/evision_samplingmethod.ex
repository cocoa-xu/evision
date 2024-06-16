defmodule Evision.SamplingMethod do
  @type enum :: integer()
  @doc enum: true
  def cv_SAMPLING_UNIFORM, do: 0
  @doc enum: true
  def cv_SAMPLING_PROGRESSIVE_NAPSAC, do: 1
  @doc enum: true
  def cv_SAMPLING_NAPSAC, do: 2
  @doc enum: true
  def cv_SAMPLING_PROSAC, do: 3
end
