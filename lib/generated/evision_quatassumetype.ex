defmodule Evision.QuatAssumeType do
  @type enum :: integer()
  @doc enum: true
  def cv_QUAT_ASSUME_NOT_UNIT, do: 0
  @doc enum: true
  def cv_QUAT_ASSUME_UNIT, do: 1
end
