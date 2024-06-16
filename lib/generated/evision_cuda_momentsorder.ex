defmodule Evision.CUDA.MomentsOrder do
  @type enum :: integer()
  @doc enum: true
  def cv_FIRST_ORDER_MOMENTS, do: 1
  @doc enum: true
  def cv_SECOND_ORDER_MOMENTS, do: 2
  @doc enum: true
  def cv_THIRD_ORDER_MOMENTS, do: 3
end
