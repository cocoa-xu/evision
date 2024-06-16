defmodule Evision.BorderTypes do
  @type enum :: integer()
  @doc enum: true
  def cv_BORDER_CONSTANT, do: 0
  @doc enum: true
  def cv_BORDER_REPLICATE, do: 1
  @doc enum: true
  def cv_BORDER_REFLECT, do: 2
  @doc enum: true
  def cv_BORDER_WRAP, do: 3
  @doc enum: true
  def cv_BORDER_REFLECT_101, do: 4
  @doc enum: true
  def cv_BORDER_TRANSPARENT, do: 5
  @doc enum: true
  def cv_BORDER_REFLECT101, do: cv_BORDER_REFLECT_101()
  @doc enum: true
  def cv_BORDER_DEFAULT, do: cv_BORDER_REFLECT_101()
  @doc enum: true
  def cv_BORDER_ISOLATED, do: 16
end
