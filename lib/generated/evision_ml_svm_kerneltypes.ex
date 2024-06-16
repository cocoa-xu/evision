defmodule Evision.ML.SVM.KernelTypes do
  @type enum :: integer()
  @doc enum: true
  def cv_CUSTOM, do: -1
  @doc enum: true
  def cv_LINEAR, do: 0
  @doc enum: true
  def cv_POLY, do: 1
  @doc enum: true
  def cv_RBF, do: 2
  @doc enum: true
  def cv_SIGMOID, do: 3
  @doc enum: true
  def cv_CHI2, do: 4
  @doc enum: true
  def cv_INTER, do: 5
end
