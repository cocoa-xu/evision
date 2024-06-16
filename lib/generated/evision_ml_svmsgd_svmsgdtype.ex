defmodule Evision.ML.SVMSGD.SvmsgdType do
  @type enum :: integer()
  @doc enum: true
  def cv_SGD, do: 0
  @doc enum: true
  def cv_ASGD, do: 1
end
