defmodule Evision.ML.ErrorTypes do
  @type enum :: integer()
  @doc enum: true
  def cv_TEST_ERROR, do: 0
  @doc enum: true
  def cv_TRAIN_ERROR, do: 1
end
