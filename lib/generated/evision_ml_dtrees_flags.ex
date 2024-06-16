defmodule Evision.ML.DTrees.Flags do
  import Bitwise

  @type enum :: integer()
  @doc enum: true
  def cv_PREDICT_AUTO, do: 0
  @doc enum: true
  def cv_PREDICT_SUM, do: bsl(1, 8)
  @doc enum: true
  def cv_PREDICT_MAX_VOTE, do: bsl(2, 8)
  @doc enum: true
  def cv_PREDICT_MASK, do: bsl(3, 8)
end
