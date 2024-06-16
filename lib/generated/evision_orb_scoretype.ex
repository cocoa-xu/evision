defmodule Evision.ORB.ScoreType do
  @type enum :: integer()
  @doc enum: true
  def cv_HARRIS_SCORE, do: 0
  @doc enum: true
  def cv_FAST_SCORE, do: 1
end
