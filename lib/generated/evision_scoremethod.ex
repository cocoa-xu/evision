defmodule Evision.ScoreMethod do
  @type enum :: integer()
  @doc enum: true
  def cv_SCORE_METHOD_RANSAC, do: 0
  @doc enum: true
  def cv_SCORE_METHOD_MSAC, do: 1
  @doc enum: true
  def cv_SCORE_METHOD_MAGSAC, do: 2
  @doc enum: true
  def cv_SCORE_METHOD_LMEDS, do: 3
end
