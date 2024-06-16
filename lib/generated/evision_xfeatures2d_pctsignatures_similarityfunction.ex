defmodule Evision.XFeatures2D.PCTSignatures.SimilarityFunction do
  @type enum :: integer()
  @doc enum: true
  def cv_MINUS, do: 0
  @doc enum: true
  def cv_GAUSSIAN, do: 1
  @doc enum: true
  def cv_HEURISTIC, do: 2
end
