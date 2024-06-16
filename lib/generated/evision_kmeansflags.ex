defmodule Evision.KmeansFlags do
  @type enum :: integer()
  @doc enum: true
  def cv_KMEANS_RANDOM_CENTERS, do: 0
  @doc enum: true
  def cv_KMEANS_PP_CENTERS, do: 2
  @doc enum: true
  def cv_KMEANS_USE_INITIAL_LABELS, do: 1
end
