defmodule Evision.NeighborSearchMethod do
  @type enum :: integer()
  @doc enum: true
  def cv_NEIGH_FLANN_KNN, do: 0
  @doc enum: true
  def cv_NEIGH_GRID, do: 1
  @doc enum: true
  def cv_NEIGH_FLANN_RADIUS, do: 2
end
