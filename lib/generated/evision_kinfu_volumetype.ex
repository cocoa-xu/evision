defmodule Evision.KinFu.VolumeType do
  @type enum :: integer()
  @doc enum: true
  def cv_TSDF, do: 0
  @doc enum: true
  def cv_HASHTSDF, do: 1
  @doc enum: true
  def cv_COLOREDTSDF, do: 2
end
