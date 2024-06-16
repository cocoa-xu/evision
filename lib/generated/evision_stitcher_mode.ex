defmodule Evision.Stitcher.Mode do
  @type enum :: integer()
  @doc enum: true
  def cv_PANORAMA, do: 0
  @doc enum: true
  def cv_SCANS, do: 1
end
