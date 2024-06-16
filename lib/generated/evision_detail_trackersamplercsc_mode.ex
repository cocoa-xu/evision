defmodule Evision.Detail.TrackerSamplerCSC.MODE do
  @type enum :: integer()
  @doc enum: true
  def cv_MODE_INIT_POS, do: 1
  @doc enum: true
  def cv_MODE_INIT_NEG, do: 2
  @doc enum: true
  def cv_MODE_TRACK_POS, do: 3
  @doc enum: true
  def cv_MODE_TRACK_NEG, do: 4
  @doc enum: true
  def cv_MODE_DETECT, do: 5
end
