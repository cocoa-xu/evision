defmodule Evision.Detail.TrackerSamplerCS do
  @type enum :: integer()
  @doc enum: true
  def cv_MODE_POSITIVE, do: 1
  @doc enum: true
  def cv_MODE_NEGATIVE, do: 2
  @doc enum: true
  def cv_MODE_CLASSIFY, do: 3
end
