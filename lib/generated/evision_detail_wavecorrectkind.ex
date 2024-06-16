defmodule Evision.Detail.WaveCorrectKind do
  @type enum :: integer()
  @doc enum: true
  def cv_WAVE_CORRECT_HORIZ, do: 0
  @doc enum: true
  def cv_WAVE_CORRECT_VERT, do: 1
  @doc enum: true
  def cv_WAVE_CORRECT_AUTO, do: 2
end
