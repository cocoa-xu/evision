defmodule Evision.TemplateMatchModes do
  @type enum :: integer()
  @doc enum: true
  def cv_TM_SQDIFF, do: 0
  @doc enum: true
  def cv_TM_SQDIFF_NORMED, do: 1
  @doc enum: true
  def cv_TM_CCORR, do: 2
  @doc enum: true
  def cv_TM_CCORR_NORMED, do: 3
  @doc enum: true
  def cv_TM_CCOEFF, do: 4
  @doc enum: true
  def cv_TM_CCOEFF_NORMED, do: 5
end
