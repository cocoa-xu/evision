defmodule Evision.CCM.DISTANCE_TYPE do
  @type enum :: integer()
  @doc enum: true
  def cv_DISTANCE_CIE76, do: 0
  @doc enum: true
  def cv_DISTANCE_CIE94_GRAPHIC_ARTS, do: 1
  @doc enum: true
  def cv_DISTANCE_CIE94_TEXTILES, do: 2
  @doc enum: true
  def cv_DISTANCE_CIE2000, do: 3
  @doc enum: true
  def cv_DISTANCE_CMC_1TO1, do: 4
  @doc enum: true
  def cv_DISTANCE_CMC_2TO1, do: 5
  @doc enum: true
  def cv_DISTANCE_RGB, do: 6
  @doc enum: true
  def cv_DISTANCE_RGBL, do: 7
end
