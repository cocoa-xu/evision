defmodule Evision.XImgProc.AngleRangeOption do
  @type enum :: integer()
  @doc enum: true
  def cv_ARO_0_45, do: 0
  @doc enum: true
  def cv_ARO_45_90, do: 1
  @doc enum: true
  def cv_ARO_90_135, do: 2
  @doc enum: true
  def cv_ARO_315_0, do: 3
  @doc enum: true
  def cv_ARO_315_45, do: 4
  @doc enum: true
  def cv_ARO_45_135, do: 5
  @doc enum: true
  def cv_ARO_315_135, do: 6
  @doc enum: true
  def cv_ARO_CTR_HOR, do: 7
  @doc enum: true
  def cv_ARO_CTR_VER, do: 8
end
