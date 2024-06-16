defmodule Evision.XImgProc.WMFWeightType do
  import Bitwise

  @type enum :: integer()
  @doc enum: true
  def cv_WMF_EXP, do: 1
  @doc enum: true
  def cv_WMF_IV1, do: bsl(1, 1)
  @doc enum: true
  def cv_WMF_IV2, do: bsl(1, 2)
  @doc enum: true
  def cv_WMF_COS, do: bsl(1, 3)
  @doc enum: true
  def cv_WMF_JAC, do: bsl(1, 4)
  @doc enum: true
  def cv_WMF_OFF, do: bsl(1, 5)
end
