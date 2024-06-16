defmodule Evision.XImgProc.EdgeAwareFiltersList do
  @type enum :: integer()
  @doc enum: true
  def cv_DTF_NC, do: 0
  @doc enum: true
  def cv_DTF_IC, do: 1
  @doc enum: true
  def cv_DTF_RF, do: 2
  @doc enum: true
  def cv_GUIDED_FILTER, do: 3
  @doc enum: true
  def cv_AM_FILTER, do: 4
end
