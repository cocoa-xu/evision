defmodule Evision.XImgProc.LocalBinarizationMethods do
  @type enum :: integer()
  @doc enum: true
  def cv_BINARIZATION_NIBLACK, do: 0
  @doc enum: true
  def cv_BINARIZATION_SAUVOLA, do: 1
  @doc enum: true
  def cv_BINARIZATION_WOLF, do: 2
  @doc enum: true
  def cv_BINARIZATION_NICK, do: 3
end
