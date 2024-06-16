defmodule Evision.DNN.ImagePaddingMode do
  @type enum :: integer()
  @doc enum: true
  def cv_DNN_PMODE_NULL, do: 0
  @doc enum: true
  def cv_DNN_PMODE_CROP_CENTER, do: 1
  @doc enum: true
  def cv_DNN_PMODE_LETTERBOX, do: 2
end
