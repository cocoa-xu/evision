defmodule Evision.DNN.SoftNMSMethod do
  @type enum :: integer()
  @doc enum: true
  def cv_SOFTNMS_LINEAR, do: 1
  @doc enum: true
  def cv_SOFTNMS_GAUSSIAN, do: 2
end
