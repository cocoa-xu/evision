defmodule Evision.DNN.DataLayout do
  @type enum :: integer()
  @doc enum: true
  def cv_DNN_LAYOUT_UNKNOWN, do: 0
  @doc enum: true
  def cv_DNN_LAYOUT_ND, do: 1
  @doc enum: true
  def cv_DNN_LAYOUT_NCHW, do: 2
  @doc enum: true
  def cv_DNN_LAYOUT_NCDHW, do: 3
  @doc enum: true
  def cv_DNN_LAYOUT_NHWC, do: 4
  @doc enum: true
  def cv_DNN_LAYOUT_NDHWC, do: 5
  @doc enum: true
  def cv_DNN_LAYOUT_PLANAR, do: 6
end
