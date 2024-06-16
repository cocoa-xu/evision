defmodule Evision.DNN.Backend do
  @type enum :: integer()
  @doc enum: true
  def cv_DNN_BACKEND_DEFAULT, do: 0
  @doc enum: true
  def cv_DNN_BACKEND_HALIDE, do: (0 + 1)
  @doc enum: true
  def cv_DNN_BACKEND_INFERENCE_ENGINE, do: (0 + 2)
  @doc enum: true
  def cv_DNN_BACKEND_OPENCV, do: (0 + 3)
  @doc enum: true
  def cv_DNN_BACKEND_VKCOM, do: (0 + 4)
  @doc enum: true
  def cv_DNN_BACKEND_CUDA, do: (0 + 5)
  @doc enum: true
  def cv_DNN_BACKEND_WEBNN, do: (0 + 6)
  @doc enum: true
  def cv_DNN_BACKEND_TIMVX, do: (0 + 7)
  @doc enum: true
  def cv_DNN_BACKEND_CANN, do: (0 + 8)
end
