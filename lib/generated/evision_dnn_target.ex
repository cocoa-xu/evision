defmodule Evision.DNN.Target do
  @type enum :: integer()
  @doc enum: true
  def cv_DNN_TARGET_CPU, do: 0
  @doc enum: true
  def cv_DNN_TARGET_OPENCL, do: (0 + 1)
  @doc enum: true
  def cv_DNN_TARGET_OPENCL_FP16, do: (0 + 2)
  @doc enum: true
  def cv_DNN_TARGET_MYRIAD, do: (0 + 3)
  @doc enum: true
  def cv_DNN_TARGET_VULKAN, do: (0 + 4)
  @doc enum: true
  def cv_DNN_TARGET_FPGA, do: (0 + 5)
  @doc enum: true
  def cv_DNN_TARGET_CUDA, do: (0 + 6)
  @doc enum: true
  def cv_DNN_TARGET_CUDA_FP16, do: (0 + 7)
  @doc enum: true
  def cv_DNN_TARGET_HDDL, do: (0 + 8)
  @doc enum: true
  def cv_DNN_TARGET_NPU, do: (0 + 9)
  @doc enum: true
  def cv_DNN_TARGET_CPU_FP16, do: (0 + 10)
end
