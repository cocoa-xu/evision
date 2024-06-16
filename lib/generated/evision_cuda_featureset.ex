defmodule Evision.CUDA.FeatureSet do
  @type enum :: integer()
  @doc enum: true
  def cv_FEATURE_SET_COMPUTE_10, do: 10
  @doc enum: true
  def cv_FEATURE_SET_COMPUTE_11, do: 11
  @doc enum: true
  def cv_FEATURE_SET_COMPUTE_12, do: 12
  @doc enum: true
  def cv_FEATURE_SET_COMPUTE_13, do: 13
  @doc enum: true
  def cv_FEATURE_SET_COMPUTE_20, do: 20
  @doc enum: true
  def cv_FEATURE_SET_COMPUTE_21, do: 21
  @doc enum: true
  def cv_FEATURE_SET_COMPUTE_30, do: 30
  @doc enum: true
  def cv_FEATURE_SET_COMPUTE_32, do: 32
  @doc enum: true
  def cv_FEATURE_SET_COMPUTE_35, do: 35
  @doc enum: true
  def cv_FEATURE_SET_COMPUTE_50, do: 50
  @doc enum: true
  def cv_GLOBAL_ATOMICS, do: cv_FEATURE_SET_COMPUTE_11()
  @doc enum: true
  def cv_SHARED_ATOMICS, do: cv_FEATURE_SET_COMPUTE_12()
  @doc enum: true
  def cv_NATIVE_DOUBLE, do: cv_FEATURE_SET_COMPUTE_13()
  @doc enum: true
  def cv_WARP_SHUFFLE_FUNCTIONS, do: cv_FEATURE_SET_COMPUTE_30()
  @doc enum: true
  def cv_DYNAMIC_PARALLELISM, do: cv_FEATURE_SET_COMPUTE_35()
end
