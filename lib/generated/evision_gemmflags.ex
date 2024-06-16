defmodule Evision.GemmFlags do
  @type enum :: integer()
  @doc enum: true
  def cv_GEMM_1_T, do: 1
  @doc enum: true
  def cv_GEMM_2_T, do: 2
  @doc enum: true
  def cv_GEMM_3_T, do: 4
end
