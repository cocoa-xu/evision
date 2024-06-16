defmodule Evision.DecompTypes do
  @type enum :: integer()
  @doc enum: true
  def cv_DECOMP_LU, do: 0
  @doc enum: true
  def cv_DECOMP_SVD, do: 1
  @doc enum: true
  def cv_DECOMP_EIG, do: 2
  @doc enum: true
  def cv_DECOMP_CHOLESKY, do: 3
  @doc enum: true
  def cv_DECOMP_QR, do: 4
  @doc enum: true
  def cv_DECOMP_NORMAL, do: 16
end
