defmodule Evision.Mat.DepthMask do
  import Bitwise

  @type enum :: integer()
  @doc enum: true
  def cv_DEPTH_MASK_8U, do: bsl(1, 0)
  @doc enum: true
  def cv_DEPTH_MASK_8S, do: bsl(1, 1)
  @doc enum: true
  def cv_DEPTH_MASK_16U, do: bsl(1, 2)
  @doc enum: true
  def cv_DEPTH_MASK_16S, do: bsl(1, 3)
  @doc enum: true
  def cv_DEPTH_MASK_32S, do: bsl(1, 4)
  @doc enum: true
  def cv_DEPTH_MASK_32F, do: bsl(1, 5)
  @doc enum: true
  def cv_DEPTH_MASK_64F, do: bsl(1, 6)
  @doc enum: true
  def cv_DEPTH_MASK_16F, do: bsl(1, 7)
  @doc enum: true
  def cv_DEPTH_MASK_ALL, do: (bsl(cv_DEPTH_MASK_64F(), 1) - 1)
  @doc enum: true
  def cv_DEPTH_MASK_ALL_BUT_8S, do: band(cv_DEPTH_MASK_ALL(), bnot(cv_DEPTH_MASK_8S()))
  @doc enum: true
  def cv_DEPTH_MASK_ALL_16F, do: (bsl(cv_DEPTH_MASK_16F(), 1) - 1)
  @doc enum: true
  def cv_DEPTH_MASK_FLT, do: (cv_DEPTH_MASK_32F() + cv_DEPTH_MASK_64F())
end
