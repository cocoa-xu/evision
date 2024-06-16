defmodule Evision.InterpolationMasks do
  import Bitwise

  @type enum :: integer()
  @doc enum: true
  def cv_INTER_BITS, do: 5
  @doc enum: true
  def cv_INTER_BITS2, do: (cv_INTER_BITS() * 2)
  @doc enum: true
  def cv_INTER_TAB_SIZE, do: bsl(1, cv_INTER_BITS())
  @doc enum: true
  def cv_INTER_TAB_SIZE2, do: (cv_INTER_TAB_SIZE() * cv_INTER_TAB_SIZE())
end
