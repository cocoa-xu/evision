defmodule Evision.FloodFillFlags do
  import Bitwise

  @type enum :: integer()
  @doc enum: true
  def cv_FLOODFILL_FIXED_RANGE, do: bsl(1, 16)
  @doc enum: true
  def cv_FLOODFILL_MASK_ONLY, do: bsl(1, 17)
end
