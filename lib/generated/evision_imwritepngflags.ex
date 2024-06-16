defmodule Evision.ImwritePNGFlags do
  @type enum :: integer()
  @doc enum: true
  def cv_IMWRITE_PNG_STRATEGY_DEFAULT, do: 0
  @doc enum: true
  def cv_IMWRITE_PNG_STRATEGY_FILTERED, do: 1
  @doc enum: true
  def cv_IMWRITE_PNG_STRATEGY_HUFFMAN_ONLY, do: 2
  @doc enum: true
  def cv_IMWRITE_PNG_STRATEGY_RLE, do: 3
  @doc enum: true
  def cv_IMWRITE_PNG_STRATEGY_FIXED, do: 4
end
