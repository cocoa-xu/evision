defmodule Evision.TrackerKCF.MODE do
  import Bitwise

  @type enum :: integer()
  @doc enum: true
  def cv_GRAY, do: bsl(1, 0)
  @doc enum: true
  def cv_CN, do: bsl(1, 1)
  @doc enum: true
  def cv_CUSTOM, do: bsl(1, 2)
end
