defmodule Evision.PolishingMethod do
  @type enum :: integer()
  @doc enum: true
  def cv_NONE_POLISHER, do: 0
  @doc enum: true
  def cv_LSQ_POLISHER, do: 1
  @doc enum: true
  def cv_MAGSAC, do: 2
  @doc enum: true
  def cv_COV_POLISHER, do: 3
end
