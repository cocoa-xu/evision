defmodule Evision.PCA.Flags do
  @type enum :: integer()
  @doc enum: true
  def cv_DATA_AS_ROW, do: 0
  @doc enum: true
  def cv_DATA_AS_COL, do: 1
  @doc enum: true
  def cv_USE_AVG, do: 2
end
