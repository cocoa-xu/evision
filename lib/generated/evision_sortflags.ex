defmodule Evision.SortFlags do
  @type enum :: integer()
  @doc enum: true
  def cv_SORT_EVERY_ROW, do: 0
  @doc enum: true
  def cv_SORT_EVERY_COLUMN, do: 1
  @doc enum: true
  def cv_SORT_ASCENDING, do: 0
  @doc enum: true
  def cv_SORT_DESCENDING, do: 16
end
