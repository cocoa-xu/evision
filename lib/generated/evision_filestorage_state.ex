defmodule Evision.FileStorage.State do
  @type enum :: integer()
  @doc enum: true
  def cv_UNDEFINED, do: 0
  @doc enum: true
  def cv_VALUE_EXPECTED, do: 1
  @doc enum: true
  def cv_NAME_EXPECTED, do: 2
  @doc enum: true
  def cv_INSIDE_MAP, do: 4
end
