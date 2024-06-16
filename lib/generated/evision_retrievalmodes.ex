defmodule Evision.RetrievalModes do
  @type enum :: integer()
  @doc enum: true
  def cv_RETR_EXTERNAL, do: 0
  @doc enum: true
  def cv_RETR_LIST, do: 1
  @doc enum: true
  def cv_RETR_CCOMP, do: 2
  @doc enum: true
  def cv_RETR_TREE, do: 3
  @doc enum: true
  def cv_RETR_FLOODFILL, do: 4
end
