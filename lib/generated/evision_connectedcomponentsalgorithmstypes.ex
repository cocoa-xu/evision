defmodule Evision.ConnectedComponentsAlgorithmsTypes do
  @type enum :: integer()
  @doc enum: true
  def cv_CCL_DEFAULT, do: -1
  @doc enum: true
  def cv_CCL_WU, do: 0
  @doc enum: true
  def cv_CCL_GRANA, do: 1
  @doc enum: true
  def cv_CCL_BOLELLI, do: 2
  @doc enum: true
  def cv_CCL_SAUF, do: 3
  @doc enum: true
  def cv_CCL_BBDT, do: 4
  @doc enum: true
  def cv_CCL_SPAGHETTI, do: 5
end
