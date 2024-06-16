defmodule Evision.ML.EM.Types do
  @type enum :: integer()
  @doc enum: true
  def cv_COV_MAT_SPHERICAL, do: 0
  @doc enum: true
  def cv_COV_MAT_DIAGONAL, do: 1
  @doc enum: true
  def cv_COV_MAT_GENERIC, do: 2
  @doc enum: true
  def cv_COV_MAT_DEFAULT, do: cv_COV_MAT_DIAGONAL()
end
