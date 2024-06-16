defmodule Evision.CovarFlags do
  @type enum :: integer()
  @doc enum: true
  def cv_COVAR_SCRAMBLED, do: 0
  @doc enum: true
  def cv_COVAR_NORMAL, do: 1
  @doc enum: true
  def cv_COVAR_USE_AVG, do: 2
  @doc enum: true
  def cv_COVAR_SCALE, do: 4
  @doc enum: true
  def cv_COVAR_ROWS, do: 8
  @doc enum: true
  def cv_COVAR_COLS, do: 16
end
