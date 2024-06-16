defmodule Evision.Formatter.FormatType do
  @type enum :: integer()
  @doc enum: true
  def cv_FMT_DEFAULT, do: 0
  @doc enum: true
  def cv_FMT_MATLAB, do: 1
  @doc enum: true
  def cv_FMT_CSV, do: 2
  @doc enum: true
  def cv_FMT_PYTHON, do: 3
  @doc enum: true
  def cv_FMT_NUMPY, do: 4
  @doc enum: true
  def cv_FMT_C, do: 5
end
