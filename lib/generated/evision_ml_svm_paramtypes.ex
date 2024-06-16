defmodule Evision.ML.SVM.ParamTypes do
  @type enum :: integer()
  @doc enum: true
  def cv_C, do: 0
  @doc enum: true
  def cv_GAMMA, do: 1
  @doc enum: true
  def cv_P, do: 2
  @doc enum: true
  def cv_NU, do: 3
  @doc enum: true
  def cv_COEF, do: 4
  @doc enum: true
  def cv_DEGREE, do: 5
end
