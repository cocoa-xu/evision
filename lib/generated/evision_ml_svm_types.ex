defmodule Evision.ML.SVM.Types do
  @type enum :: integer()
  @doc enum: true
  def cv_C_SVC, do: 100
  @doc enum: true
  def cv_NU_SVC, do: 101
  @doc enum: true
  def cv_ONE_CLASS, do: 102
  @doc enum: true
  def cv_EPS_SVR, do: 103
  @doc enum: true
  def cv_NU_SVR, do: 104
end
