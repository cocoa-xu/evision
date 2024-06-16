defmodule Evision.OCL.OclVectorStrategy do
  @type enum :: integer()
  @doc enum: true
  def cv_OCL_VECTOR_OWN, do: 0
  @doc enum: true
  def cv_OCL_VECTOR_MAX, do: 1
  @doc enum: true
  def cv_OCL_VECTOR_DEFAULT, do: cv_OCL_VECTOR_OWN()
end
