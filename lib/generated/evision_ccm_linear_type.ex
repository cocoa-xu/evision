defmodule Evision.CCM.LINEAR_TYPE do
  @type enum :: integer()
  @doc enum: true
  def cv_LINEARIZATION_IDENTITY, do: 0
  @doc enum: true
  def cv_LINEARIZATION_GAMMA, do: 1
  @doc enum: true
  def cv_LINEARIZATION_COLORPOLYFIT, do: 2
  @doc enum: true
  def cv_LINEARIZATION_COLORLOGPOLYFIT, do: 3
  @doc enum: true
  def cv_LINEARIZATION_GRAYPOLYFIT, do: 4
  @doc enum: true
  def cv_LINEARIZATION_GRAYLOGPOLYFIT, do: 5
end
