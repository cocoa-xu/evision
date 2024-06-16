defmodule Evision.SVD.Flags do
  @type enum :: integer()
  @doc enum: true
  def cv_MODIFY_A, do: 1
  @doc enum: true
  def cv_NO_UV, do: 2
  @doc enum: true
  def cv_FULL_UV, do: 4
end
