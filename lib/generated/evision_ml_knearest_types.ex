defmodule Evision.ML.KNearest.Types do
  @type enum :: integer()
  @doc enum: true
  def cv_BRUTE_FORCE, do: 1
  @doc enum: true
  def cv_KDTREE, do: 2
end
