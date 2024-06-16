defmodule Evision.HistCompMethods do
  @type enum :: integer()
  @doc enum: true
  def cv_HISTCMP_CORREL, do: 0
  @doc enum: true
  def cv_HISTCMP_CHISQR, do: 1
  @doc enum: true
  def cv_HISTCMP_INTERSECT, do: 2
  @doc enum: true
  def cv_HISTCMP_BHATTACHARYYA, do: 3
  @doc enum: true
  def cv_HISTCMP_HELLINGER, do: cv_HISTCMP_BHATTACHARYYA()
  @doc enum: true
  def cv_HISTCMP_CHISQR_ALT, do: 4
  @doc enum: true
  def cv_HISTCMP_KL_DIV, do: 5
end
