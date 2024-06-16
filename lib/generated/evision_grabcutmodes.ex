defmodule Evision.GrabCutModes do
  @type enum :: integer()
  @doc enum: true
  def cv_GC_INIT_WITH_RECT, do: 0
  @doc enum: true
  def cv_GC_INIT_WITH_MASK, do: 1
  @doc enum: true
  def cv_GC_EVAL, do: 2
  @doc enum: true
  def cv_GC_EVAL_FREEZE_MODEL, do: 3
end
