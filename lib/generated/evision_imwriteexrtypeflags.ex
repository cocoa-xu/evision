defmodule Evision.ImwriteEXRTypeFlags do
  @type enum :: integer()
  @doc enum: true
  def cv_IMWRITE_EXR_TYPE_HALF, do: 1
  @doc enum: true
  def cv_IMWRITE_EXR_TYPE_FLOAT, do: 2
end
