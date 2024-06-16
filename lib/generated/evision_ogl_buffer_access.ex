defmodule Evision.OGL.Buffer.Access do
  @type enum :: integer()
  @doc enum: true
  def cv_READ_ONLY, do: 35000
  @doc enum: true
  def cv_WRITE_ONLY, do: 35001
  @doc enum: true
  def cv_READ_WRITE, do: 35002
end
