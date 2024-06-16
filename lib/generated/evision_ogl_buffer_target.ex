defmodule Evision.OGL.Buffer.Target do
  @type enum :: integer()
  @doc enum: true
  def cv_ARRAY_BUFFER, do: 34962
  @doc enum: true
  def cv_ELEMENT_ARRAY_BUFFER, do: 34963
  @doc enum: true
  def cv_PIXEL_PACK_BUFFER, do: 35051
  @doc enum: true
  def cv_PIXEL_UNPACK_BUFFER, do: 35052
end
