defmodule Evision.FileStorage.Mode do
  import Bitwise

  @type enum :: integer()
  @doc enum: true
  def cv_READ, do: 0
  @doc enum: true
  def cv_WRITE, do: 1
  @doc enum: true
  def cv_APPEND, do: 2
  @doc enum: true
  def cv_MEMORY, do: 4
  @doc enum: true
  def cv_FORMAT_MASK, do: bsl(7, 3)
  @doc enum: true
  def cv_FORMAT_AUTO, do: 0
  @doc enum: true
  def cv_FORMAT_XML, do: bsl(1, 3)
  @doc enum: true
  def cv_FORMAT_YAML, do: bsl(2, 3)
  @doc enum: true
  def cv_FORMAT_JSON, do: bsl(3, 3)
  @doc enum: true
  def cv_BASE64, do: 64
  @doc enum: true
  def cv_WRITE_BASE64, do: bor(cv_BASE64(), cv_WRITE())
end
