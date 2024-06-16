defmodule Evision.HOGDescriptor.DescriptorStorageFormat do
  @type enum :: integer()
  @doc enum: true
  def cv_DESCR_FORMAT_COL_BY_COL, do: 0
  @doc enum: true
  def cv_DESCR_FORMAT_ROW_BY_ROW, do: 1
end
