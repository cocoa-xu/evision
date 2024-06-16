defmodule Evision.AKAZE.DescriptorType do
  @type enum :: integer()
  @doc enum: true
  def cv_DESCRIPTOR_KAZE_UPRIGHT, do: 2
  @doc enum: true
  def cv_DESCRIPTOR_KAZE, do: 3
  @doc enum: true
  def cv_DESCRIPTOR_MLDB_UPRIGHT, do: 4
  @doc enum: true
  def cv_DESCRIPTOR_MLDB, do: 5
end
