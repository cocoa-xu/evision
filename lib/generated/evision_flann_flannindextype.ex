defmodule Evision.Flann.FlannIndexType do
  @type enum :: integer()
  @doc enum: true
  def cv_FLANN_INDEX_TYPE_8U, do: 0
  @doc enum: true
  def cv_FLANN_INDEX_TYPE_8S, do: 1
  @doc enum: true
  def cv_FLANN_INDEX_TYPE_16U, do: 2
  @doc enum: true
  def cv_FLANN_INDEX_TYPE_16S, do: 3
  @doc enum: true
  def cv_FLANN_INDEX_TYPE_32S, do: 4
  @doc enum: true
  def cv_FLANN_INDEX_TYPE_32F, do: 5
  @doc enum: true
  def cv_FLANN_INDEX_TYPE_64F, do: 6
  @doc enum: true
  def cv_FLANN_INDEX_TYPE_STRING, do: (6 + 1)
  @doc enum: true
  def cv_FLANN_INDEX_TYPE_BOOL, do: (6 + 2)
  @doc enum: true
  def cv_FLANN_INDEX_TYPE_ALGORITHM, do: (6 + 3)
  @doc enum: true
  def cv_LAST_VALUE_FLANN_INDEX_TYPE, do: cv_FLANN_INDEX_TYPE_ALGORITHM()
end
