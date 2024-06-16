defmodule Evision.CUDACodec.VideoReaderProps do
  @type enum :: integer()
  @doc enum: true
  def cv_PROP_DECODED_FRAME_IDX, do: 0
  @doc enum: true
  def cv_PROP_EXTRA_DATA_INDEX, do: 1
  @doc enum: true
  def cv_PROP_RAW_PACKAGES_BASE_INDEX, do: 2
  @doc enum: true
  def cv_PROP_NUMBER_OF_RAW_PACKAGES_SINCE_LAST_GRAB, do: 3
  @doc enum: true
  def cv_PROP_RAW_MODE, do: 4
  @doc enum: true
  def cv_PROP_LRF_HAS_KEY_FRAME, do: 5
  @doc enum: true
  def cv_PROP_COLOR_FORMAT, do: 6
  @doc enum: true
  def cv_PROP_UDP_SOURCE, do: 7
  @doc enum: true
  def cv_PROP_ALLOW_FRAME_DROP, do: 8
  @doc enum: true
  def cv_PROP_NOT_SUPPORTED, do: (8 + 1)
end
