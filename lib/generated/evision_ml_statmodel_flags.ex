defmodule Evision.ML.StatModel.Flags do
  @type enum :: integer()
  @doc enum: true
  def cv_UPDATE_MODEL, do: 1
  @doc enum: true
  def cv_RAW_OUTPUT, do: 1
  @doc enum: true
  def cv_COMPRESSED_INPUT, do: 2
  @doc enum: true
  def cv_PREPROCESSED_INPUT, do: 4
end
