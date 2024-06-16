defmodule Evision.Text.ClassifierType do
  @type enum :: integer()
  @doc enum: true
  def cv_OCR_KNN_CLASSIFIER, do: 0
  @doc enum: true
  def cv_OCR_CNN_CLASSIFIER, do: 1
end
