defmodule Evision.Text.OCREngineMode do
  @type enum :: integer()
  @doc enum: true
  def cv_OEM_TESSERACT_ONLY, do: 0
  @doc enum: true
  def cv_OEM_CUBE_ONLY, do: 1
  @doc enum: true
  def cv_OEM_TESSERACT_CUBE_COMBINED, do: 2
  @doc enum: true
  def cv_OEM_DEFAULT, do: 3
end
