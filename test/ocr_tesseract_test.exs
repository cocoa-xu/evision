defmodule Evision.OCRTesseractTest do
  use ExUnit.Case

  @moduletag timeout: 120_000
  @tag :require_tesseract

  @test_image Path.join(__DIR__, "testdata/ocr_test.png")

  test "OCRTesseract.run returns recognized text" do
    tess = Evision.Text.OCRTesseract.create()
    mat = Evision.imread(@test_image)
    text = Evision.Text.OCRTesseract.run(tess, mat, 0)
    assert is_binary(text)
    assert String.contains?(String.upcase(text), "HELLO")
  end

  test "OCRTesseract.runWithComponents returns text with bounding boxes" do
    tess = Evision.Text.OCRTesseract.create()
    mat = Evision.imread(@test_image)

    result = Evision.Text.OCRTesseract.runWithComponents(tess, mat)
    assert {text, rects, texts, confidences} = result

    assert is_binary(text)
    assert String.contains?(String.upcase(text), "HELLO")

    assert length(rects) > 0
    assert length(texts) == length(rects)
    assert length(confidences) == length(rects)

    Enum.each(rects, fn {x, y, w, h} ->
      assert is_integer(x) and is_integer(y)
      assert is_integer(w) and is_integer(h)
    end)

    Enum.each(confidences, fn conf ->
      assert is_float(conf)
    end)
  end
end
