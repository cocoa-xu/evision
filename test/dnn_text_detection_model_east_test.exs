defmodule Evision.DNN.TextDetectionModelEAST.Test do
  use ExUnit.Case
  alias Evision.DNN.TextDetectionModelEAST

  @moduletag timeout: 600_000
  @tag :dnn
  @tag :require_downloading
  @download_file "https://huggingface.co/opencv/opencv_contribution/resolve/main/east_text_detection/east_text_detection_2026jul.onnx?download=true"
  test "Text Detection EAST" do
    weights = Path.join([__DIR__, "testdata", "east_text_detection.onnx"])
    text_image = Evision.imread(Path.join([__DIR__, "testdata", "1023_6.png"]))

    Evision.TestHelper.download!(@download_file, weights)

    net = Evision.DNN.readNet(weights, config: "", framework: "")

    model =
      TextDetectionModelEAST.textDetectionModelEAST(net)
      |> TextDetectionModelEAST.setInputParams(
        scale: 1.0,
        size: {320, 320},
        mean: {123.68, 116.78, 103.94},
        swapRB: true,
        crop: false
      )
      |> TextDetectionModelEAST.setConfidenceThreshold(0.5)
      |> TextDetectionModelEAST.setNMSThreshold(0.4)

    {vertices, confidences} = TextDetectionModelEAST.detect(model, text_image)

    assert [[{_, _}, {_, _}, {_, _}, {_, _}] = quad] = vertices

    for {{x, y}, {expected_x, expected_y}} <- Enum.zip(quad, [{3, 16}, {3, 8}, {50, 7}, {50, 16}]) do
      assert_in_delta x, expected_x, 2
      assert_in_delta y, expected_y, 2
    end

    assert [confidence] = confidences
    assert_in_delta confidence, 0.9988, 1.0e-3
  end
end
