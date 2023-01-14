defmodule Evision.DNN.TextDetectionModelEAST.Test do
  use ExUnit.Case
  alias Evision.DNN.TextDetectionModelEAST

  @moduletag timeout: 600_000
  @tag :dnn
  @tag :require_downloading
  @download_file_1 "https://github.com/cocoa-xu/evision-extra/raw/main/testdata/text-detection/frozen_east_text_detection.pb.part1"
  @download_file_2 "https://github.com/cocoa-xu/evision-extra/raw/main/testdata/text-detection/frozen_east_text_detection.pb.part2"
  test "Text Detection EAST" do
    weights_p1 = Path.join([__DIR__, "testdata", "frozen_east_text_detection.pb.part1"])
    weights_p2 = Path.join([__DIR__, "testdata", "frozen_east_text_detection.pb.part2"])
    weights = Path.join([__DIR__, "testdata", "frozen_east_text_detection.pb"])
    text_image = Evision.imread(Path.join([__DIR__, "testdata", "1023_6.png"]))

    Evision.TestHelper.download!(@download_file_1, weights_p1)
    Evision.TestHelper.download!(@download_file_2, weights_p2)
    data_p1 = File.read!(weights_p1)
    data_p2 = File.read!(weights_p2)
    File.write!(weights, data_p1 <> data_p2)

    net = Evision.DNN.readNet(weights, config: "", framework: "")

    # disable Winograd, OpenCV 4.7.0
    # https://github.com/opencv/opencv/issues/23080
    enable_winograd = System.get_env("ENABLE_WINOGRAD", "no")
    if enable_winograd == "no" do
      Evision.DNN.Net.enableWinograd(net, false)
    end

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

    assert vertices == [[{3, 16}, {3, 8}, {50, 7}, {50, 16}]]
    assert confidences == [0.998794674873352]
  end
end
