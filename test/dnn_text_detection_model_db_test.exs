defmodule Evision.DNN.TextDetectionModelDB.Test do
  use ExUnit.Case
  alias Evision.DNN.TextDetectionModelDB

  @moduletag timeout: 600_000
  @tag :dnn
  @tag :require_downloading
  @download_file "https://github.com/cocoa-xu/evision-extra/raw/main/testdata/text-detection/DB_IC15_resnet18.onnx"
  test "Text Detection DB" do
    weights = Path.join([__DIR__, "testdata", "DB_IC15_resnet18.onnx"])
    text_image = Evision.imread(Path.join([__DIR__, "testdata", "1023_6.png"]))

    Evision.TestHelper.download!(
      @download_file,
      weights
    )

    net = Evision.DNN.readNet(weights, config: "", framework: "")

    model =
      TextDetectionModelDB.textDetectionModelDB(net)
      |> TextDetectionModelDB.setInputParams(
        scale: 1.0 / 255.0,
        size: {736, 736},
        mean: {122.67891434, 116.66876762, 104.00698793},
        swapRB: false,
        crop: false
      )
      |> TextDetectionModelDB.setBinaryThreshold(0.3)
      |> TextDetectionModelDB.setPolygonThreshold(0.5)
      |> TextDetectionModelDB.setMaxCandidates(200)
      |> TextDetectionModelDB.setUnclipRatio(2.0)

    {vertices, confidences} = TextDetectionModelDB.detect(model, text_image)

    # arm64 and x86_64 builds differ by up to 3 px on this small image.
    assert [[{_, _}, {_, _}, {_, _}, {_, _}] = quad] = vertices

    for {{x, y}, {expected_x, expected_y}} <- Enum.zip(quad, [{2, 19}, {2, 8}, {27, 5}, {27, 17}]) do
      assert_in_delta x, expected_x, 3
      assert_in_delta y, expected_y, 3
    end

    assert Enum.count(confidences) > 0
  end
end
