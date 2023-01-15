defmodule Evision.DNN.DetectionModel.Test do
  use ExUnit.Case
  alias Evision.DNN.DetectionModel

  @moduletag timeout: 600_000
  @tag :dnn
  @tag :require_downloading
  @download_file "https://github.com/AlexeyAB/darknet/releases/download/yolov4/yolov4.weights"

  test "yolov4" do
    dog = Path.join([__DIR__, "testdata", "dog.jpg"])
    weights = Path.join([__DIR__, "testdata", "yolov4.weights"])
    config = Path.join([__DIR__, "testdata", "yolov4.cfg"])
    mat = Evision.imread(dog)

    Evision.TestHelper.download!(@download_file, weights)

    net = Evision.DNN.readNet(weights, config: config, framework: "")

    model = DetectionModel.detectionModel(net)
    model = DetectionModel.setInputParams(model,
      scale: 1.0,
      size: {416, 416},
      mean: {0, 0, 0},
      swapRB: true,
      crop: false
    )

    {classes, _, _} = DetectionModel.detect(model, mat)

    assert classes == [
             0,
             24,
             24,
             24,
             0,
             24,
             24,
             0,
             47,
             9,
             9,
             9,
             9,
             9,
             9,
             26,
             24,
             12,
             12,
             47,
             24,
             24,
             24,
             26,
             26,
             26,
             26,
             26,
             26,
             26,
             47,
             26,
             26
           ]
  end
end
