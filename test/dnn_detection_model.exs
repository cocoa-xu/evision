defmodule Evision.Test do
  use ExUnit.Case

  alias Evision.{Mat, DNN.DetectionModel}

  @tag :dnn
  @tag :require_downloading
  test "yolov4" do
    dog = Path.join(__DIR__, "dog.jpg")
    weights = Path.join(__DIR__, "yolov4.weights")
    config = Path.join(__DIR__, "yolov4.cfg")
    mat = Evision.imread(dog)

    Evision.TestHelper.download!(
      "https://github.com/AlexeyAB/darknet/releases/download/yolov4/yolov4.weights",
      weights
    )

    DetectionModel.detectionModel(weights, config: config)
    |> DetectionModel.setInputParams(scale: 1.0, size: {416, 416}, mean: {0, 0, 0}, swapRB: true, crop: false)
    |> DetectionModel.detect(mat)
  end
end
