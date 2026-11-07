defmodule Evision.DNNDetection.Test do
  use ExUnit.Case

  @moduletag timeout: 600_000

  @download_file "https://huggingface.co/opencv/opencv_contribution/resolve/main/ssd_mobilenet_v2_coco_2018_03_29/ssd_mobilenet_v2_coco_2018_03_29_2026jul.onnx?download=true"
  @out_names [
    "detection_boxes:0",
    "detection_scores:0",
    "detection_classes:0",
    "num_detections:0"
  ]

  # The ONNX export of the TensorFlow model takes a uint8 NHWC RGB image and
  # returns normalized {y1, x1, y2, x2} boxes with 1-based COCO class ids.
  defp detect(net, image, labels, confidence_threshold) do
    {h, w, _} = image.shape

    input =
      image
      |> Evision.cvtColor(Evision.Constant.cv_COLOR_BGR2RGB())
      |> Evision.resize({300, 300})
      |> Evision.Mat.to_binary()
      |> Evision.Mat.from_binary_by_shape({:u, 8}, {1, 300, 300, 3})

    [boxes, scores, classes, count] =
      net
      |> Evision.DNN.Net.setInput(input)
      |> Evision.DNN.Net.forward(outBlobNames: @out_names)
      |> Enum.map(&Evision.Mat.to_nx(&1, Nx.BinaryBackend))

    count = count |> Nx.to_flat_list() |> hd() |> trunc()
    boxes = boxes |> Nx.reshape({:auto, 4}) |> Nx.to_list()

    [boxes, Nx.to_flat_list(scores), Nx.to_flat_list(classes)]
    |> Enum.zip()
    |> Enum.take(count)
    |> Enum.filter(fn {_box, score, _class} -> score > confidence_threshold end)
    |> Enum.map(fn {[y1, x1, y2, x2], score, class} ->
      {Enum.at(labels, trunc(class) - 1), score,
       {trunc(x1 * w), trunc(y1 * h), trunc(x2 * w), trunc(y2 * h)}}
    end)
  end

  @tag :dnn
  @tag :require_downloading
  test "load ssd_mobilenet_v2 and do inference" do
    model = Path.join([__DIR__, "testdata", "models", "ssd_mobilenet_v2_coco_2018_03_29.onnx"])
    Evision.TestHelper.download!(@download_file, model)

    labels =
      Path.join([__DIR__, "testdata", "models", "coco_names.txt"])
      |> File.read!()
      |> String.split("\n")

    image = Evision.imread(Path.join([__DIR__, "testdata", "dnn_detection_test.jpg"]))
    detections = detect(Evision.DNN.readNet(model), image, labels, 0.5)

    assert detections |> Enum.map(&elem(&1, 0)) |> Enum.frequencies() ==
             %{"car" => 5, "person" => 1}

    assert [{"car", score, box} | _] = detections
    assert_in_delta score, 0.9736, 1.0e-3

    for {actual, expected} <- Enum.zip(Tuple.to_list(box), [464, 205, 602, 339]) do
      assert_in_delta actual, expected, 2
    end
  end
end
