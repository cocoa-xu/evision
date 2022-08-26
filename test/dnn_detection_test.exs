defmodule Evision.DNN.Test do
  use ExUnit.Case
  import ExUnit.CaptureIO
  alias Evision, as: Cv

  @moduletag timeout: 600_000

  defmodule DetectionModel do
    def visualise_pred(mat, _labels, []), do: {:ok, mat}

    def visualise_pred(mat, labels, [translated_out | outs]) do
      {:ok, mat} = _visualise_pred(mat, labels, translated_out)
      visualise_pred(mat, labels, outs)
    end

    defp _visualise_pred(mat, _labels, []), do: {:ok, mat}

    defp _visualise_pred(mat, labels, [{class_id, confidence, l, t, r, b} | outs]) do
      confidence = "#{Float.round(confidence, 2)}"
      label = Enum.at(labels, class_id)
      text = "#{label}: #{confidence}"
      {:ok, mat} = Cv.rectangle(mat, [l, t], [r, b], [255, 0, 0])

      {:ok, {{label_weight, label_height}, baseline}} =
        Cv.getTextSize(text, Cv.cv_FONT_HERSHEY_SIMPLEX(), 0.5, 1)

      label_weight = trunc(label_weight)
      label_height = trunc(label_height)
      top = max(t, label_height)

      {:ok, mat} =
        Cv.rectangle(mat, [l, top - label_height], [l + label_weight, top + baseline], [
          255,
          255,
          255
        ])

      {:ok, mat} = Cv.putText(mat, text, [l, top], Cv.cv_FONT_HERSHEY_SIMPLEX(), 0.5, [0, 0, 255])

      _visualise_pred(mat, labels, outs)
    end

    def postprocess(mat, detections, net, confidence_threshold) do
      {:ok, out_layers} = Cv.DNN.Net.getUnconnectedOutLayers(net)
      {:ok, out_layer} = Cv.DNN.Net.getLayer(net, Enum.at(out_layers, 0))
      out_layer_type = Cv.DNN.Layer.get_type(out_layer) |> IO.iodata_to_binary()
      _postprocess(mat, detections, net, confidence_threshold, out_layer_type, [])
    end

    defp _postprocess(_mat, [], _net, _confidence_threshold, <<"DetectionOutput">>, acc),
      do: {:ok, Enum.reverse(acc)}

    defp _postprocess(
           mat,
           [outs | detections],
           net,
           confidence_threshold,
           <<"DetectionOutput">>,
           acc
         ) do
      {:ok, data} = Cv.Mat.to_binary(outs)
      {:ok, {h, w, _}} = Cv.Mat.shape(mat)
      {:ok, translated_outs} = _translate_outs(confidence_threshold, data, h, w, [])

      _postprocess(mat, detections, net, confidence_threshold, "DetectionOutput", [
        translated_outs | acc
      ])
    end

    defp _translate_outs(_confidence_threshold, <<>>, _h, _w, acc), do: {:ok, acc}

    defp _translate_outs(
           confidence_threshold,
           <<_batch_id::float-size(32)-little, class_id::float-size(32)-little,
             confidence::float-size(32)-little, left::float-size(32)-little,
             top::float-size(32)-little, right::float-size(32)-little,
             bottom::float-size(32)-little, rest::binary>>,
           h,
           w,
           acc
         ) do
      if confidence > confidence_threshold do
        [class_id, l, t, r, b] =
          Enum.map([class_id, left, top, right, bottom], fn f -> trunc(f) end)

        width = r - l + 1
        height = b - t + 1

        [l, t, r, b] =
          if width <= 2 or height <= 2 do
            Enum.map([left * w, top * h, right * w, bottom * h], fn f -> trunc(f) end)
          else
            [l, t, r, b]
          end

        _translate_outs(confidence_threshold, rest, h, w, [
          {class_id - 1, confidence, l, t, r, b} | acc
        ])
      else
        _translate_outs(confidence_threshold, rest, h, w, acc)
      end
    end

    def get_labels(class_label_file) do
      class_label_file
      |> File.read!()
      |> String.split("\n")
    end

    def predict(image_file, model, out_names, opts \\ []) do
      {:ok, mat} = Cv.imread(image_file)
      {:ok, blob} = Cv.DNN.blobFromImage(mat, opts)

      {:ok, model} = Cv.DNN.Net.setInput(model, blob, name: "", scalefactor: 1.0, mean: [0, 0, 0])

      start_time = :os.system_time(:millisecond)
      {:ok, detections} = Cv.DNN.Net.forward(model, outBlobNames: out_names)
      end_time = :os.system_time(:millisecond)
      IO.puts("Inference time=>#{end_time - start_time} ms")
      {:ok, mat, detections}
    end

    def get_model(params, config, framework \\ "") do
      {:ok, net} =
        Cv.DNN.readNet(params,
          config: config,
          framework: framework
        )

      {:ok, out_names} = Cv.DNN.Net.getUnconnectedOutLayersNames(net)
      {:ok, net, out_names}
    end
  end

  defmodule SSDMobileNetV2 do
    def predict_and_show(filename, confidence_threshold \\ 0.5) do
      model_config =
        __DIR__
        |> Path.join("models")
        |> Path.join("ssd_mobilenet_v2_coco_2018_03_29.pbtxt")

      Cv.TestHelper.download!(
        "https://raw.githubusercontent.com/opencv/opencv_extra/master/testdata/dnn/ssd_mobilenet_v2_coco_2018_03_29.pbtxt",
        model_config
      )

      model_class_list =
        __DIR__
        |> Path.join("models")
        |> Path.join("coco_names.txt")

      Cv.TestHelper.download!(
        "https://raw.githubusercontent.com/cocoa-xu/evision/main/test/models/coco_names.txt",
        model_class_list
      )

      File.mkdir_p!(Path.join([__DIR__, "models", "ssd_mobilenet_v2_coco_2018_03_29"]))

      model_graph_pb =
        Path.join([
          __DIR__,
          "models",
          "ssd_mobilenet_v2_coco_2018_03_29",
          "frozen_inference_graph.pb"
        ])

      model_tar =
        __DIR__
        |> Path.join("models")
        |> Path.join("ssd_mobilenet_v2_coco_2018_03_29.tar.gz")

      test_setup =
        if not File.exists?(model_graph_pb) do
          Cv.TestHelper.download!(
            "http://download.tensorflow.org/models/object_detection/ssd_mobilenet_v2_coco_2018_03_29.tar.gz",
            model_tar
          )

          model_tar
          |> File.read!()
          |> :zlib.gunzip()
          |> then(&:erl_tar.extract({:binary, &1}, [:memory, :compressed]))
          |> elem(1)
          |> Enum.map(fn {filename, content} -> {List.to_string(filename), content} end)
          |> Enum.reject(
            &(elem(&1, 0) != "ssd_mobilenet_v2_coco_2018_03_29/frozen_inference_graph.pb")
          )
          |> Enum.at(0)
          |> then(fn {_, content} ->
            File.write!(model_graph_pb, content)
            :ok
          end)
        else
          :ok
        end

      assert :ok == test_setup

      if File.exists?(filename) do
        {:ok, net, out_names} =
          DetectionModel.get_model(
            model_graph_pb,
            model_config
          )

        labels = DetectionModel.get_labels(model_class_list)

        {:ok, mat, detections} =
          DetectionModel.predict(filename, net, out_names,
            scalefactor: 1,
            swapRB: true,
            mean: [0, 0, 0],
            size: [300, 300]
          )

        {:ok, translated_outs} =
          DetectionModel.postprocess(mat, detections, net, confidence_threshold)

        DetectionModel.visualise_pred(mat, labels, translated_outs)
      end
    end
  end

  @tag :dnn
  @tag :require_downloading
  test "load ssd_mobilenet_v2 and do inference" do
    io =
      capture_io(fn ->
        SSDMobileNetV2.predict_and_show(Path.join(__DIR__, "dnn_detection_test.jpg"))
      end)

    assert "Inference time=>" <> _time = io
  end
end
