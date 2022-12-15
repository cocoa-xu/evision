defmodule Evision.Zoo.TextDetection.DB_IC15 do
  @spec init(binary | :resnet18 | :resnet50, nil | Keyword.t()) :: {:error, String.t()} | Evision.DNN.Net.t()
  def init(model_path, opts \\ [])

  def init(model_type, opts) when model_type in [:resnet18, :resnet50] do
    {model_url, filename} = model_info(model_type)
    cache_dir = opts[:cache_dir]
    with {:ok, local_path} <- Evision.Zoo.download(model_url, filename, cache_dir: cache_dir) do
      init(local_path, opts)
    else
      {:error, msg} ->
        raise msg
    end
  end

  def init(model_path, opts) when is_binary(model_path) do
    config = default_config()
    backend = opts[:backend] || config[:backend]
    target = opts[:target] || config[:target]

    height = opts[:height] || config[:height]
    width = opts[:width] || config[:width]
    binary_threshold = opts[:binary_threshold] || config[:binary_threshold]
    polygon_threshold = opts[:polygon_threshold] || config[:polygon_threshold]
    max_candidates = opts[:max_candidates] || config[:max_candidates]
    unclip_ratio = opts[:unclip_ratio] || config[:unclip_ratio]

    model =
      Evision.DNN.TextDetectionModelDB.textDetectionModelDB(model_path)
      |> Evision.DNN.TextDetectionModelDB.setInputParams(
        scale: 1.0/255.0,
        size: {width, height},
        mean: mean(),
        swapRB: false,
        crop: false
      )
      |> Evision.DNN.TextDetectionModelDB.setBinaryThreshold(binary_threshold)
      |> Evision.DNN.TextDetectionModelDB.setPolygonThreshold(polygon_threshold)
      |> Evision.DNN.TextDetectionModelDB.setMaxCandidates(max_candidates)
      |> Evision.DNN.TextDetectionModelDB.setUnclipRatio(unclip_ratio)
    Evision.DNN.TextDetectionModelDB.setPreferableBackend(model, backend)
    Evision.DNN.TextDetectionModelDB.setPreferableTarget(model, target)
    model
  end

  defp mean, do: {122.67891434, 116.66876762, 104.00698793}

  @spec infer(Evision.DNN.TextDetectionModelDB.t(), Evision.Mat.maybe_mat_in()) :: any()
  def infer(self=%Evision.DNN.TextDetectionModelDB{}, image) do
    Evision.DNN.TextDetectionModelDB.detectTextRectangles(self, image)
  end

  def visualize(image, detections, confidences) do
    text_color = {0, 0, 255}
    Enum.reduce(Enum.zip(detections, confidences), image, fn {rotation_box, conf}, img ->
      points = Evision.Mat.as_type(Evision.boxPoints(rotation_box), :s32)
      [b0, b1 | _] = Nx.to_flat_list(Evision.Mat.to_nx(points, Nx.BinaryBackend))
      Evision.polylines(img, [points], true, {0, 255, 0}, thickness: 2)
      |> Evision.putText("#{conf}", {b0, b1 + 12}, Evision.cv_FONT_HERSHEY_DUPLEX(), 0.3, text_color)
    end)
  end

  def model_info(:resnet18) do
    {
      "https://github.com/opencv/opencv_zoo/blob/master/models/text_detection_db/text_detection_DB_IC15_resnet18_2021sep.onnx?raw=true",
      "text_detection_DB_IC15_resnet18_2021sep.onnx"
    }
  end

  def model_info(:resnet50) do
    {
      # source: https://docs.opencv.org/4.x/d4/d43/tutorial_dnn_text_spotting.html
      "https://drive.google.com/uc?export=download&id=17_ABp79PlFt9yPCxSaarVc_DKTmrSGGf",
      "text_detection_DB_IC15_resnet50.onnx"
    }
  end

  def smartcell_tasks do
    [
      %{
        id: "db_ic15_resnet18",
        label: "DB IC15 (ResNet18)",
        docs_url: "https://github.com/opencv/opencv_zoo/tree/master/models/text_detection_db",
        params: smartcell_params()
      },
      %{
        id: "db_ic15_resnet50",
        label: "DB IC15 (ResNet50)",
        docs_url: "https://github.com/opencv/opencv_zoo/tree/master/models/text_detection_db",
        params: smartcell_params()
      },
    ]
  end

  def default_config do
    %{
      backend: Evision.cv_DNN_BACKEND_OPENCV(),
      target: Evision.cv_DNN_TARGET_CPU(),
      width: 736,
      height: 736,
      binary_threshold: 0.3,
      polygon_threshold: 0.5,
      max_candidates: 200,
      unclip_ratio: 2.0
    }
  end

  def smartcell_params() do
    config = default_config()
    [
      %{field: "width", label: "Width", type: :number, default: config[:width], tooltip: "Should be a multiple of 32."},
      %{field: "height", label: "Height", type: :number, default: config[:height], tooltip: "Should be a multiple of 32."},
      %{field: "binary_threshold", label: "Binary Threshold", type: :number, default: config[:binary_threshold]},
      %{field: "polygon_threshold", label: "Polygon Threshold", type: :number, default: config[:polygon_threshold]},
      %{field: "max_candidates", label: "Max Candidates", type: :number, default: config[:max_candidates]},
      %{field: "unclip_ratio", label: "Unclip Ratio", type: :number, default: config[:unclip_ratio]}
    ]
  end

  def to_quoted(attrs) do
    {backend, target} = Evision.Zoo.to_quoted_backend_and_target(attrs)

    opts = [
      backend: backend,
      target: target
    ]
    {width, height} = {attrs["width"], attrs["height"]}

    model =
      case attrs["variant_id"] do
        "db_ic15_resnet18" ->
          :resnet18
        "db_ic15_resnet50" ->
          :resnet50
        unknown_id ->
          raise "Unknown variant: #{inspect(unknown_id)}"
      end

    [
      quote do
        model = Evision.Zoo.TextDetection.DB_IC15.init(unquote(model), unquote(opts))
      end,
      quote do
        image_input = Kino.Input.image("Image")
        form = Kino.Control.form([image: image_input], submit: "Run")

        frame = Kino.Frame.new()

        form
        |> Kino.Control.stream()
        |> Stream.filter(& &1.data.image)
        |> Kino.listen(fn %{data: %{image: image}} ->
          Kino.Frame.render(frame, Kino.Markdown.new("Running..."))

          {original_height, original_width} = {image.height, image.width}
          image = Evision.Mat.from_binary(image.data, {:u, 8}, image.height, image.width, 3)
          image_ = Evision.resize(image, unquote({width, height}))
          {detections, confidences} = Evision.Zoo.TextDetection.DB_IC15.infer(model, image_)

          image_ = Evision.cvtColor(image_, Evision.cv_COLOR_RGB2BGR())
          vis_img = Evision.Zoo.TextDetection.DB_IC15.visualize(image_, detections, confidences)
          vis_img = Evision.resize(vis_img, {original_width, original_height})

          Kino.Frame.render(frame, Kino.Image.new(Evision.imencode(".png", vis_img), :png))
        end)

        Kino.Layout.grid([form, frame], boxed: true, gap: 16)
      end
    ]
  end
end
