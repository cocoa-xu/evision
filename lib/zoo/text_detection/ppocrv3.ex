
defmodule Evision.Zoo.TextDetection.PPOCRV3 do
  @moduledoc """
  Real-time Scene Text Detection with PP-OCRv3
  """

  @doc """
  Default configuration.
  """
  @spec default_config :: map()
  def default_config do
    %{
      backend: Evision.Constant.cv_DNN_BACKEND_OPENCV(),
      target: Evision.Constant.cv_DNN_TARGET_CPU(),
      width: 736,
      height: 736,
      binary_threshold: 0.3,
      polygon_threshold: 0.5,
      max_candidates: 200,
      unclip_ratio: 2.0
    }
  end

  @doc """
  Customizable parameters from smart cell.
  """
  @spec smartcell_params() :: Evision.Zoo.smartcell_params()
  def smartcell_params() do
    config = default_config()
    [
      %{
        name: "Text Detector",
        params: [
          %{field: "width", label: "Width", type: :number, default: config[:width], tooltip: "Multiples of 32."},
          %{field: "height", label: "Height", type: :number, default: config[:height], tooltip: "Multiples of 32."},
          %{field: "binary_threshold", label: "Binary Threshold", type: :float, default: config[:binary_threshold]},
          %{field: "polygon_threshold", label: "Polygon Threshold", type: :float, default: config[:polygon_threshold]},
          %{field: "max_candidates", label: "Max Candidates", type: :number, default: config[:max_candidates]},
          %{field: "unclip_ratio", label: "Unclip Ratio", type: :float, default: config[:unclip_ratio]}
        ]
      }
    ]
  end

  @doc """
  Initialize model.

  ##### Positional arguments
  - **model**: `String.t()` | `:en` | `:cn`

    - When `model` is a string, it will be treat as the path to a weight file
      and `init/2` will load the model from it.

    - When `model` is one of `:en` and `:cn`,
      `init/2` will download and load the predefined model.

  ##### Keyword arguments
  - **cache_dir**: `String.t()`.

    Path to the cache directory.

    Optional. Defaults to `:filename.basedir(:user_cache, "", ...)`

  - **backend**: `integer()`.

    Specify the backend.

    Optional. Defaults to `Evision.Constant.cv_DNN_BACKEND_OPENCV()`.

  - **target**: `integer()`.

    Specify the target.

    Optional. Defaults to `Evision.Constant.cv_DNN_TARGET_CPU()`.
  """
  @spec init(binary | :en | :cn, nil | Keyword.t()) :: {:error, String.t()} | Evision.DNN.TextDetectionModelDB.t()
  def init(model_path, opts \\ [])

  def init(model_type, opts) when model_type in [:en, :cn] do
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

  @doc """
  Inference.

  ##### Positional arguments
  - **self**: `Evision.DNN.TextDetectionModelDB.t()`.

    An initialized `Evision.DNN.TextDetectionModelDB` model.

  - **image**: `Evision.Mat.maybe_mat_in()`.

    Input image.

  ##### Return
  `{detections, confidence}`
  """
  @spec infer(Evision.DNN.TextDetectionModelDB.t(), Evision.Mat.maybe_mat_in()) :: {list({{number(), number()}, {number(), number()}, number()}), list(number())} | {:error, String.t()}
  def infer(self=%Evision.DNN.TextDetectionModelDB{}, image) do
    Evision.DNN.TextDetectionModelDB.detectTextRectangles(self, image)
  end

  @doc """
  Visualize the result.

  ##### Positional arguments
  - **image**: `Evision.Mat.maybe_mat_in()`.

    Original image.

  - **detections**: `list({{number(), number()}, {number(), number()}, number()})`.

    Rotation retangulars.

  - **confidences**: `list(number())`.

    Confidence values.

  ##### Keyword arguments
  - **box_color**: `{blue=integer(), green=integer(), red=integer()}`.

    Values should be in `[0, 255]`. Defaults to `{0, 255, 0}`.

    Specify the color of the bounding box.

  - **text_color**: `{blue=integer(), green=integer(), red=integer()}`.

    Values should be in `[0, 255]`. Defaults to `{0, 0, 255}`.

    Specify the color of the text (confidence value).
  """
  def visualize(image, detections, confidences, opts \\ []) do
    box_color = opts[:box_color] || {0, 255, 0}
    text_color = opts[:text_color] || {0, 0, 255}
    Enum.reduce(Enum.zip(detections, confidences), image, fn {rotation_box, conf}, img ->
      points = Evision.Mat.as_type(Evision.boxPoints(rotation_box), :s32)
      [b0, b1 | _] = Nx.to_flat_list(Evision.Mat.to_nx(points, Nx.BinaryBackend))
      Evision.polylines(img, [points], true, box_color, thickness: 2)
      |> Evision.putText("#{conf}", {b0, b1 + 12}, Evision.Constant.cv_FONT_HERSHEY_DUPLEX(), 0.3, text_color)
    end)
  end

  @doc """
  Model URL and filename of predefined model.
  """
  @spec model_info(:en | :cn | :en_int8 | :cn_int8) :: {String.t(), String.t()}
  def model_info(:en) do
    {
      "https://github.com/opencv/opencv_zoo/raw/fd2da740ebc6d0fe489d86cb55133148978cd82e/models/text_detection_ppocr/text_detection_en_ppocrv3_2023may.onnx",
      "text_detection_en_ppocrv3_2023may.onnx"
    }
  end

  def model_info(:cn) do
    {
      "https://github.com/opencv/opencv_zoo/raw/fd2da740ebc6d0fe489d86cb55133148978cd82e/models/text_detection_ppocr/text_detection_cn_ppocrv3_2023may.onnx",
      "text_detection_cn_ppocrv3_2023may.onnx"
    }
  end

  def model_info(:en_int8) do
    {
      "https://github.com/opencv/opencv_zoo/raw/fd2da740ebc6d0fe489d86cb55133148978cd82e/models/text_detection_ppocr/text_detection_en_ppocrv3_2023may_int8.onnx",
      "text_detection_en_ppocrv3_2023may_int8.onnx"
    }
  end

  def model_info(:cn_int8) do
    {
      "https://github.com/opencv/opencv_zoo/raw/fd2da740ebc6d0fe489d86cb55133148978cd82e/models/text_detection_ppocr/text_detection_cn_ppocrv3_2023may_int8.onnx",
      "text_detection_cn_ppocrv3_2023may_int8.onnx"
    }
  end

  @doc """
  Docs in smart cell.
  """
  @spec docs() :: String.t()
  def docs do
    @moduledoc
  end

  @doc """
  Smart cell tasks.

  A list of variants of the current model.
  """
  @spec smartcell_tasks() :: Evision.Zoo.smartcell_tasks()
  def smartcell_tasks do
    [
      %{
        id: "ppocrv3_en",
        label: "PP-OCR V3 EN",
        docs_url: "https://github.com/opencv/opencv_zoo/tree/master/models/text_detection_ppocr",
        params: smartcell_params(),
        docs: docs()
      },
      %{
        id: "ppocrv3_cn",
        label: "PP-OCR V3 CN",
        docs_url: "https://github.com/opencv/opencv_zoo/tree/master/models/text_detection_ppocr",
        params: smartcell_params(),
        docs: docs()
      },
      %{
        id: "ppocrv3_en_int8",
        label: "PP-OCR V3 EN (int8)",
        docs_url: "https://github.com/opencv/opencv_zoo/tree/master/models/text_detection_ppocr",
        params: smartcell_params(),
        docs: docs()
      },
      %{
        id: "ppocrv3_cn_int8",
        label: "PP-OCR V3 CN (int8)",
        docs_url: "https://github.com/opencv/opencv_zoo/tree/master/models/text_detection_ppocr",
        params: smartcell_params(),
        docs: docs()
      },
    ]
  end

  @doc """
  Generate quoted code from smart cell attrs.
  """
  @spec to_quoted(map()) :: list()
  def to_quoted(attrs) do
    {backend, target} = Evision.Zoo.to_quoted_backend_and_target(attrs)

    opts = [
      backend: backend,
      target: target
    ]
    {width, height} = {attrs["width"], attrs["height"]}

    model =
      case attrs["variant_id"] do
        "ppocrv3_en" ->
          :en
        "ppocrv3_en_int8" ->
          :en_int8
        "ppocrv3_cn" ->
          :cn
        "ppocrv3_cn_int8" ->
          :cn_int8
        unknown_id ->
          raise "Unknown variant: #{inspect(unknown_id)}"
      end

    [
      quote do
        model = Evision.Zoo.TextDetection.DB.init(unquote(model), unquote(opts))
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

          image =
            image.file_ref
            |> Kino.Input.file_path()
            |> File.read!()
            |> Evision.Mat.from_binary({:u, 8}, image.height, image.width, 3)

          image_ = Evision.resize(image, unquote({width, height}))
          {detections, confidences} = Evision.Zoo.TextDetection.DB.infer(model, image_)

          image_ = Evision.cvtColor(image_, Evision.Constant.cv_COLOR_RGB2BGR())
          vis_img = Evision.Zoo.TextDetection.DB.visualize(image_, detections, confidences)
          vis_img = Evision.resize(vis_img, {original_width, original_height})

          Kino.Frame.render(frame, Kino.Image.new(Evision.imencode(".png", vis_img), :png))
        end)

        Kino.Layout.grid([form, frame], boxed: true, gap: 16)
      end
    ]
  end
end
