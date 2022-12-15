defmodule Evision.Zoo.TextRecognition.CRNN do
  @spec init(binary | :en | :ch | :cn, nil | Keyword.t()) :: {:error, String.t()} | Evision.DNN.Net.t()
  def init(model_path, opts \\ [])

  def init(model_type, opts) when model_type in [:en, :ch, :cn] do
    {model_url, filename} = model_info(model_type)
    {charset_url, charset_filename} = charset_info(model_type)
    cache_dir = opts[:cache_dir]
    with {:ok, local_path} <- Evision.Zoo.download(model_url, filename, cache_dir: cache_dir),
         {:ok, _} <- Evision.Zoo.download(charset_url, charset_filename, cache_dir: cache_dir) do
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

    model = Evision.DNN.readNet(model_path)
    Evision.DNN.Net.setPreferableBackend(model, backend)
    Evision.DNN.Net.setPreferableTarget(model, target)
    model
  end

  def model_info(:en) do
    {
      "https://github.com/opencv/opencv_zoo/blob/master/models/text_recognition_crnn/text_recognition_CRNN_EN_2021sep.onnx?raw=true",
      "text_recognition_CRNN_EN_2021sep.onnx"
    }
  end

  def model_info(:en_fp16) do
    {
      "https://github.com/opencv/opencv_zoo/blob/master/models/text_recognition_crnn/text_recognition_CRNN_EN_2022oct_fp16.onnx?raw=true",
      "text_recognition_CRNN_EN_2022oct_fp16.onnx"
    }
  end

  def model_info(:en_int8) do
    {
      "https://github.com/opencv/opencv_zoo/blob/master/models/text_recognition_crnn/text_recognition_CRNN_EN_2022oct_int8.onnx?raw=true",
      "text_recognition_CRNN_EN_2022oct_int8.onnx"
    }
  end

  def model_info(:ch) do
    {
      "https://github.com/opencv/opencv_zoo/blob/master/models/text_recognition_crnn/text_recognition_CRNN_CH_2021sep.onnx?raw=true",
      "text_recognition_CRNN_CH_2021sep.onnx"
    }
  end

  def model_info(:ch_fp16) do
    {
      "https://github.com/opencv/opencv_zoo/blob/master/models/text_recognition_crnn/text_recognition_CRNN_CH_2022oct_fp16.onnx?raw=true",
      "text_recognition_CRNN_CH_2022oct_fp16.onnx"
    }
  end

  def model_info(:ch_int8) do
    {
      "https://github.com/opencv/opencv_zoo/blob/master/models/text_recognition_crnn/text_recognition_CRNN_CH_2022oct_int8.onnx?raw=true",
      "text_recognition_CRNN_CH_2022oct_int8.onnx"
    }
  end

  def model_info(:cn) do
    {
      "https://github.com/opencv/opencv_zoo/blob/master/models/text_recognition_crnn/text_recognition_CRNN_CN_2021nov.onnx?raw=true",
      "text_recognition_CRNN_CN_2021nov.onnx"
    }
  end

  def model_info(:cn_int8) do
    {
      "https://github.com/opencv/opencv_zoo/blob/master/models/text_recognition_crnn/text_recognition_CRNN_CN_2021nov-act_int8-wt_int8-quantized.onnx?raw=true",
      "text_recognition_CRNN_CN_2021nov-act_int8-wt_int8-quantized.onnx"
    }
  end

  def charset_info(:en) do
    {
      "https://raw.githubusercontent.com/opencv/opencv_zoo/master/models/text_recognition_crnn/charset_36_EN.txt",
      "text_recognition_CRNN_EN_2021sep_charset_36_EN.txt"
    }
  end

  def charset_info(:ch) do
    {
      "https://raw.githubusercontent.com/opencv/opencv_zoo/master/models/text_recognition_crnn/charset_94_CH.txt",
      "text_recognition_CRNN_CH_2021sep_charset_94_CH.txt"
    }
  end

  def charset_info(:cn) do
    {
      "https://raw.githubusercontent.com/opencv/opencv_zoo/master/models/text_recognition_crnn/charset_3944_CN.txt",
      "text_recognition_CRNN_CN_2021nov_charset_3944_CN.txt"
    }
  end

  def input_size do
    {100, 32}
  end

  def target_vertices do
    {input_width, input_height} = input_size()
    Nx.tensor(
      [
        [0, input_height - 1],
        [0, 0],
        [input_width - 1, 0],
        [input_width - 1, input_height - 1]
      ],
      type: :f32, backend: Nx.BinaryBackend
    )
  end

  def get_charset(model_type, opts \\ []) do
    {charset_url, charset_filename} = charset_info(model_type)
    cache_dir = opts[:cache_dir]
    with {:ok, local_path} <- Evision.Zoo.download(charset_url, charset_filename, cache_dir: cache_dir),
         {:ok, content} <- File.read(local_path) do
      String.split(content, "\n")
    else
      _ ->
        raise "Cannot load charset: #{inspect(model_type)}, url=#{charset_url}"
    end
  end

  def preprocess(image, rotation_box, to_gray) do
    vertices = Evision.Mat.as_type(Evision.Mat.as_type(Evision.boxPoints(rotation_box), :s32), :f32)
    rotationMatrix = Evision.getPerspectiveTransform(vertices, target_vertices())
    cropped = Evision.warpPerspective(image, rotationMatrix, input_size())
    input =
      if to_gray do
        Evision.cvtColor(cropped, Evision.cv_COLOR_RGB2GRAY())
      else
        cropped
      end

    Evision.DNN.blobFromImage(input, size: input_size(), mean: 127.5, scalefactor: 1.0/127.5)
  end

  @spec infer(Evision.DNN.Net.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Keyword.t()) :: any()
  def infer(self=%Evision.DNN.Net{}, image, rotation_box, opts \\ []) do
    # Preprocess
    to_gray = opts[:to_gray] || false
    inputBlob = preprocess(image, rotation_box, to_gray)

    # Forward
    Evision.DNN.Net.setInput(self, inputBlob)
    outputBlob = Evision.DNN.Net.forward(self, outputName: "")

    # Postprocess
    charset = opts[:charset]
    postprocess(outputBlob, charset)
  end

  @spec postprocess(Evision.Mat.t(), :ch | :cn | :en | list(String.t())) :: bitstring
  def postprocess(outputBlob=%Evision.Mat{}, charset) do
    output = Evision.Mat.to_nx(outputBlob, Nx.BinaryBackend)
    charset =
      case charset do
        charset when charset in [:ch, :cn, :en] ->
          get_charset(charset)
        charset when is_list(charset) ->
          charset
        _ ->
          raise "Invalid charset: #{inspect(charset)}"
      end

    num_chars = elem(outputBlob.shape, 0)
    chars =
      for i <- 0..num_chars-1, into: [] do
        c = Nx.to_number(Nx.argmax(output[[i, 0]]))
        case c do
          0 ->
            "-"
          _ ->
            Enum.at(charset, c - 1)
        end
      end

    for i <- 0..Enum.count(chars)-1, into: <<>> do
      c = Enum.at(chars, i)
      case c do
        "-" ->
          ""
        _ ->
          if not(i > 0 and Enum.at(chars, i + 1, "-") == c) do
            c
          end
      end
    end
  end

  def postprocess(_) do
    ""
  end

  def visualize(image, texts, detections, confidences) do
    text_color = {0, 0, 255}
    Enum.reduce(Enum.zip([texts, detections, confidences]), image, fn {text, pts, conf}, img ->
      [b0, b1 | _] = Nx.to_flat_list(pts)
      conf = Float.round(conf, 2)
      Evision.polylines(img, [pts], true, {0, 255, 0}, thickness: 2)
      |> Evision.putText("#{conf}: #{text}", {b0, b1 + 12}, Evision.cv_FONT_HERSHEY_DUPLEX(), 1.0, text_color)
    end)
  end

  def smartcell_tasks do
    [
      %{
        id: "crnn_en",
        label: "CRNN EN",
        docs_url: "https://github.com/opencv/opencv_zoo/tree/master/models/text_recognition_crnn",
        params: smartcell_params()
      },
      %{
        id: "crnn_en_fp16",
        label: "CRNN EN (FP16)",
        docs_url: "https://github.com/opencv/opencv_zoo/tree/master/models/text_recognition_crnn",
        params: smartcell_params()
      },
      %{
        id: "crnn_en_int8",
        label: "CRNN EN (INT8)",
        docs_url: "https://github.com/opencv/opencv_zoo/tree/master/models/text_recognition_crnn",
        params: smartcell_params()
      },
      %{
        id: "crnn_ch",
        label: "CRNN CH",
        docs_url: "https://github.com/opencv/opencv_zoo/tree/master/models/text_recognition_crnn",
        params: smartcell_params()
      },
      %{
        id: "crnn_ch_fp16",
        label: "CRNN CH (FP16)",
        docs_url: "https://github.com/opencv/opencv_zoo/tree/master/models/text_recognition_crnn",
        params: smartcell_params()
      },
      %{
        id: "crnn_ch_int8",
        label: "CRNN CH (INT8)",
        docs_url: "https://github.com/opencv/opencv_zoo/tree/master/models/text_recognition_crnn",
        params: smartcell_params()
      },
      %{
        id: "crnn_cn",
        label: "CRNN CN",
        docs_url: "https://github.com/opencv/opencv_zoo/tree/master/models/text_recognition_crnn",
        params: smartcell_params()
      },
      %{
        id: "crnn_cn_int8",
        label: "CRNN CN (INT8)",
        docs_url: "https://github.com/opencv/opencv_zoo/tree/master/models/text_recognition_crnn",
        params: smartcell_params()
      },
    ]
  end

  def default_config do
    %{
      backend: Evision.cv_DNN_BACKEND_OPENCV(),
      target: Evision.cv_DNN_TARGET_CPU(),
      detector: "db_ic15_resnet18"
    }
  end

  def smartcell_params() do
    config = default_config()
    [
      %{
        name: "Recognizer",
        params: [
          %{field: "detector", label: "Text Detector", type: :string, default: config[:detector],
            is_option: true,
            options: [
              %{value: "db_ic15_resnet18", label: "DB IC15 ResNet18"},
              %{value: "db_ic15_resnet50", label: "DB IC15 ResNet50"},
              %{value: "db_td500_resnet18", label: "DB TD500 ResNet18"},
              %{value: "db_td500_resnet50", label: "DB TD500 ResNet50"},
            ]
          },
        ]
      }
    ] ++ Evision.Zoo.TextDetection.DB.smartcell_params()
  end

  def to_quoted(attrs) do
    {backend, target} = Evision.Zoo.to_quoted_backend_and_target(attrs)

    opts = [
      backend: backend,
      target: target
    ]
    {detector_width, detector_height} = {attrs["width"], attrs["height"]}

    {model, to_gray?, charset} =
      case attrs["variant_id"] do
        "crnn_" <> variant_id ->
          case variant_id do
            "en" <> _ ->
              {String.to_atom(variant_id), true, :en}
            "ch" <> _ ->
              {String.to_atom(variant_id), false, :ch}
            "cn" <> _ ->
              {String.to_atom(variant_id), false, :cn}
            _ ->
              raise "Unknown variant: crnn_#{inspect(variant_id)}"
          end
        unknown_id ->
          raise "Unknown variant: #{inspect(unknown_id)}"
      end

    {detector_module, detector_model} =
      case attrs["detector"] do
        "db_" <> db_detector ->
          case db_detector do
            db_detector when db_detector in ["ic15_resnet18", "ic15_resnet50", "td500_resnet18", "td500_resnet50"] ->
              {Evision.Zoo.TextDetection.DB, String.to_atom(db_detector)}
            _ ->
              raise "Unknown DB variant: #{inspect(db_detector)}"
          end
        unknown_id ->
          raise "Unknown text detector: #{inspect(unknown_id)}"
      end

    [
      quote do
        detector = unquote(detector_module).init(unquote(detector_model), unquote(opts))
        recognizer = Evision.Zoo.TextRecognition.CRNN.init(unquote(model), unquote(opts))
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
          scale_height = original_height / unquote(detector_height)
          scale_width = original_width / unquote(detector_width)
          image = Evision.Mat.from_binary(image.data, {:u, 8}, image.height, image.width, 3)
          image_ = Evision.resize(image, unquote({detector_width, detector_height}))

          charset = Evision.Zoo.TextRecognition.CRNN.get_charset(unquote(charset))
          {detections, confidences} = unquote(detector_module).infer(detector, image_)
          texts = Enum.map(detections, &Evision.Zoo.TextRecognition.CRNN.infer(
            recognizer, image_, &1, to_gray: unquote(to_gray?), charset: charset))

          detections = Enum.map(detections, fn d ->
            Nx.multiply(
              Evision.Mat.to_nx(Evision.boxPoints(d), Nx.BinaryBackend),
              Nx.tensor([scale_width, scale_height], backend: Nx.BinaryBackend)
            )
            |> Nx.as_type(:s32)
          end)

          image = Evision.cvtColor(image, Evision.cv_COLOR_RGB2BGR())
          vis_img = Evision.Zoo.TextRecognition.CRNN.visualize(image, texts, detections, confidences)

          Kino.Frame.render(frame, Kino.Image.new(Evision.imencode(".png", vis_img), :png))
        end)

        Kino.Layout.grid([form, frame], boxed: true, gap: 16)
      end
    ]
  end
end
