defmodule Evision.Zoo.FaceDetection.YuNet do
  @spec init(binary | :default_model | :quant_model, {pos_integer(), pos_integer()}, nil | Keyword.t()) :: {:error, String.t()} | Evision.FaceDetectorYN.t()
  def init(model_path, input_size, opts \\ [])

  def init(model_type, input_size, opts) when model_type in [:default_model, :quant_model] do
    {model_url, filename} = model_info(model_type)
    cache_dir = opts[:cache_dir]
    with {:ok, local_path} <- Evision.Zoo.download(model_url, filename, cache_dir: cache_dir) do
      init(local_path, input_size, opts)
    else
      {:error, msg} ->
        raise msg
    end
  end

  def init(model_path, input_size, opts) when is_binary(model_path) do
    config = default_config()
    conf_threshold = opts[:conf_threshold] || config[:conf_threshold]
    nms_threshold = opts[:nms_threshold] || config[:nms_threshold]
    top_k = opts[:top_k] || config[:top_k]
    backend = opts[:backend] || config[:backend]
    target = opts[:target] || config[:target]

    Evision.FaceDetectorYN.create(
      model_path, "", input_size,
      score_threshold: conf_threshold,
      nms_threshold: nms_threshold,
      top_k: top_k,
      backend_id: backend,
      target_id: target
    )
  end

  @spec setInputSize(Evision.FaceDetectorYN.t(), {pos_integer, pos_integer}) :: :ok | {:error, String.t()}
  def setInputSize(self=%Evision.FaceDetectorYN{}, {w, h}) when is_integer(w) and is_integer(h) and w > 0 and h > 0 do
    Evision.FaceDetectorYN.setInputSize(self, {w, h})
  end

  @spec infer(Evision.FaceDetectorYN.t(), Evision.Mat.maybe_mat_in()) :: any()
  def infer(self=%Evision.FaceDetectorYN{}, image) do
    {w, h} = case image.shape do
      {h, w, _} -> {w, h}
      {h, w} -> {w, h}
      invalid -> raise "Invalid image shape #{inspect(invalid)}"
    end
    setInputSize(self, {w, h})
    {_, points} = Evision.FaceDetectorYN.detect(self, image)
    points
  end

  def visualize(image, results, opts \\ [])
  def visualize(image, {:error, "empty matrix"}, _opts) do
    image
  end

  def visualize(image, results, opts) do
    box_color = opts[:box_color] || {0, 255, 0}
    text_color = opts[:text_color] || {0, 0, 255}

    landmark_color = [
      {255,   0,   0}, # right eye
      {  0,   0, 255}, # left eye
      {  0, 255,   0}, # nose tip
      {255,   0, 255}, # right mouth corner
      {  0, 255, 255}  # left mouth corner
    ]

    case results.shape do
      {num_faces, 15} when num_faces > 0 ->
        results = Evision.Mat.to_nx(results, Nx.BinaryBackend)
        for i <- 0..num_faces-1, reduce: image do
          image ->
            det = results[i]
            [b0, b1, b2, b3] = Nx.to_flat_list(Nx.as_type(det[0..3], :s32))
            conf = Float.round(Nx.to_number(det[-1]), 4)

            image =
              Evision.rectangle(image, {b0, b1}, {b0+b2, b1+b3}, box_color, thickness: 2)
              |> Evision.putText("#{conf}", {b0, b1+12}, Evision.cv_FONT_HERSHEY_DUPLEX(), 0.5, text_color)

            landmarks = Nx.reshape(Nx.as_type(det[4..13], :s32), {5, 2})
            for idx <- 0..4, reduce: image do
              image ->
                landmark = List.to_tuple(Nx.to_flat_list(landmarks[idx]))
                Evision.circle(image, landmark, 2, Enum.at(landmark_color, idx), thickness: 2)
            end
        end
      _ ->
        image
    end
  end

  def default_config do
    %{
      backend: Evision.cv_DNN_BACKEND_OPENCV(),
      target: Evision.cv_DNN_TARGET_CPU(),
      conf_threshold: 0.9,
      nms_threshold: 0.3,
      top_k: 5000
    }
  end

  def model_info(:default_model) do
    {
      "https://github.com/opencv/opencv_zoo/blob/cd1ac4e61dc51575cac38d6346494865d0dfa5ba/models/face_detection_yunet/face_detection_yunet_2022mar.onnx?raw=true",
      "face_detection_yunet_2022mar.onnx"
    }
  end

  def model_info(:quant_model) do
    {
      "https://github.com/opencv/opencv_zoo/blob/cd1ac4e61dc51575cac38d6346494865d0dfa5ba/models/face_detection_yunet/face_detection_yunet_2022mar-act_int8-wt_int8-quantized.onnx?raw=true",
      "face_detection_yunet_2022mar-act_int8-wt_int8-quantized.onnx"
    }
  end

  def smartcell_tasks do
    [
      %{
        id: "yunet",
        label: "YuNet",
        docs_url: "https://github.com/ShiqiYu/libfacedetection",
        params: smartcell_params()
      },
      %{
        id: "yunet_quant",
        label: "YuNet (quant)",
        docs_url: "https://github.com/ShiqiYu/libfacedetection",
        params: smartcell_params()
      },
    ]
  end

  def smartcell_params() do
    config = default_config()
    [
      %{field: "top_k", label: "Top-k", type: :number, default: config[:top_k]},
      %{field: "nms_threshold", label: "NMS Threshold", type: :number, default: config[:nms_threshold]},
      %{field: "conf_threshold", label: "Confidence", type: :number, default: config[:conf_threshold]},
    ]
  end

  def to_quoted(attrs) do
    {backend, target} = Evision.Zoo.to_quoted_backend_and_target(attrs)

    opts = [
      top_k: attrs["top_k"],
      nms_threshold: attrs["nms_threshold"],
      conf_threshold: attrs["conf_threshold"],
      backend: backend,
      target: target
    ]

    model =
      case attrs["variant_id"] do
        "yunet_quant" ->
          :quant_model
        _ ->
          :default_model
      end

    [
      quote do
        model = Evision.Zoo.FaceDetection.YuNet.init(unquote(model), {320, 320}, unquote(opts))
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

          image = Evision.Mat.from_binary(image.data, {:u, 8}, image.height, image.width, 3)
          results = Evision.Zoo.FaceDetection.YuNet.infer(model, image)

          image = Evision.cvtColor(image, Evision.cv_COLOR_RGB2BGR())
          Evision.Zoo.FaceDetection.YuNet.visualize(image, results)
          |> then(&Kino.Frame.render(frame, Kino.Image.new(Evision.imencode(".png", &1), :png)))
        end)

        Kino.Layout.grid([form, frame], boxed: true, gap: 16)
      end
    ]
  end
end
