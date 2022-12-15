defmodule Evision.Zoo.ImageClassification.PPResNet do
  @spec init(binary | :default_model | :quant_model, nil | Keyword.t()) :: {:error, String.t()} | Evision.DNN.Net.t()
  def init(model_path, opts \\ [])

  def init(model_type, opts) when model_type in [:default_model, :quant_model] do
    {model_url, filename} = model_info(model_type)
    {labels_url, labels_filename} = labels()
    cache_dir = opts[:cache_dir]
    with {:ok, local_path} <- Evision.Zoo.download(model_url, filename, cache_dir: cache_dir),
         {:ok, _labels_path} <- Evision.Zoo.download(labels_url, labels_filename, cache_dir: cache_dir) do
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

    net = Evision.DNN.readNet(model_path)
    Evision.DNN.Net.setPreferableBackend(net, backend)
    Evision.DNN.Net.setPreferableTarget(net, target)
    net
  end

  @spec infer(Evision.DNN.Net.t(), Evision.Mat.maybe_mat_in(), Keyword.t()) :: any()
  def infer(self=%Evision.DNN.Net{}, image, opts \\ []) do
    top_k = opts[:top_k] || 5
    inputBlob = preprocess(image)
    Evision.DNN.Net.setInput(self, inputBlob)
    outputBlob = Evision.DNN.Net.forward(self, outputName: "save_infer_model/scale_0.tmp_0")
    # todo: use Evision.Backend when Nx.slice is implemented
    result = Nx.squeeze(Evision.Mat.to_nx(outputBlob, Nx.BinaryBackend))
    Nx.to_flat_list(Nx.argsort(result, direction: :desc)[[0..top_k-1]])
  end

  def get_labels(opts \\ []) do
    {labels_url, labels_filename} = labels()
    cache_dir = opts[:cache_dir]
    with {:ok, labels_path} <- Evision.Zoo.download(labels_url, labels_filename, cache_dir: cache_dir),
         {:ok, content} <- File.read(labels_path) do
      String.split(content, "\n")
    else
      {:error, msg} ->
        IO.puts("Cannot load label file: #{inspect(msg)}")
        []
    end
  end

  def preprocess(image) do
    image
    |> Evision.Mat.as_type(:f32)
    |> Evision.resize({224, 224})
    |> Evision.Mat.to_nx()
    |> Nx.divide(Nx.broadcast(Nx.tensor(255.0, backend: Evision.Backend), {224, 224, 3}))
    |> Nx.subtract(mean())
    |> Nx.divide(Nx.broadcast(std(), {224, 224, 3}))
    |> Evision.Mat.from_nx_2d()
    |> Evision.DNN.blobFromImage()
  end

  defp mean do
    Evision.Mat.to_nx(Evision.Mat.literal([[[0.485, 0.456, 0.406]]], :f32))
  end

  defp std do
    Evision.Mat.to_nx(Evision.Mat.literal([[[0.229, 0.224, 0.225]]], :f32))
  end

  def visualize(image, results, opts \\ []) do
    labels = get_labels(opts)
    top_classes = Enum.map(results, &Enum.at(labels, &1))
    {image, top_classes}
  end

  def default_config do
    %{
      backend: Evision.cv_DNN_BACKEND_OPENCV(),
      target: Evision.cv_DNN_TARGET_CPU(),
      top_k: 5
    }
  end

  def model_info(:default_model) do
    {
      "https://github.com/opencv/opencv_zoo/blob/master/models/image_classification_ppresnet/image_classification_ppresnet50_2022jan.onnx?raw=true",
      "image_classification_ppresnet50_2022jan.onnx"
    }
  end

  def model_info(:quant_model) do
    {
      "https://github.com/opencv/opencv_zoo/blob/master/models/image_classification_ppresnet/image_classification_ppresnet50_2022jan-act_int8-wt_int8-quantized.onnx?raw=true",
      "image_classification_ppresnet50_2022jan-act_int8-wt_int8-quantized.onnx"
    }
  end

  def labels do
    {
      "https://raw.githubusercontent.com/opencv/opencv_zoo/master/models/image_classification_ppresnet/imagenet_labels.txt",
      "image_classification_ppresnet50_imagenet_labels.txt"
    }
  end

  def smartcell_tasks do
    [
      %{
        id: "pp_resnet",
        label: "PP-ResNet",
        docs_url: "https://github.com/opencv/opencv_zoo/tree/master/models/image_classification_ppresnet",
        params: smartcell_params()
      },
      %{
        id: "pp_resnet_quant",
        label: "PP-ResNet (quant)",
        docs_url: "https://github.com/opencv/opencv_zoo/tree/master/models/image_classification_ppresnet",
        params: smartcell_params()
      },
    ]
  end

  def smartcell_params() do
    config = default_config()
    [
      %{
        name: "Image Classifier",
        params: [
          %{field: "top_k", label: "Top-k", type: :number, default: config[:top_k]},
        ]
      }
    ]
  end

  def to_quoted(attrs) do
    {backend, target} = Evision.Zoo.to_quoted_backend_and_target(attrs)

    opts = [
      backend: backend,
      target: target
    ]
    top_k = attrs["top_k"]

    model =
      case attrs["variant_id"] do
        "pp_resnet_quant" ->
          :quant_model
        _ ->
          :default_model
      end

    [
      quote do
        model = Evision.Zoo.ImageClassification.PPResNet.init(unquote(model), unquote(opts))
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

          {height, width} = {image.height, image.width}
          image = Evision.Mat.from_binary(image.data, {:u, 8}, height, width, 3)
          image_ = Evision.resize(image, {256, 256})[[16..239, 16..239]]
          results = Evision.Zoo.ImageClassification.PPResNet.infer(model, image_, top_k: unquote(top_k))


          image = Evision.cvtColor(image, Evision.cv_COLOR_RGB2BGR())
          {vis_img, top_classes} = Evision.Zoo.ImageClassification.PPResNet.visualize(image, results)

          Kino.Frame.render(frame, Kino.Image.new(Evision.imencode(".png", vis_img), :png))
          Kino.Frame.append(frame, Evision.SmartCell.SimpleList.new(top_classes))
        end)

        Kino.Layout.grid([form, frame], boxed: true, gap: 16)
      end
    ]
  end
end
