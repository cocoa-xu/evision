defmodule Evision.Zoo.ImageClassification.MobileNetV2 do
  @moduledoc """
  MobileNets: Efficient Convolutional Neural Networks for Mobile Vision Applications
  """

  @doc """
  Default configuration.
  """
  @spec default_config :: map()
  def default_config do
    %{
      backend: Evision.Constant.cv_DNN_BACKEND_OPENCV(),
      target: Evision.Constant.cv_DNN_TARGET_CPU(),
      top_k: 5
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
        name: "Image Classifier",
        params: [
          %{field: "top_k", label: "Top-k", type: :number, default: config[:top_k]},
        ]
      }
    ]
  end

  @doc """
  Initialize model.

  ##### Positional arguments
  - **model**: `String.t()` | `:default_model` | `:quant_model`.

    - When `model` is a string, it will be treat as the path to a weight file
      and `init/2` will load the model from it.

    - When `model` is either `:default_model` or `:quant_model`, `init/2` will
      download and load the predefined model.

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

  @doc """
  Inference.

  ##### Positional arguments
  - **self**: `Evision.DNN.Net.t()`.

    An initialized MobileNetV2 model.

  - **image**: `Evision.Mat.maybe_mat_in()`.

    Input image.

  ##### Keyword arguments
  - **top_k**: `pos_integer()`.

    Get top k results.

    Optional. Defaults to `5`.
  """
  @spec infer(Evision.DNN.Net.t(), Evision.Mat.maybe_mat_in(), Keyword.t()) :: list(number())
  def infer(self=%Evision.DNN.Net{}, image, opts \\ []) do
    top_k = opts[:top_k] || 5
    inputBlob = preprocess(image)
    Evision.DNN.Net.setInput(self, inputBlob)
    outputBlob = Evision.DNN.Net.forward(self, outputName: "")
    outputBlob =
      if is_list(outputBlob) do
        [outputBlob] = outputBlob
        outputBlob
      else
        outputBlob
      end
    # todo: use Evision.Backend when Nx.slice is implemented
    result = Nx.squeeze(Evision.Mat.to_nx(outputBlob, Nx.BinaryBackend))
    Nx.to_flat_list(Nx.argsort(result, direction: :desc)[[0..top_k-1]])
  end

  @doc """
  Get labels.

  ##### Keyword arguments
  - **labels_path**: `String.t()`.

    Path to the label file. Defaults to `nil`.

    When `labels_path` is `nil`, `get_labels/1` will try to
    download the default label file.

  - **cache_dir**: `String.t()`.

    Path to the cache directory.

    Optional. Defaults to `:filename.basedir(:user_cache, "", ...)`

  ##### Returns
  A list of labels.
  """
  @spec get_labels(Keyword.t()) :: [binary]
  def get_labels(opts \\ []) do
    labels_path = opts[:labels_path]
    labels_path =
      if labels_path == nil do
        cache_dir = opts[:cache_dir]
        {labels_url, labels_filename} = labels()
        with {:ok, labels_path} <- Evision.Zoo.download(labels_url, labels_filename, cache_dir: cache_dir) do
          labels_path
        else
          {:error, msg} ->
            raise "Cannot download label file: #{inspect(msg)}"
        end
      else
        labels_path
      end

    with {:ok, content} <- File.read(labels_path) do
      String.split(content, "\n")
    else
      {:error, msg} ->
        raise "Cannot load label file: #{inspect(msg)}"
    end
  end

  @doc """
  Preprocessing the input image.

  `infer/3` will call this function automatically.

  ##### Positional arguments
  - **image**: `Evision.Mat.maybe_mat_in()`.

    Input image.
  """
  @spec preprocess(Evision.Mat.maybe_mat_in()) :: Evision.Mat.t()
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

  @doc """
  Model URL and filename of predefined model.
  """
  @spec model_info(:default_model | :quant_model) :: {String.t(), String.t()}
  def model_info(:default_model) do
    {
      "https://github.com/opencv/opencv_zoo/raw/fd2da740ebc6d0fe489d86cb55133148978cd82e/models/image_classification_mobilenet/image_classification_mobilenetv2_2022apr.onnx",
      "image_classification_mobilenetv2_2022apr.onnx"
    }
  end

  def model_info(:quant_model) do
    {
      "https://github.com/opencv/opencv_zoo/raw/fd2da740ebc6d0fe489d86cb55133148978cd82e/models/image_classification_mobilenet/image_classification_mobilenetv2_2022apr_int8.onnx",
      "image_classification_mobilenetv2_2022apr_int8.onnx"
    }
  end

  @doc """
  Default label file URL and filename.
  """
  @spec labels :: {String.t(), String.t()}
  def labels do
    {
      "https://raw.githubusercontent.com/opencv/opencv_zoo/ca3e31aa1f1b1330d008598659d386bd06f300b0/models/image_classification_mobilenet/imagenet_labels.txt",
      "image_classification_mobilenetv2_imagenet_labels.txt"
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
        id: "mobilenet_v2",
        label: "MobileNet V2",
        docs_url: "https://github.com/opencv/opencv_zoo/tree/master/models/image_classification_mobilenet",
        params: smartcell_params(),
        docs: docs(),
      },
      %{
        id: "mobilenet_v2_quant",
        label: "MobileNet V2 (quant)",
        docs_url: "https://github.com/opencv/opencv_zoo/tree/master/models/image_classification_mobilenet",
        params: smartcell_params(),
        docs: docs(),
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
    top_k = attrs["top_k"]

    model =
      case attrs["variant_id"] do
        "mobilenet_v2_quant" ->
          :quant_model
        _ ->
          :default_model
      end

    [
      quote do
        model = Evision.Zoo.ImageClassification.MobileNetV2.init(unquote(model), unquote(opts))
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

          image =
            image.file_ref
            |> Kino.Input.file_path()
            |> File.read!()
            |> Evision.Mat.from_binary({:u, 8}, height, width, 3)

          image_ = Evision.resize(image, {256, 256})[[16..239, 16..239]]
          results = Evision.Zoo.ImageClassification.MobileNetV2.infer(model, image_, top_k: unquote(top_k))

          labels = Evision.Zoo.ImageClassification.MobileNetV2.get_labels()
          top_classes = Enum.map(results, &Enum.at(labels, &1))

          Kino.Frame.render(frame, Evision.SmartCell.SimpleList.new(top_classes))
        end)

        Kino.Layout.grid([form, frame], boxed: true, gap: 16)
      end
    ]
  end
end
