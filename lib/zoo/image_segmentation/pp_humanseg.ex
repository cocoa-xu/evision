defmodule Evision.Zoo.ImageSegmentation.PPHumanSeg do
  @moduledoc """
  PP-Human Segmentation model.
  """

  @doc """
  Default configuration.
  """
  @spec default_config :: map()
  def default_config do
    %{
      backend: Evision.Constant.cv_DNN_BACKEND_OPENCV(),
      target: Evision.Constant.cv_DNN_TARGET_CPU(),
    }
  end

  @doc """
  Customizable parameters from smart cell.
  """
  @spec smartcell_params() :: Evision.Zoo.smartcell_params()
  def smartcell_params() do
    []
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
  def init(model, opts \\ [])

  def init(model_type, opts) when model_type in [:default_model, :quant_model] do
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

    net = Evision.DNN.readNet(model_path)
    Evision.DNN.Net.setPreferableBackend(net, backend)
    Evision.DNN.Net.setPreferableTarget(net, target)
    net
  end

  @doc """
  Inference.

  ##### Positional arguments
  - **self**: `Evision.DNN.Net.t()`.

    An initialized PPHumanSeg model.

  - **image**: `Evision.Mat.maybe_mat_in()`.

    Input image.
  """
  @spec infer(Evision.DNN.Net.t(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t()
  def infer(self=%Evision.DNN.Net{}, image) do
    inputBlob = preprocess(image)
    Evision.DNN.Net.setInput(self, inputBlob)
    outputBlob = Evision.DNN.Net.forward(self, outputName: "save_infer_model/scale_0.tmp_1")
    outputBlob =
      if is_list(outputBlob) do
        [outputBlob] = outputBlob
        outputBlob
      else
        outputBlob
      end
    # todo: use Evision.Backend when Nx.slice is implemented
    result = Evision.Mat.to_nx(Evision.Mat.squeeze(outputBlob), Nx.BinaryBackend)
    Evision.Mat.from_nx(Nx.as_type(Nx.argmax(result, axis: 0), :u8))
  end

  @doc """
  Preprocessing the input image.

  `infer/2` will call this function automatically.

  ##### Positional arguments
  - **image**: `Evision.Mat.maybe_mat_in()`.

    Input image.
  """
  @spec preprocess(Evision.Mat.maybe_mat_in()) :: Evision.Mat.t()
  def preprocess(image) do
    image
    |> Evision.Mat.as_type(:f32)
    |> Evision.resize({192, 192})
    |> Evision.Mat.to_nx()
    |> Nx.divide(Nx.broadcast(Nx.tensor(255.0, backend: Evision.Backend), {192, 192, 3}))
    |> Nx.subtract(mean())
    |> Nx.divide(Nx.broadcast(std(), {192, 192, 3}))
    |> Evision.Mat.from_nx_2d()
    |> Evision.DNN.blobFromImage()
  end

  defp mean do
    Evision.Mat.to_nx(Evision.Mat.literal([[[0.5, 0.5, 0.5]]], :f32))
  end

  defp std do
    Evision.Mat.to_nx(Evision.Mat.literal([[[0.5, 0.5, 0.5]]], :f32))
  end

  @doc """
  Visualize the result.

  ##### Positional arguments
  - **image**: `Evision.Mat.maybe_mat_in()`.

    Original image.

  - **results**: `Evision.Mat.maybe_mat_in()`.

    Results given by `infer/2`.

  ##### Keyword arguments
  - **weight**: `number()`.

    A number in `[0.0, 1.0]`. Defaults to `0.6`.

    Specify the weight of the original image. The weight of the segmentation visualization image
    will be `1 - weight`.

  ##### Return
  A list that contains two images (`Evision.Mat.t()`).

  - The first one is the original image with the segmentation overlay.
  - The second one is the segmentation image.
  """
  @spec visualize(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Keyword.t()) :: list(Evision.Mat.t())
  def visualize(image, results, opts \\ []) do
    weight = opts[:weight] || 0.6

    color_map = Evision.Mat.literal(Evision.Zoo.ImageSegmentation.color_map(256), :u8)
    c1 = Evision.lut(results, color_map[[:all, 0]])
    c2 = Evision.lut(results, color_map[[:all, 1]])
    c3 = Evision.lut(results, color_map[[:all, 2]])
    segmentation = Evision.merge([c1, c2, c3])

    [Evision.addWeighted(image, weight, segmentation, 1 - weight, 0), segmentation]
  end

  @doc """
  Model URL and filename of predefined model.
  """
  @spec model_info(:default_model | :quant_model) :: {String.t(), String.t()}
  def model_info(:default_model) do
    {
      "https://github.com/opencv/opencv_zoo/raw/fd2da740ebc6d0fe489d86cb55133148978cd82e/models/human_segmentation_pphumanseg/human_segmentation_pphumanseg_2023mar.onnx",
      "human_segmentation_pphumanseg_2023mar.onnx"
    }
  end

  def model_info(:quant_model) do
    {
      "https://github.com/opencv/opencv_zoo/raw/fd2da740ebc6d0fe489d86cb55133148978cd82e/models/human_segmentation_pphumanseg/human_segmentation_pphumanseg_2023mar_int8.onnx",
      "human_segmentation_pphumanseg_2023mar_int8.onnx"
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
        id: "pp_humanseg",
        label: "PP-HumanSeg",
        docs_url: "https://github.com/opencv/opencv_zoo/tree/master/models/human_segmentation_pphumanseg",
        params: smartcell_params(),
        docs: docs()
      },
      %{
        id: "pp_humanseg_quant",
        label: "PP-HumanSeg (quant)",
        docs_url: "https://github.com/opencv/opencv_zoo/tree/master/models/human_segmentation_pphumanseg",
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

    model =
      case attrs["variant_id"] do
        "pp_humanseg_quant" ->
          :quant_model
        _ ->
          :default_model
      end

    [
      quote do
        model = Evision.Zoo.ImageSegmentation.PPHumanSeg.init(unquote(model), unquote(opts))
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

          results = Evision.Zoo.ImageSegmentation.PPHumanSeg.infer(model, image)
          results = Evision.resize(results, {width, height}, interpolation: Evision.Constant.cv_INTER_NEAREST())

          image = Evision.cvtColor(image, Evision.Constant.cv_COLOR_RGB2BGR())
          vis_imgs = Evision.Zoo.ImageSegmentation.PPHumanSeg.visualize(image, results)
          vis_imgs = Enum.map(vis_imgs, &Kino.Image.new(Evision.imencode(".png", &1), :png))
          Kino.Frame.render(frame, Kino.Layout.grid(vis_imgs, columns: 2))
        end)

        Kino.Layout.grid([form, frame], boxed: true, gap: 16)
      end
    ]
  end
end
