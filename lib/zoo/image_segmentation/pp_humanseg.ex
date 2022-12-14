defmodule Evision.Zoo.ImageSegmentation.PPHumanSeg do
  @spec init(binary | :default_model | :quant_model, nil | Keyword.t()) :: {:error, String.t()} | Evision.DNN.Net.t()
  def init(model_path, opts \\ [])

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

  @spec infer(Evision.DNN.Net.t(), Evision.Mat.maybe_mat_in()) :: any()
  def infer(self=%Evision.DNN.Net{}, image) do
    inputBlob = preprocess(image)
    Evision.DNN.Net.setInput(self, inputBlob)
    outputBlob = Evision.DNN.Net.forward(self, outputName: "save_infer_model/scale_0.tmp_1")
    # todo: use Evision.Backend when Nx.slice is implemented
    result = Evision.Mat.to_nx(Evision.Mat.squeeze(outputBlob[0]), Nx.BinaryBackend)
    Evision.Mat.from_nx(Nx.as_type(Nx.argmax(result, axis: 0), :u8))
  end

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

  def visualize(image, results, opts \\ []) do
    weight = opts[:weight] || 0.6

    color_map = Evision.Mat.literal(Evision.Zoo.ImageSegmentation.color_map(256), :u8)
    c1 = Evision.lut(results, color_map[[:all, 0]])
    c2 = Evision.lut(results, color_map[[:all, 1]])
    c3 = Evision.lut(results, color_map[[:all, 2]])
    segmentation = Evision.merge([c1, c2, c3])

    [Evision.addWeighted(image, weight, segmentation, 1 - weight, 0), segmentation]
  end

  def default_config do
    %{
      backend: Evision.cv_DNN_BACKEND_OPENCV(),
      target: Evision.cv_DNN_TARGET_CPU(),
    }
  end

  def model_info(:default_model) do
    {
      "https://github.com/opencv/opencv_zoo/blob/master/models/human_segmentation_pphumanseg/human_segmentation_pphumanseg_2021oct.onnx?raw=true",
      "human_segmentation_pphumanseg_2021oct.onnx"
    }
  end

  def model_info(:quant_model) do
    {
      "https://github.com/opencv/opencv_zoo/blob/master/models/human_segmentation_pphumanseg/human_segmentation_pphumanseg_2021oct-act_int8-wt_int8-quantized.onnx?raw=true",
      "human_segmentation_pphumanseg_2021oct-act_int8-wt_int8-quantized.onnx"
    }
  end

  def smartcell_tasks do
    [
      %{
        id: "pp_humanseg",
        label: "PP-HumanSeg",
        docs_url: "https://github.com/opencv/opencv_zoo/tree/master/models/human_segmentation_pphumanseg",
        params: smartcell_params()
      },
      %{
        id: "pp_humanseg_quant",
        label: "PP-HumanSeg (quant)",
        docs_url: "https://github.com/opencv/opencv_zoo/tree/master/models/human_segmentation_pphumanseg",
        params: smartcell_params()
      },
    ]
  end

  def smartcell_params() do
    []
  end

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
          image = Evision.Mat.from_binary(image.data, {:u, 8}, height, width, 3)
          results = Evision.Zoo.ImageSegmentation.PPHumanSeg.infer(model, image)
          results = Evision.resize(results, {width, height}, interpolation: Evision.cv_INTER_NEAREST())

          image = Evision.cvtColor(image, Evision.cv_COLOR_RGB2BGR())
          vis_imgs = Evision.Zoo.ImageSegmentation.PPHumanSeg.visualize(image, results)
          vis_imgs = Enum.map(vis_imgs, &Kino.Image.new(Evision.imencode(".png", &1), :png))
          Kino.Frame.render(frame, Kino.Layout.grid(vis_imgs, columns: 2))
        end)

        Kino.Layout.grid([form, frame], boxed: true, gap: 16)
      end
    ]
  end
end
