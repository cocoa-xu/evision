defmodule Evision.Zoo.FaceRecognition.SFace do
  @moduledoc """
  SFace: Sigmoid-Constrained Hypersphere Loss for Robust Face Recognition
  """

  @doc """
  Default configuration.
  """
  @spec default_config :: map()
  def default_config do
    %{
      backend: Evision.Constant.cv_DNN_BACKEND_OPENCV(),
      target: Evision.Constant.cv_DNN_TARGET_CPU(),
      distance_type: :cosine_similarity,
      cosine_threshold: 0.363,
      l2_norm_threshold: 1.128,

      conf_threshold: 0.9,
      nms_threshold: 0.3
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
        name: "Face Recognizer",
        params: [
          %{field: "detector", label: "Face Detector", type: :string, default: "yunet",
            is_option: true,
            options: [
              %{value: "yunet", label: "YuNet"},
              %{value: "yunet_quant", label: "YuNet (Quant)"}
            ]
          },
          %{field: "distance_type", label: "Distance Type", type: :string, default: "#{config[:distance_type]}",
            is_option: true,
            options: [
              %{value: "cosine_similarity", label: "Cosine Similarity"},
              %{value: "l2_norm", label: "L2 Norm"},
            ]
          },
          %{field: "cosine_threshold", label: "Cosine Threshold", type: :float, default: config[:cosine_threshold]},
          %{field: "l2_norm_threshold", label: "L2-norm Threshold", type: :float, default: config[:l2_norm_threshold]}
        ]
      }
    ] ++ Evision.Zoo.FaceDetection.YuNet.smartcell_params()
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
  @spec init(binary | :default_model | :quant_model, nil | Keyword.t()) :: {:error, String.t()} | Evision.FaceRecognizerSF.t()
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

    Evision.FaceRecognizerSF.create(
      model_path, "",
      backend_id: backend,
      target_id: target
    )
  end

  @doc """
  Get feature for the input face image.

  ##### Positional arguments
  - **self**: `Evision.FaceRecognizerSF.t()`.

    An initialized FaceRecognizerSF model.

  - **face_image**: `Evision.Mat.maybe_mat_in()`.

    Input face image.

  - **bbox**: `Evision.Mat.maybe_mat_in()`.

    Optional bounding box that specifies the face location in the given image.

    Defaults to `nil`.

    When `bbox` is not `nil`, `preprocess/3` will crop the face from given image
    and use the cropped image as the input.

    Otherwise, `face_image` will be set as the input.
  """
  def infer(self, face_image, bbox \\ nil) do
    inputBlob = preprocess(self, face_image, bbox)
    Evision.FaceRecognizerSF.feature(self, inputBlob)
  end

  @doc """
  Preprocessing the input face image.

  `infer/3` will call this function automatically.

  ##### Positional arguments
  - **self**: `Evision.FaceRecognizerSF.t()`.

    An initialized FaceRecognizerSF model.

  - **face_image**: `Evision.Mat.maybe_mat_in()`.

    Input face image.

  - **bbox**: `Evision.Mat.maybe_mat_in()`.

    Optional bounding box that specifies the face location in the given image.

    Defaults to `nil`.

    When `bbox` is not `nil`, `preprocess/3` will crop the face from given image
    and return the cropped image.

    Otherwise, `face_image` will be returned.
  """
  def preprocess(_self, face_image, nil), do: face_image
  def preprocess(self, face_image, bbox) do
    Evision.FaceRecognizerSF.alignCrop(self, face_image, bbox)
  end

  @doc """
  Compare two face features.

  ##### Positional Arguments
  - **self**: `Evision.FaceRecognizerSF.t()`

  - **face1_feat**: `Evision.Mat.maybe_mat_in()`.

    Feature value of face 1.

  - **face2_feat**: `Evision.Mat.maybe_mat_in()`.

    Feature value of face 2.

  ##### Keyword Arguments
  - **distance_type**: `atom`.

    Either `:cosine_similarity` or `:l2_norm`. Defaults to `:cosine_similarity`.

  - **cosine_threshold**: `number()`.

    Defaults to `0.363`.

  - **l2_norm_threshold**: `number()`.

    Defaults to `1.128`.

  ##### Return
  A map with three keys.

  - **matched**: `boolean()`

    `true` if two faces match, `false` otherwise.

  - **measure**: `"cosine_score"` | "`l2_norm_distance`"

    Distance type.

  - **retval**: `number()`

    - When `measure == "cosine_score"`, `retval` is the cosine similarity score.
    - When `measure == "l2_norm_distance"`, `retval` is the L2 norm distance.

  """
  @spec match_feature(Evision.FaceRecognizerSF.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: any()
  def match_feature(self=%Evision.FaceRecognizerSF{}, face1_feat, face2_feat, opts \\ []) do
    config = default_config()
    distance_type = opts[:distance_type] || config[:distance_type]
    distance_type =
      case distance_type do
        :cosine_similarity ->
          0
        :l2_norm ->
          1
        _ ->
          raise "Unknown distance_type: #{inspect(distance_type)}"
    end

    result = Evision.FaceRecognizerSF.match(self, face1_feat, face2_feat, dis_type: distance_type)
    distance_type = opts[:distance_type] || :cosine_similarity
    get_result(distance_type, result, opts)
  end

  defp get_result(:cosine_similarity, cosine_score, opts) do
    default_config = default_config()
    cosine_threshold = opts[:cosine_threshold] || default_config[:cosine_threshold]
    %{matched: cosine_score >= cosine_threshold, retval: cosine_score, measure: "cosine_score"}
  end

  defp get_result(:l2_norm, l2_norm_distance, opts) do
    default_config = default_config()
    l2_norm_threshold = opts[:l2_norm_threshold] || default_config[:l2_norm_threshold]
    %{matched: l2_norm_distance < l2_norm_threshold, retval: l2_norm_distance, measure: "l2_norm_distance"}
  end

  defp get_result(distance_type, _, _) do
    raise "Unknown distance_type: #{inspect(distance_type)}"
  end

  @doc """
  Compare two faces.

  ##### Positional Arguments
  - **self**: `Evision.FaceRecognizerSF.t()`

  - **original**: `Evision.Mat.maybe_mat_in()`.

    Original face.

  - **comparison**: `Evision.Mat.maybe_mat_in()`.

    Comparison face.

  ##### Keyword Arguments
  - **distance_type**: `atom`.

    Either `:cosine_similarity` or `:l2_norm`. Defaults to `:cosine_similarity`.

  - **cosine_threshold**: `number()`.

    Defaults to `0.363`.

  - **l2_norm_threshold**: `number()`.

    Defaults to `1.128`.

  - **detector_module**: `module()`.

    Face detector module. Defaults to `Evision.Zoo.FaceDetection.YuNet`.

  - **detector_model**: `String.t()` | `atom()`.

    Face detector model name or path to model weights. Defaults to `:default_model`.

  - **detector_opts**: `Keyword.t()`.

    Face detector initialization options. Defaults to `[]`.

  ##### Return
  A map with three keys.

  - **matched**: `boolean()`

    `true` if two faces match, `false` otherwise.

  - **measure**: `"cosine_score"` | "`l2_norm_distance`"

    Distance type.

  - **retval**: `number()`

    - When `measure == "cosine_score"`, `retval` is the cosine similarity score.
    - When `measure == "l2_norm_distance"`, `retval` is the L2 norm distance.
  """
  @spec match(Evision.FaceRecognizerSF.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: any()
  def match(self=%Evision.FaceRecognizerSF{}, original, comparison, opts \\ []) do
    detector_module = opts[:detector_module] || Evision.Zoo.FaceDetection.YuNet
    detector_model = opts[:detector_model] || :default_model
    detector_opts = opts[:detector_opts] || []
    detector = detector_module.init(detector_model, detector_opts)

    original_face_bbox = detector_module.infer(detector, original)
    comparison_face_bbox = detector_module.infer(detector, comparison)

    original_face_feature = Evision.Zoo.FaceRecognition.SFace.infer(self, original, original_face_bbox)
    comparison_face_feature = Evision.Zoo.FaceRecognition.SFace.infer(self, comparison, comparison_face_bbox)

    match_feature(self, original_face_feature, comparison_face_feature, opts)
  end

  @doc """
  Model URL and filename of predefined model.
  """
  @spec model_info(:default_model | :quant_model) :: {String.t(), String.t()}
  def model_info(:default_model) do
    {
      "https://github.com/opencv/opencv_zoo/raw/fd2da740ebc6d0fe489d86cb55133148978cd82e/models/face_recognition_sface/face_recognition_sface_2021dec.onnx",
      "face_recognition_sface_2021dec.onnx"
    }
  end

  def model_info(:quant_model) do
    {
      "https://github.com/opencv/opencv_zoo/raw/fd2da740ebc6d0fe489d86cb55133148978cd82e/models/face_recognition_sface/face_recognition_sface_2021dec_int8.onnx",
      "face_recognition_sface_2021dec_int8.onnx"
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
        id: "sface",
        label: "SFace",
        docs_url: "https://github.com/opencv/opencv_zoo/tree/master/models/face_recognition_sface",
        params: smartcell_params(),
        docs: docs(),
      },
      %{
        id: "sface_quant",
        label: "SFace (quant)",
        docs_url: "https://github.com/opencv/opencv_zoo/tree/master/models/face_recognition_sface",
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

    recognizer_opts = [
      backend: backend,
      target: target,
      distance_type: String.to_atom(attrs["distance_type"]),
      cosine_threshold: attrs["cosine_threshold"],
      l2_norm_threshold: attrs["l2_norm_threshold"]
    ]

    detector_opts = [
      backend: backend,
      target: target,
      nms_threshold: attrs["nms_threshold"],
      conf_threshold: attrs["conf_threshold"],
      top_k: 5
    ]

    model =
      case attrs["variant_id"] do
        "sface_quant" ->
          :quant_model
        _ ->
          :default_model
      end

    {detector_module, detector_model} =
      case attrs["detector"] do
        "yunet" ->
          {Evision.Zoo.FaceDetection.YuNet, :default_model}
        "yunet_quant" ->
          {Evision.Zoo.FaceDetection.YuNet, :quant_model}
        unknown_detector ->
          raise "Unknown face detector: #{inspect(unknown_detector)}"
      end

    [
      quote do
        recognizer = Evision.Zoo.FaceRecognition.SFace.init(unquote(model), unquote(recognizer_opts))
        detector = unquote(detector_module).init(unquote(detector_model), unquote(detector_opts))
      end,
      quote do
        original_input = Kino.Input.image("Original")
        comparison_input = Kino.Input.image("Comparison")
        form = Kino.Control.form([original: original_input, comparison: comparison_input], submit: "Run")

        frame = Kino.Frame.new()

        form
        |> Kino.Control.stream()
        |> Stream.filter(& (&1.data.original != nil) or (&1.data.comparison != nil))
        |> Kino.listen(fn %{data: %{original: original_image, comparison: comparison_image}} ->
          Kino.Frame.render(frame, Kino.Markdown.new("Running..."))

          original_image =
            original_image.file_ref
            |> Kino.Input.file_path()
            |> File.read!()
            |> Evision.Mat.from_binary({:u, 8}, original_image.height, original_image.width, 3)

          comparison_image =
            comparison_image.file_ref
            |> Kino.Input.file_path()
            |> File.read!()
            |> Evision.Mat.from_binary({:u, 8}, comparison_image.height, comparison_image.width, 3)

          original_results = unquote(detector_module).infer(detector, original_image)
          comparison_results = unquote(detector_module).infer(detector, comparison_image)

          case {original_results, comparison_results} do
            {%Evision.Mat{}, %Evision.Mat{}} ->
              original_bbox = Evision.Mat.to_nx(original_results, Nx.BinaryBackend)[0][0..-2//1]
              comparison_bbox = Evision.Mat.to_nx(comparison_results, Nx.BinaryBackend)[0][0..-2//1]

              original_blob =
                Evision.FaceRecognizerSF.alignCrop(recognizer, original_image, original_bbox)

              original_feature =
                Evision.FaceRecognizerSF.feature(recognizer, original_blob)
                |> Evision.Mat.to_nx()
                |> Evision.Mat.from_nx()

              comparison_blob =
                Evision.FaceRecognizerSF.alignCrop(recognizer, comparison_image, comparison_bbox)

              comparison_feature =
                Evision.FaceRecognizerSF.feature(recognizer, comparison_blob)
                |> Evision.Mat.to_nx()
                |> Evision.Mat.from_nx()

              %{matched: matched, retval: val, measure: measure} = Evision.Zoo.FaceRecognition.SFace.match_feature(
                recognizer, original_feature, comparison_feature)

              original_image = Evision.cvtColor(original_image, Evision.Constant.cv_COLOR_RGB2BGR())
              comparison_image = Evision.cvtColor(comparison_image, Evision.Constant.cv_COLOR_RGB2BGR())
              vis_original = unquote(detector_module).visualize(original_image, original_results[0])
              vis_comparison = unquote(detector_module).visualize(comparison_image, comparison_results[0])

              vis = [
                Kino.Image.new(Evision.imencode(".png", vis_original), :png),
                Kino.Image.new(Evision.imencode(".png", vis_comparison), :png)
              ]

              Kino.Frame.render(frame, Kino.Layout.grid(vis, columns: 2))
              Kino.Frame.append(frame, Kino.Markdown.new("Result: #{matched}, #{measure}: #{val}"))

            {{:error, _}, %Evision.Mat{}} ->
              Kino.Frame.render(frame, Kino.Markdown.new("Cannot detect any face in the original image"))

            {%Evision.Mat{}, _} ->
              Kino.Frame.render(frame, Kino.Markdown.new("Cannot detect any face in the comparison image"))

            {_, _} ->
              Kino.Frame.render(frame, Kino.Markdown.new("Cannot detect any face in both original and comparison images"))
          end
        end)

        Kino.Layout.grid([form, frame], boxed: true, gap: 16)
      end
    ]
  end
end
