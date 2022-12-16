defmodule Evision.Zoo do
  @moduledoc false

  @type smartcell_option :: %{
    value: Strung.t(),
    label: Strung.t()
  }
  @type smartcell_param :: %{
    field: String.t(),
    label: String.t(),
    type: atom(),
    default: term(),
    is_option: boolean() | nil,
    options: [smartcell_option()]
  }
  @type smartcell_param_map :: %{
    name: String.t(),
    params: [smartcell_param()]
  }
  @type smartcell_params :: [smartcell_param_map()]
  @type variant :: %{
    id: String.t(),
    name: String.t(),
    docs_url: String.t(),
    params: smartcell_params(),
    docs: String.t()
  }
  @type smartcell_tasks :: [variant()]

  def download(file_url, filename, opts \\ [])
      when is_binary(file_url) and is_binary(filename) and is_list(opts) do
    cache_dir = opts[:cache_dir] || cache_dir()
    filepath = Path.join([cache_dir, filename])

    if File.exists?(filepath) do
      {:ok, filepath}
    else
      case Evision.Zoo.Utils.HTTP.download(file_url, filepath) do
        :ok ->
          {:ok, filepath}

        err ->
          err
      end
    end
  end

  def cache_dir do
    cache_opts = if System.get_env("MIX_XDG"), do: %{os: :linux}, else: %{}

    cache_dir = Path.expand(:filename.basedir(:user_cache, "", cache_opts))

    File.mkdir_p!(cache_dir)
    cache_dir
  end

  def to_quoted_backend_and_target(attrs) do
    backend =
      case attrs["backend"] do
        "cuda" ->
          quote do
            Evision.cv_DNN_BACKEND_CUDA()
          end

        "timvx" ->
          quote do
            Evision.cv_DNN_BACKEND_TIMVX()
          end

        _ ->
          quote do
            Evision.cv_DNN_BACKEND_OPENCV()
          end
      end

    target =
      case attrs["target"] do
        "cuda" ->
          quote do
            Evision.cv_DNN_TARGET_CUDA()
          end

        "cuda_fp16" ->
          quote do
            Evision.cv_DNN_TARGET_CUDA_FP16()
          end

        _ ->
          quote do
            Evision.cv_DNN_TARGET_CPU()
          end
      end

    {backend, target}
  end
end
