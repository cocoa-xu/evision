defmodule Evision.Zoo do
  @moduledoc """
  Evision Model Zoo
  """

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

  def backends do
    %{
      "opencv" =>
        {Evision.Constant.cv_DNN_BACKEND_OPENCV(), "OpenCV",
         quote do
           Evision.Constant.cv_DNN_BACKEND_OPENCV()
         end},
      "cuda" =>
        {Evision.Constant.cv_DNN_BACKEND_CUDA(), "CUDA",
         quote do
           Evision.Constant.cv_DNN_BACKEND_CUDA()
         end},
      "halide" =>
        {Evision.Constant.cv_DNN_BACKEND_HALIDE(), "Halide",
         quote do
           Evision.Constant.cv_DNN_BACKEND_HALIDE()
         end},
      "inference_engine" =>
        {Evision.Constant.cv_DNN_BACKEND_INFERENCE_ENGINE(), "Inference Engine",
         quote do
           Evision.Constant.cv_DNN_BACKEND_INFERENCE_ENGINE()
         end},
      "timvx" =>
        {Evision.Constant.cv_DNN_BACKEND_TIMVX(), "TIMVX",
         quote do
           Evision.Constant.cv_DNN_BACKEND_TIMVX()
         end},
      "vkcom" =>
        {Evision.Constant.cv_DNN_BACKEND_VKCOM(), "VKCOM",
         quote do
           Evision.Constant.cv_DNN_BACKEND_VKCOM()
         end}
    }
  end

  def targets do
    %{
      Evision.Constant.cv_DNN_TARGET_CPU() => %{value: "cpu", label: "CPU"},
      Evision.Constant.cv_DNN_TARGET_CUDA() => %{value: "cuda", label: "CUDA"},
      Evision.Constant.cv_DNN_TARGET_CUDA_FP16() => %{value: "cuda_fp16", label: "CUDA FP16"},
      Evision.Constant.cv_DNN_TARGET_FPGA() => %{value: "fpga", label: "FPGA"},
      Evision.Constant.cv_DNN_TARGET_HDDL() => %{value: "hddl", label: "HDDL"},
      Evision.Constant.cv_DNN_TARGET_MYRIAD() => %{value: "myriad", label: "Myriad"},
      Evision.Constant.cv_DNN_TARGET_NPU() => %{value: "npu", label: "NPU"},
      Evision.Constant.cv_DNN_TARGET_OPENCL() => %{value: "opencl", label: "OpenCL"},
      Evision.Constant.cv_DNN_TARGET_OPENCL_FP16() => %{
        value: "opencl_fp16",
        label: "OpenCL FP16"
      },
      Evision.Constant.cv_DNN_TARGET_VULKAN() => %{value: "vulkan", label: "Vulkan"}
    }
  end

  def targets_reverse_lookup do
    %{
      "cpu" =>
        quote do
          Evision.Constant.cv_DNN_TARGET_CPU()
        end,
      "cuda" =>
        quote do
          Evision.Constant.cv_DNN_TARGET_CUDA()
        end,
      "cuda_fp16" =>
        quote do
          Evision.Constant.cv_DNN_TARGET_CUDA_FP16()
        end,
      "fpga" =>
        quote do
          Evision.Constant.cv_DNN_TARGET_FPGA()
        end,
      "hddl" =>
        quote do
          Evision.Constant.cv_DNN_TARGET_HDDL()
        end,
      "myriad" =>
        quote do
          Evision.Constant.cv_DNN_TARGET_MYRIAD()
        end,
      "npu" =>
        quote do
          Evision.Constant.cv_DNN_TARGET_NPU()
        end,
      "opencl" =>
        quote do
          Evision.Constant.cv_DNN_TARGET_OPENCL()
        end,
      "opencl_fp16" =>
        quote do
          Evision.Constant.cv_DNN_TARGET_OPENCL_FP16()
        end,
      "vulkan" =>
        quote do
          Evision.Constant.cv_DNN_TARGET_VULKAN()
        end
    }
  end

  def available_backend_and_target do
    backends = backends()
    targets = targets()

    {backend_options, target_options} =
      Enum.reduce(Map.to_list(backends), {[], []}, fn {backend_value,
                                                       {backend_id, backend_label, _}},
                                                      {backend_options, target_options} ->
        available_targets = Evision.DNN.getAvailableTargets(backend_id)

        if Enum.count(available_targets) == 0 do
          {backend_options, target_options}
        else
          target_options =
            Enum.reduce(available_targets, target_options, fn t, target_options ->
              target = Map.get(targets, t)

              if target do
                [target | target_options]
              else
                target_options
              end
            end)

          backend_options = [%{value: backend_value, label: backend_label} | backend_options]
          {backend_options, target_options}
        end
      end)

    {Enum.reverse(backend_options), Enum.reverse(target_options)}
  end

  def to_quoted_backend_and_target(attrs) do
    backend = attrs["backend"]
    backends = backends()
    selected_backend = Map.get(backends, backend)

    backend =
      if selected_backend do
        elem(selected_backend, 2)
      else
        quote do
          Evision.Constant.cv_DNN_BACKEND_OPENCV()
        end
      end

    targets_reverse_lookup = targets_reverse_lookup()
    target = attrs["target"]
    selected_target = Map.get(targets_reverse_lookup, target)

    target =
      if selected_target do
        selected_target
      else
        quote do
          Evision.Constant.cv_DNN_TARGET_CPU()
        end
      end

    {backend, target}
  end
end
