defmodule Evision.Zoo do
  def download(model_url, filename, opts \\ []) do
    # todo
    {:ok, ""}
  end

  def to_quoted_backend_and_target(attrs) do
    backend = case attrs["backend"] do
      "cuda" -> quote do
        Evision.cv_DNN_BACKEND_CUDA()
      end
      "timvx" -> quote do
        Evision.cv_DNN_BACKEND_TIMVX()
      end
      _ -> quote do
        Evision.cv_DNN_BACKEND_OPENCV()
      end
    end

    target = case attrs["target"] do
      "cuda" -> quote do
        Evision.cv_DNN_TARGET_CUDA()
      end
      "cuda_fp16" -> quote do
        Evision.cv_DNN_TARGET_CUDA_FP16()
      end
      _ -> quote do
        Evision.cv_DNN_TARGET_CPU()
      end
    end

    {backend, target}
  end
end
