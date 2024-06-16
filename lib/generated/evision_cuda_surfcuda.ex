defmodule Evision.CUDA.SURFCUDA do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `CUDA.SURFCUDA` struct.

  - **ref**. `reference()`

    The underlying erlang resource variable.

  """
  @type t :: %__MODULE__{
    ref: reference()
  }
  @enforce_keys [:ref]
  defstruct [:ref]
  alias __MODULE__, as: T

  @doc false
  def to_struct({:ok, %{class: Evision.CUDA.SURFCUDA, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.CUDA.SURFCUDA, ref: ref}) do
    %T{
      ref: ref
    }
  end

  @doc false
  def to_struct(ret) do
    Evision.Internal.Structurise.to_struct(ret)
  end
  
  @doc false
  def from_struct(%T{ref: ref}) do
    ref
  end

  @doc """
  create

  ##### Positional Arguments
  - **hessianThreshold**: `double`.

    Threshold for hessian keypoint detector used in SURF.

  ##### Keyword Arguments
  - **nOctaves**: `integer()`.

    Number of pyramid octaves the keypoint detector will use.

  - **nOctaveLayers**: `integer()`.

    Number of octave layers within each octave.

  - **extended**: `bool`.

    Extended descriptor flag (true - use extended 128-element descriptors; false - use
    64-element descriptors).

  - **keypointsRatio**: `float`.

    Limits a maximum number of features

  - **upright**: `bool`.

    Up-right or rotated features flag (true - do not compute orientation of features;
    false - compute orientation).

  ##### Return
  - **retval**: `SURF_CUDA`

  Python prototype (for reference only):
  ```python3
  create(_hessianThreshold[, _nOctaves[, _nOctaveLayers[, _extended[, _keypointsRatio[, _upright]]]]]) -> retval
  ```
  """
  @spec create(number(), [{:extended, term()} | {:keypointsRatio, term()} | {:nOctaveLayers, term()} | {:nOctaves, term()} | {:upright, term()}] | nil) :: Evision.CUDA.SURFCUDA.t() | {:error, String.t()}
  def create(hessianThreshold, opts) when is_number(hessianThreshold) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:extended, :keypointsRatio, :nOctaveLayers, :nOctaves, :upright])
    positional = [
      hessianThreshold: Evision.Internal.Structurise.from_struct(hessianThreshold)
    ]
    :evision_nif.cuda_cuda_SURF_CUDA_create_static(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  create

  ##### Positional Arguments
  - **hessianThreshold**: `double`.

    Threshold for hessian keypoint detector used in SURF.

  ##### Keyword Arguments
  - **nOctaves**: `integer()`.

    Number of pyramid octaves the keypoint detector will use.

  - **nOctaveLayers**: `integer()`.

    Number of octave layers within each octave.

  - **extended**: `bool`.

    Extended descriptor flag (true - use extended 128-element descriptors; false - use
    64-element descriptors).

  - **keypointsRatio**: `float`.

    Limits a maximum number of features

  - **upright**: `bool`.

    Up-right or rotated features flag (true - do not compute orientation of features;
    false - compute orientation).

  ##### Return
  - **retval**: `SURF_CUDA`

  Python prototype (for reference only):
  ```python3
  create(_hessianThreshold[, _nOctaves[, _nOctaveLayers[, _extended[, _keypointsRatio[, _upright]]]]]) -> retval
  ```
  """
  @spec create(number()) :: Evision.CUDA.SURFCUDA.t() | {:error, String.t()}
  def create(hessianThreshold) when is_number(hessianThreshold)
  do
    positional = [
      hessianThreshold: Evision.Internal.Structurise.from_struct(hessianThreshold)
    ]
    :evision_nif.cuda_cuda_SURF_CUDA_create_static(positional)
    |> to_struct()
  end

  @doc """
  defaultNorm

  ##### Positional Arguments
  - **self**: `Evision.CUDA.SURFCUDA.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  defaultNorm() -> retval
  ```
  """
  @spec defaultNorm(Evision.CUDA.SURFCUDA.t()) :: integer() | {:error, String.t()}
  def defaultNorm(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_SURF_CUDA_defaultNorm(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  descriptorSize

  ##### Positional Arguments
  - **self**: `Evision.CUDA.SURFCUDA.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  descriptorSize() -> retval
  ```
  """
  @spec descriptorSize(Evision.CUDA.SURFCUDA.t()) :: integer() | {:error, String.t()}
  def descriptorSize(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_SURF_CUDA_descriptorSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Finds the keypoints using fast hessian detector used in SURF

  ##### Positional Arguments
  - **self**: `Evision.CUDA.SURFCUDA.t()`
  - **img**: `Evision.CUDA.GpuMat.t()`.

    Source image, currently supports only CV_8UC1 images.

  - **mask**: `Evision.CUDA.GpuMat.t()`.

    A mask image same size as src and of type CV_8UC1.

  ##### Return
  - **keypoints**: `Evision.CUDA.GpuMat.t()`.

    Detected keypoints.

  Python prototype (for reference only):
  ```python3
  detect(img, mask[, keypoints]) -> keypoints
  ```
  """
  @spec detect(Evision.CUDA.SURFCUDA.t(), Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t(), [{atom(), term()},...] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def detect(self, img, mask, opts) when is_struct(img, Evision.CUDA.GpuMat) and is_struct(mask, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img),
      mask: Evision.Internal.Structurise.from_struct(mask)
    ]
    :evision_nif.cuda_cuda_SURF_CUDA_detect(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Finds the keypoints using fast hessian detector used in SURF

  ##### Positional Arguments
  - **self**: `Evision.CUDA.SURFCUDA.t()`
  - **img**: `Evision.CUDA.GpuMat.t()`.

    Source image, currently supports only CV_8UC1 images.

  - **mask**: `Evision.CUDA.GpuMat.t()`.

    A mask image same size as src and of type CV_8UC1.

  ##### Return
  - **keypoints**: `Evision.CUDA.GpuMat.t()`.

    Detected keypoints.

  Python prototype (for reference only):
  ```python3
  detect(img, mask[, keypoints]) -> keypoints
  ```
  """
  @spec detect(Evision.CUDA.SURFCUDA.t(), Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def detect(self, img, mask) when is_struct(img, Evision.CUDA.GpuMat) and is_struct(mask, Evision.CUDA.GpuMat)
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img),
      mask: Evision.Internal.Structurise.from_struct(mask)
    ]
    :evision_nif.cuda_cuda_SURF_CUDA_detect(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Finds the keypoints and computes their descriptors using fast hessian detector used in SURF

  ##### Positional Arguments
  - **self**: `Evision.CUDA.SURFCUDA.t()`
  - **img**: `Evision.CUDA.GpuMat.t()`.

    Source image, currently supports only CV_8UC1 images.

  - **mask**: `Evision.CUDA.GpuMat.t()`.

    A mask image same size as src and of type CV_8UC1.

  ##### Keyword Arguments
  - **useProvidedKeypoints**: `bool`.

    Compute descriptors for the user-provided keypoints and recompute keypoints direction.

  ##### Return
  - **keypoints**: `Evision.CUDA.GpuMat.t()`.

    Detected keypoints.

  - **descriptors**: `Evision.CUDA.GpuMat.t()`.

    Keypoint descriptors.

  Python prototype (for reference only):
  ```python3
  detectWithDescriptors(img, mask[, keypoints[, descriptors[, useProvidedKeypoints]]]) -> keypoints, descriptors
  ```
  """
  @spec detectWithDescriptors(Evision.CUDA.SURFCUDA.t(), Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t(), [{:useProvidedKeypoints, term()}] | nil) :: {Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t()} | {:error, String.t()}
  def detectWithDescriptors(self, img, mask, opts) when is_struct(img, Evision.CUDA.GpuMat) and is_struct(mask, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:useProvidedKeypoints])
    positional = [
      img: Evision.Internal.Structurise.from_struct(img),
      mask: Evision.Internal.Structurise.from_struct(mask)
    ]
    :evision_nif.cuda_cuda_SURF_CUDA_detectWithDescriptors(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Finds the keypoints and computes their descriptors using fast hessian detector used in SURF

  ##### Positional Arguments
  - **self**: `Evision.CUDA.SURFCUDA.t()`
  - **img**: `Evision.CUDA.GpuMat.t()`.

    Source image, currently supports only CV_8UC1 images.

  - **mask**: `Evision.CUDA.GpuMat.t()`.

    A mask image same size as src and of type CV_8UC1.

  ##### Keyword Arguments
  - **useProvidedKeypoints**: `bool`.

    Compute descriptors for the user-provided keypoints and recompute keypoints direction.

  ##### Return
  - **keypoints**: `Evision.CUDA.GpuMat.t()`.

    Detected keypoints.

  - **descriptors**: `Evision.CUDA.GpuMat.t()`.

    Keypoint descriptors.

  Python prototype (for reference only):
  ```python3
  detectWithDescriptors(img, mask[, keypoints[, descriptors[, useProvidedKeypoints]]]) -> keypoints, descriptors
  ```
  """
  @spec detectWithDescriptors(Evision.CUDA.SURFCUDA.t(), Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t()) :: {Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t()} | {:error, String.t()}
  def detectWithDescriptors(self, img, mask) when is_struct(img, Evision.CUDA.GpuMat) and is_struct(mask, Evision.CUDA.GpuMat)
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img),
      mask: Evision.Internal.Structurise.from_struct(mask)
    ]
    :evision_nif.cuda_cuda_SURF_CUDA_detectWithDescriptors(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  downloadKeypoints

  ##### Positional Arguments
  - **self**: `Evision.CUDA.SURFCUDA.t()`
  - **keypointsGPU**: `Evision.CUDA.GpuMat.t()`

  ##### Return
  - **keypoints**: `[Evision.KeyPoint]`

  Python prototype (for reference only):
  ```python3
  downloadKeypoints(keypointsGPU) -> keypoints
  ```
  """
  @spec downloadKeypoints(Evision.CUDA.SURFCUDA.t(), Evision.CUDA.GpuMat.t()) :: list(Evision.KeyPoint.t()) | {:error, String.t()}
  def downloadKeypoints(self, keypointsGPU) when is_struct(keypointsGPU, Evision.CUDA.GpuMat)
  do
    positional = [
      keypointsGPU: Evision.Internal.Structurise.from_struct(keypointsGPU)
    ]
    :evision_nif.cuda_cuda_SURF_CUDA_downloadKeypoints(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec get_extended(Evision.CUDA.SURFCUDA.t()) :: boolean()
  def get_extended(self) do
    :evision_nif.cuda_SURF_CUDA_get_extended(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec get_hessianThreshold(Evision.CUDA.SURFCUDA.t()) :: number()
  def get_hessianThreshold(self) do
    :evision_nif.cuda_SURF_CUDA_get_hessianThreshold(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec get_keypointsRatio(Evision.CUDA.SURFCUDA.t()) :: number()
  def get_keypointsRatio(self) do
    :evision_nif.cuda_SURF_CUDA_get_keypointsRatio(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec get_nOctaveLayers(Evision.CUDA.SURFCUDA.t()) :: integer()
  def get_nOctaveLayers(self) do
    :evision_nif.cuda_SURF_CUDA_get_nOctaveLayers(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec get_nOctaves(Evision.CUDA.SURFCUDA.t()) :: integer()
  def get_nOctaves(self) do
    :evision_nif.cuda_SURF_CUDA_get_nOctaves(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec get_upright(Evision.CUDA.SURFCUDA.t()) :: boolean()
  def get_upright(self) do
    :evision_nif.cuda_SURF_CUDA_get_upright(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
end
