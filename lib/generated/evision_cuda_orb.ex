defmodule Evision.CUDA.ORB do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `CUDA.ORB` struct.

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
  def to_struct({:ok, %{class: Evision.CUDA.ORB, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.CUDA.ORB, ref: ref}) do
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
  #### Variant 1:
  compute

  ##### Positional Arguments
  - **self**: `Evision.CUDA.ORB.t()`
  - **images**: `[Evision.Mat]`.

    Image set.

  ##### Return
  - **keypoints**: `[[Evision.KeyPoint]]`.

    Input collection of keypoints. Keypoints for which a descriptor cannot be
    computed are removed. Sometimes new keypoints can be added, for example: SIFT duplicates keypoint
    with several dominant orientations (for each orientation).

  - **descriptors**: `[Evision.Mat]`.

    Computed descriptors. In the second variant of the method descriptors[i] are
    descriptors computed for a keypoints[i]. Row j is the keypoints (or keypoints[i]) is the
    descriptor for keypoint j-th keypoint.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  compute(images, keypoints[, descriptors]) -> keypoints, descriptors
  ```
  #### Variant 2:
  Computes the descriptors for a set of keypoints detected in an image (first variant) or image set
  (second variant).

  ##### Positional Arguments
  - **self**: `Evision.CUDA.ORB.t()`
  - **image**: `Evision.Mat`.

    Image.

  ##### Return
  - **keypoints**: `[Evision.KeyPoint]`.

    Input collection of keypoints. Keypoints for which a descriptor cannot be
    computed are removed. Sometimes new keypoints can be added, for example: SIFT duplicates keypoint
    with several dominant orientations (for each orientation).

  - **descriptors**: `Evision.Mat.t()`.

    Computed descriptors. In the second variant of the method descriptors[i] are
    descriptors computed for a keypoints[i]. Row j is the keypoints (or keypoints[i]) is the
    descriptor for keypoint j-th keypoint.

  Python prototype (for reference only):
  ```python3
  compute(image, keypoints[, descriptors]) -> keypoints, descriptors
  ```

  """
  @spec compute(Evision.CUDA.ORB.t(), list(Evision.Mat.maybe_mat_in()), list(list(Evision.KeyPoint.t())), [{atom(), term()},...] | nil) :: {list(list(Evision.KeyPoint.t())), list(Evision.Mat.t())} | {:error, String.t()}
  def compute(self, images, keypoints, opts) when is_list(images) and is_list(keypoints) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      images: Evision.Internal.Structurise.from_struct(images),
      keypoints: Evision.Internal.Structurise.from_struct(keypoints)
    ]
    :evision_nif.cuda_ORB_compute(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec compute(Evision.CUDA.ORB.t(), Evision.Mat.maybe_mat_in(), list(Evision.KeyPoint.t()), [{atom(), term()},...] | nil) :: {list(Evision.KeyPoint.t()), Evision.Mat.t()} | {:error, String.t()}
  def compute(self, image, keypoints, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and is_list(keypoints) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      keypoints: Evision.Internal.Structurise.from_struct(keypoints)
    ]
    :evision_nif.cuda_ORB_compute(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  compute

  ##### Positional Arguments
  - **self**: `Evision.CUDA.ORB.t()`
  - **images**: `[Evision.Mat]`.

    Image set.

  ##### Return
  - **keypoints**: `[[Evision.KeyPoint]]`.

    Input collection of keypoints. Keypoints for which a descriptor cannot be
    computed are removed. Sometimes new keypoints can be added, for example: SIFT duplicates keypoint
    with several dominant orientations (for each orientation).

  - **descriptors**: `[Evision.Mat]`.

    Computed descriptors. In the second variant of the method descriptors[i] are
    descriptors computed for a keypoints[i]. Row j is the keypoints (or keypoints[i]) is the
    descriptor for keypoint j-th keypoint.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  compute(images, keypoints[, descriptors]) -> keypoints, descriptors
  ```
  #### Variant 2:
  Computes the descriptors for a set of keypoints detected in an image (first variant) or image set
  (second variant).

  ##### Positional Arguments
  - **self**: `Evision.CUDA.ORB.t()`
  - **image**: `Evision.Mat`.

    Image.

  ##### Return
  - **keypoints**: `[Evision.KeyPoint]`.

    Input collection of keypoints. Keypoints for which a descriptor cannot be
    computed are removed. Sometimes new keypoints can be added, for example: SIFT duplicates keypoint
    with several dominant orientations (for each orientation).

  - **descriptors**: `Evision.Mat.t()`.

    Computed descriptors. In the second variant of the method descriptors[i] are
    descriptors computed for a keypoints[i]. Row j is the keypoints (or keypoints[i]) is the
    descriptor for keypoint j-th keypoint.

  Python prototype (for reference only):
  ```python3
  compute(image, keypoints[, descriptors]) -> keypoints, descriptors
  ```

  """
  @spec compute(Evision.CUDA.ORB.t(), list(Evision.Mat.maybe_mat_in()), list(list(Evision.KeyPoint.t()))) :: {list(list(Evision.KeyPoint.t())), list(Evision.Mat.t())} | {:error, String.t()}
  def compute(self, images, keypoints) when is_list(images) and is_list(keypoints)
  do
    positional = [
      images: Evision.Internal.Structurise.from_struct(images),
      keypoints: Evision.Internal.Structurise.from_struct(keypoints)
    ]
    :evision_nif.cuda_ORB_compute(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec compute(Evision.CUDA.ORB.t(), Evision.Mat.maybe_mat_in(), list(Evision.KeyPoint.t())) :: {list(Evision.KeyPoint.t()), Evision.Mat.t()} | {:error, String.t()}
  def compute(self, image, keypoints) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and is_list(keypoints)
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      keypoints: Evision.Internal.Structurise.from_struct(keypoints)
    ]
    :evision_nif.cuda_ORB_compute(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Computes the descriptors for a set of keypoints detected in an image.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.ORB.t()`
  - **image**: `Evision.Mat`.

    Image.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    CUDA stream.

  ##### Return
  - **keypoints**: `Evision.Mat.t()`.

    Input collection of keypoints.

  - **descriptors**: `Evision.Mat.t()`.

    Computed descriptors. Row j is the descriptor for j-th keypoint.

  Python prototype (for reference only):
  ```python3
  computeAsync(image[, keypoints[, descriptors[, stream]]]) -> keypoints, descriptors
  ```
  #### Variant 2:
  Computes the descriptors for a set of keypoints detected in an image.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.ORB.t()`
  - **image**: `Evision.CUDA.GpuMat.t()`.

    Image.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    CUDA stream.

  ##### Return
  - **keypoints**: `Evision.CUDA.GpuMat.t()`.

    Input collection of keypoints.

  - **descriptors**: `Evision.CUDA.GpuMat.t()`.

    Computed descriptors. Row j is the descriptor for j-th keypoint.

  Python prototype (for reference only):
  ```python3
  computeAsync(image[, keypoints[, descriptors[, stream]]]) -> keypoints, descriptors
  ```

  """
  @spec computeAsync(Evision.CUDA.ORB.t(), Evision.Mat.maybe_mat_in(), [{:stream, term()}] | nil) :: {Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def computeAsync(self, image, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.cuda_cuda_ORB_computeAsync(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec computeAsync(Evision.CUDA.ORB.t(), Evision.CUDA.GpuMat.t(), [{:stream, term()}] | nil) :: {Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t()} | {:error, String.t()}
  def computeAsync(self, image, opts) when is_struct(image, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.cuda_cuda_ORB_computeAsync(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Computes the descriptors for a set of keypoints detected in an image.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.ORB.t()`
  - **image**: `Evision.Mat`.

    Image.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    CUDA stream.

  ##### Return
  - **keypoints**: `Evision.Mat.t()`.

    Input collection of keypoints.

  - **descriptors**: `Evision.Mat.t()`.

    Computed descriptors. Row j is the descriptor for j-th keypoint.

  Python prototype (for reference only):
  ```python3
  computeAsync(image[, keypoints[, descriptors[, stream]]]) -> keypoints, descriptors
  ```
  #### Variant 2:
  Computes the descriptors for a set of keypoints detected in an image.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.ORB.t()`
  - **image**: `Evision.CUDA.GpuMat.t()`.

    Image.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    CUDA stream.

  ##### Return
  - **keypoints**: `Evision.CUDA.GpuMat.t()`.

    Input collection of keypoints.

  - **descriptors**: `Evision.CUDA.GpuMat.t()`.

    Computed descriptors. Row j is the descriptor for j-th keypoint.

  Python prototype (for reference only):
  ```python3
  computeAsync(image[, keypoints[, descriptors[, stream]]]) -> keypoints, descriptors
  ```

  """
  @spec computeAsync(Evision.CUDA.ORB.t(), Evision.Mat.maybe_mat_in()) :: {Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def computeAsync(self, image) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.cuda_cuda_ORB_computeAsync(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec computeAsync(Evision.CUDA.ORB.t(), Evision.CUDA.GpuMat.t()) :: {Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t()} | {:error, String.t()}
  def computeAsync(self, image) when is_struct(image, Evision.CUDA.GpuMat)
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.cuda_cuda_ORB_computeAsync(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  convert

  ##### Positional Arguments
  - **self**: `Evision.CUDA.ORB.t()`
  - **gpu_keypoints**: `Evision.Mat`

  ##### Return
  - **keypoints**: `[Evision.KeyPoint]`

  Converts keypoints array from internal representation to standard vector.

  Python prototype (for reference only):
  ```python3
  convert(gpu_keypoints) -> keypoints
  ```
  #### Variant 2:
  convert

  ##### Positional Arguments
  - **self**: `Evision.CUDA.ORB.t()`
  - **gpu_keypoints**: `Evision.CUDA.GpuMat.t()`

  ##### Return
  - **keypoints**: `[Evision.KeyPoint]`

  Converts keypoints array from internal representation to standard vector.

  Python prototype (for reference only):
  ```python3
  convert(gpu_keypoints) -> keypoints
  ```

  """
  @spec convert(Evision.CUDA.ORB.t(), Evision.Mat.maybe_mat_in()) :: list(Evision.KeyPoint.t()) | {:error, String.t()}
  def convert(self, gpu_keypoints) when (is_struct(gpu_keypoints, Evision.Mat) or is_struct(gpu_keypoints, Nx.Tensor) or is_number(gpu_keypoints) or is_tuple(gpu_keypoints))
  do
    positional = [
      gpu_keypoints: Evision.Internal.Structurise.from_struct(gpu_keypoints)
    ]
    :evision_nif.cuda_cuda_ORB_convert(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec convert(Evision.CUDA.ORB.t(), Evision.CUDA.GpuMat.t()) :: list(Evision.KeyPoint.t()) | {:error, String.t()}
  def convert(self, gpu_keypoints) when is_struct(gpu_keypoints, Evision.CUDA.GpuMat)
  do
    positional = [
      gpu_keypoints: Evision.Internal.Structurise.from_struct(gpu_keypoints)
    ]
    :evision_nif.cuda_cuda_ORB_convert(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  create
  ##### Keyword Arguments
  - **nfeatures**: `integer()`.
  - **scaleFactor**: `float`.
  - **nlevels**: `integer()`.
  - **edgeThreshold**: `integer()`.
  - **firstLevel**: `integer()`.
  - **wTA_K**: `integer()`.
  - **scoreType**: `integer()`.
  - **patchSize**: `integer()`.
  - **fastThreshold**: `integer()`.
  - **blurForDescriptor**: `bool`.

  ##### Return
  - **retval**: `Evision.CUDA.ORB.t()`

  Python prototype (for reference only):
  ```python3
  create([, nfeatures[, scaleFactor[, nlevels[, edgeThreshold[, firstLevel[, WTA_K[, scoreType[, patchSize[, fastThreshold[, blurForDescriptor]]]]]]]]]]) -> retval
  ```
  """
  @spec create([{:blurForDescriptor, term()} | {:edgeThreshold, term()} | {:fastThreshold, term()} | {:firstLevel, term()} | {:nfeatures, term()} | {:nlevels, term()} | {:patchSize, term()} | {:scaleFactor, term()} | {:scoreType, term()} | {:wTA_K, term()}] | nil) :: Evision.CUDA.ORB.t() | {:error, String.t()}
  def create(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:blurForDescriptor, :edgeThreshold, :fastThreshold, :firstLevel, :nfeatures, :nlevels, :patchSize, :scaleFactor, :scoreType, :wTA_K])
    positional = [
    ]
    :evision_nif.cuda_cuda_ORB_create_static(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  create
  ##### Keyword Arguments
  - **nfeatures**: `integer()`.
  - **scaleFactor**: `float`.
  - **nlevels**: `integer()`.
  - **edgeThreshold**: `integer()`.
  - **firstLevel**: `integer()`.
  - **wTA_K**: `integer()`.
  - **scoreType**: `integer()`.
  - **patchSize**: `integer()`.
  - **fastThreshold**: `integer()`.
  - **blurForDescriptor**: `bool`.

  ##### Return
  - **retval**: `Evision.CUDA.ORB.t()`

  Python prototype (for reference only):
  ```python3
  create([, nfeatures[, scaleFactor[, nlevels[, edgeThreshold[, firstLevel[, WTA_K[, scoreType[, patchSize[, fastThreshold[, blurForDescriptor]]]]]]]]]]) -> retval
  ```
  """
  @spec create() :: Evision.CUDA.ORB.t() | {:error, String.t()}
  def create() do
    positional = [
    ]
    :evision_nif.cuda_cuda_ORB_create_static(positional)
    |> to_struct()
  end

  @doc """
  defaultNorm

  ##### Positional Arguments
  - **self**: `Evision.CUDA.ORB.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  defaultNorm() -> retval
  ```
  """
  @spec defaultNorm(Evision.CUDA.ORB.t()) :: integer() | {:error, String.t()}
  def defaultNorm(self) do
    positional = [
    ]
    :evision_nif.cuda_ORB_defaultNorm(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  descriptorSize

  ##### Positional Arguments
  - **self**: `Evision.CUDA.ORB.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  descriptorSize() -> retval
  ```
  """
  @spec descriptorSize(Evision.CUDA.ORB.t()) :: integer() | {:error, String.t()}
  def descriptorSize(self) do
    positional = [
    ]
    :evision_nif.cuda_ORB_descriptorSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  descriptorType

  ##### Positional Arguments
  - **self**: `Evision.CUDA.ORB.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  descriptorType() -> retval
  ```
  """
  @spec descriptorType(Evision.CUDA.ORB.t()) :: integer() | {:error, String.t()}
  def descriptorType(self) do
    positional = [
    ]
    :evision_nif.cuda_ORB_descriptorType(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  detect

  ##### Positional Arguments
  - **self**: `Evision.CUDA.ORB.t()`
  - **images**: `[Evision.Mat]`.

    Image set.

  ##### Keyword Arguments
  - **masks**: `[Evision.Mat]`.

    Masks for each input image specifying where to look for keypoints (optional).
    masks[i] is a mask for images[i].

  ##### Return
  - **keypoints**: `[[Evision.KeyPoint]]`.

    The detected keypoints. In the second variant of the method keypoints[i] is a set
    of keypoints detected in images[i] .

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  detect(images[, masks]) -> keypoints
  ```
  #### Variant 2:
  Detects keypoints in an image (first variant) or image set (second variant).

  ##### Positional Arguments
  - **self**: `Evision.CUDA.ORB.t()`
  - **image**: `Evision.Mat`.

    Image.

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.

    Mask specifying where to look for keypoints (optional). It must be a 8-bit integer
    matrix with non-zero values in the region of interest.

  ##### Return
  - **keypoints**: `[Evision.KeyPoint]`.

    The detected keypoints. In the second variant of the method keypoints[i] is a set
    of keypoints detected in images[i] .

  Python prototype (for reference only):
  ```python3
  detect(image[, mask]) -> keypoints
  ```

  """
  @spec detect(Evision.CUDA.ORB.t(), list(Evision.Mat.maybe_mat_in()), [{:masks, term()}] | nil) :: list(list(Evision.KeyPoint.t())) | {:error, String.t()}
  def detect(self, images, opts) when is_list(images) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:masks])
    positional = [
      images: Evision.Internal.Structurise.from_struct(images)
    ]
    :evision_nif.cuda_ORB_detect(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec detect(Evision.CUDA.ORB.t(), Evision.Mat.maybe_mat_in(), [{:mask, term()}] | nil) :: list(Evision.KeyPoint.t()) | {:error, String.t()}
  def detect(self, image, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:mask])
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.cuda_ORB_detect(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  detect

  ##### Positional Arguments
  - **self**: `Evision.CUDA.ORB.t()`
  - **images**: `[Evision.Mat]`.

    Image set.

  ##### Keyword Arguments
  - **masks**: `[Evision.Mat]`.

    Masks for each input image specifying where to look for keypoints (optional).
    masks[i] is a mask for images[i].

  ##### Return
  - **keypoints**: `[[Evision.KeyPoint]]`.

    The detected keypoints. In the second variant of the method keypoints[i] is a set
    of keypoints detected in images[i] .

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  detect(images[, masks]) -> keypoints
  ```
  #### Variant 2:
  Detects keypoints in an image (first variant) or image set (second variant).

  ##### Positional Arguments
  - **self**: `Evision.CUDA.ORB.t()`
  - **image**: `Evision.Mat`.

    Image.

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.

    Mask specifying where to look for keypoints (optional). It must be a 8-bit integer
    matrix with non-zero values in the region of interest.

  ##### Return
  - **keypoints**: `[Evision.KeyPoint]`.

    The detected keypoints. In the second variant of the method keypoints[i] is a set
    of keypoints detected in images[i] .

  Python prototype (for reference only):
  ```python3
  detect(image[, mask]) -> keypoints
  ```

  """
  @spec detect(Evision.CUDA.ORB.t(), list(Evision.Mat.maybe_mat_in())) :: list(list(Evision.KeyPoint.t())) | {:error, String.t()}
  def detect(self, images) when is_list(images)
  do
    positional = [
      images: Evision.Internal.Structurise.from_struct(images)
    ]
    :evision_nif.cuda_ORB_detect(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec detect(Evision.CUDA.ORB.t(), Evision.Mat.maybe_mat_in()) :: list(Evision.KeyPoint.t()) | {:error, String.t()}
  def detect(self, image) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.cuda_ORB_detect(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  detectAndCompute

  ##### Positional Arguments
  - **self**: `Evision.CUDA.ORB.t()`
  - **image**: `Evision.Mat`
  - **mask**: `Evision.Mat`

  ##### Keyword Arguments
  - **useProvidedKeypoints**: `bool`.

  ##### Return
  - **keypoints**: `[Evision.KeyPoint]`
  - **descriptors**: `Evision.Mat.t()`.

  Detects keypoints and computes the descriptors

  Python prototype (for reference only):
  ```python3
  detectAndCompute(image, mask[, descriptors[, useProvidedKeypoints]]) -> keypoints, descriptors
  ```
  """
  @spec detectAndCompute(Evision.CUDA.ORB.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:useProvidedKeypoints, term()}] | nil) :: {list(Evision.KeyPoint.t()), Evision.Mat.t()} | {:error, String.t()}
  def detectAndCompute(self, image, mask, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (is_struct(mask, Evision.Mat) or is_struct(mask, Nx.Tensor) or is_number(mask) or is_tuple(mask)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:useProvidedKeypoints])
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      mask: Evision.Internal.Structurise.from_struct(mask)
    ]
    :evision_nif.cuda_ORB_detectAndCompute(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  detectAndCompute

  ##### Positional Arguments
  - **self**: `Evision.CUDA.ORB.t()`
  - **image**: `Evision.Mat`
  - **mask**: `Evision.Mat`

  ##### Keyword Arguments
  - **useProvidedKeypoints**: `bool`.

  ##### Return
  - **keypoints**: `[Evision.KeyPoint]`
  - **descriptors**: `Evision.Mat.t()`.

  Detects keypoints and computes the descriptors

  Python prototype (for reference only):
  ```python3
  detectAndCompute(image, mask[, descriptors[, useProvidedKeypoints]]) -> keypoints, descriptors
  ```
  """
  @spec detectAndCompute(Evision.CUDA.ORB.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: {list(Evision.KeyPoint.t()), Evision.Mat.t()} | {:error, String.t()}
  def detectAndCompute(self, image, mask) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (is_struct(mask, Evision.Mat) or is_struct(mask, Nx.Tensor) or is_number(mask) or is_tuple(mask))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      mask: Evision.Internal.Structurise.from_struct(mask)
    ]
    :evision_nif.cuda_ORB_detectAndCompute(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  detectAndComputeAsync

  ##### Positional Arguments
  - **self**: `Evision.CUDA.ORB.t()`
  - **image**: `Evision.Mat`
  - **mask**: `Evision.Mat`

  ##### Keyword Arguments
  - **useProvidedKeypoints**: `bool`.
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **keypoints**: `Evision.Mat.t()`.
  - **descriptors**: `Evision.Mat.t()`.

  Detects keypoints and computes the descriptors.

  Python prototype (for reference only):
  ```python3
  detectAndComputeAsync(image, mask[, keypoints[, descriptors[, useProvidedKeypoints[, stream]]]]) -> keypoints, descriptors
  ```
  #### Variant 2:
  detectAndComputeAsync

  ##### Positional Arguments
  - **self**: `Evision.CUDA.ORB.t()`
  - **image**: `Evision.CUDA.GpuMat.t()`
  - **mask**: `Evision.CUDA.GpuMat.t()`

  ##### Keyword Arguments
  - **useProvidedKeypoints**: `bool`.
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **keypoints**: `Evision.CUDA.GpuMat.t()`.
  - **descriptors**: `Evision.CUDA.GpuMat.t()`.

  Detects keypoints and computes the descriptors.

  Python prototype (for reference only):
  ```python3
  detectAndComputeAsync(image, mask[, keypoints[, descriptors[, useProvidedKeypoints[, stream]]]]) -> keypoints, descriptors
  ```

  """
  @spec detectAndComputeAsync(Evision.CUDA.ORB.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:stream, term()} | {:useProvidedKeypoints, term()}] | nil) :: {Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def detectAndComputeAsync(self, image, mask, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (is_struct(mask, Evision.Mat) or is_struct(mask, Nx.Tensor) or is_number(mask) or is_tuple(mask)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream, :useProvidedKeypoints])
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      mask: Evision.Internal.Structurise.from_struct(mask)
    ]
    :evision_nif.cuda_cuda_ORB_detectAndComputeAsync(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec detectAndComputeAsync(Evision.CUDA.ORB.t(), Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t(), [{:stream, term()} | {:useProvidedKeypoints, term()}] | nil) :: {Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t()} | {:error, String.t()}
  def detectAndComputeAsync(self, image, mask, opts) when is_struct(image, Evision.CUDA.GpuMat) and is_struct(mask, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream, :useProvidedKeypoints])
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      mask: Evision.Internal.Structurise.from_struct(mask)
    ]
    :evision_nif.cuda_cuda_ORB_detectAndComputeAsync(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  detectAndComputeAsync

  ##### Positional Arguments
  - **self**: `Evision.CUDA.ORB.t()`
  - **image**: `Evision.Mat`
  - **mask**: `Evision.Mat`

  ##### Keyword Arguments
  - **useProvidedKeypoints**: `bool`.
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **keypoints**: `Evision.Mat.t()`.
  - **descriptors**: `Evision.Mat.t()`.

  Detects keypoints and computes the descriptors.

  Python prototype (for reference only):
  ```python3
  detectAndComputeAsync(image, mask[, keypoints[, descriptors[, useProvidedKeypoints[, stream]]]]) -> keypoints, descriptors
  ```
  #### Variant 2:
  detectAndComputeAsync

  ##### Positional Arguments
  - **self**: `Evision.CUDA.ORB.t()`
  - **image**: `Evision.CUDA.GpuMat.t()`
  - **mask**: `Evision.CUDA.GpuMat.t()`

  ##### Keyword Arguments
  - **useProvidedKeypoints**: `bool`.
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **keypoints**: `Evision.CUDA.GpuMat.t()`.
  - **descriptors**: `Evision.CUDA.GpuMat.t()`.

  Detects keypoints and computes the descriptors.

  Python prototype (for reference only):
  ```python3
  detectAndComputeAsync(image, mask[, keypoints[, descriptors[, useProvidedKeypoints[, stream]]]]) -> keypoints, descriptors
  ```

  """
  @spec detectAndComputeAsync(Evision.CUDA.ORB.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: {Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def detectAndComputeAsync(self, image, mask) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (is_struct(mask, Evision.Mat) or is_struct(mask, Nx.Tensor) or is_number(mask) or is_tuple(mask))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      mask: Evision.Internal.Structurise.from_struct(mask)
    ]
    :evision_nif.cuda_cuda_ORB_detectAndComputeAsync(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec detectAndComputeAsync(Evision.CUDA.ORB.t(), Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t()) :: {Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t()} | {:error, String.t()}
  def detectAndComputeAsync(self, image, mask) when is_struct(image, Evision.CUDA.GpuMat) and is_struct(mask, Evision.CUDA.GpuMat)
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      mask: Evision.Internal.Structurise.from_struct(mask)
    ]
    :evision_nif.cuda_cuda_ORB_detectAndComputeAsync(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Detects keypoints in an image.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.ORB.t()`
  - **image**: `Evision.Mat`.

    Image.

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.

    Mask specifying where to look for keypoints (optional). It must be a 8-bit integer
    matrix with non-zero values in the region of interest.

  - **stream**: `Evision.CUDA.Stream.t()`.

    CUDA stream.

  ##### Return
  - **keypoints**: `Evision.Mat.t()`.

    The detected keypoints.

  Python prototype (for reference only):
  ```python3
  detectAsync(image[, keypoints[, mask[, stream]]]) -> keypoints
  ```
  #### Variant 2:
  Detects keypoints in an image.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.ORB.t()`
  - **image**: `Evision.CUDA.GpuMat.t()`.

    Image.

  ##### Keyword Arguments
  - **mask**: `Evision.CUDA.GpuMat.t()`.

    Mask specifying where to look for keypoints (optional). It must be a 8-bit integer
    matrix with non-zero values in the region of interest.

  - **stream**: `Evision.CUDA.Stream.t()`.

    CUDA stream.

  ##### Return
  - **keypoints**: `Evision.CUDA.GpuMat.t()`.

    The detected keypoints.

  Python prototype (for reference only):
  ```python3
  detectAsync(image[, keypoints[, mask[, stream]]]) -> keypoints
  ```

  """
  @spec detectAsync(Evision.CUDA.ORB.t(), Evision.Mat.maybe_mat_in(), [{:mask, term()} | {:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def detectAsync(self, image, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:mask, :stream])
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.cuda_cuda_ORB_detectAsync(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec detectAsync(Evision.CUDA.ORB.t(), Evision.CUDA.GpuMat.t(), [{:mask, term()} | {:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def detectAsync(self, image, opts) when is_struct(image, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:mask, :stream])
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.cuda_cuda_ORB_detectAsync(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Detects keypoints in an image.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.ORB.t()`
  - **image**: `Evision.Mat`.

    Image.

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.

    Mask specifying where to look for keypoints (optional). It must be a 8-bit integer
    matrix with non-zero values in the region of interest.

  - **stream**: `Evision.CUDA.Stream.t()`.

    CUDA stream.

  ##### Return
  - **keypoints**: `Evision.Mat.t()`.

    The detected keypoints.

  Python prototype (for reference only):
  ```python3
  detectAsync(image[, keypoints[, mask[, stream]]]) -> keypoints
  ```
  #### Variant 2:
  Detects keypoints in an image.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.ORB.t()`
  - **image**: `Evision.CUDA.GpuMat.t()`.

    Image.

  ##### Keyword Arguments
  - **mask**: `Evision.CUDA.GpuMat.t()`.

    Mask specifying where to look for keypoints (optional). It must be a 8-bit integer
    matrix with non-zero values in the region of interest.

  - **stream**: `Evision.CUDA.Stream.t()`.

    CUDA stream.

  ##### Return
  - **keypoints**: `Evision.CUDA.GpuMat.t()`.

    The detected keypoints.

  Python prototype (for reference only):
  ```python3
  detectAsync(image[, keypoints[, mask[, stream]]]) -> keypoints
  ```

  """
  @spec detectAsync(Evision.CUDA.ORB.t(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def detectAsync(self, image) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.cuda_cuda_ORB_detectAsync(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec detectAsync(Evision.CUDA.ORB.t(), Evision.CUDA.GpuMat.t()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def detectAsync(self, image) when is_struct(image, Evision.CUDA.GpuMat)
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.cuda_cuda_ORB_detectAsync(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  empty

  ##### Positional Arguments
  - **self**: `Evision.CUDA.ORB.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.CUDA.ORB.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.cuda_ORB_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getBlurForDescriptor

  ##### Positional Arguments
  - **self**: `Evision.CUDA.ORB.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  getBlurForDescriptor() -> retval
  ```
  """
  @spec getBlurForDescriptor(Evision.CUDA.ORB.t()) :: boolean() | {:error, String.t()}
  def getBlurForDescriptor(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_ORB_getBlurForDescriptor(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.CUDA.ORB.t()`

  ##### Return
  - **retval**: `String`

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.CUDA.ORB.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.cuda_ORB_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getEdgeThreshold

  ##### Positional Arguments
  - **self**: `Evision.CUDA.ORB.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getEdgeThreshold() -> retval
  ```
  """
  @spec getEdgeThreshold(Evision.CUDA.ORB.t()) :: integer() | {:error, String.t()}
  def getEdgeThreshold(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_ORB_getEdgeThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getFastThreshold

  ##### Positional Arguments
  - **self**: `Evision.CUDA.ORB.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getFastThreshold() -> retval
  ```
  """
  @spec getFastThreshold(Evision.CUDA.ORB.t()) :: integer() | {:error, String.t()}
  def getFastThreshold(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_ORB_getFastThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getFirstLevel

  ##### Positional Arguments
  - **self**: `Evision.CUDA.ORB.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getFirstLevel() -> retval
  ```
  """
  @spec getFirstLevel(Evision.CUDA.ORB.t()) :: integer() | {:error, String.t()}
  def getFirstLevel(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_ORB_getFirstLevel(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMaxFeatures

  ##### Positional Arguments
  - **self**: `Evision.CUDA.ORB.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getMaxFeatures() -> retval
  ```
  """
  @spec getMaxFeatures(Evision.CUDA.ORB.t()) :: integer() | {:error, String.t()}
  def getMaxFeatures(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_ORB_getMaxFeatures(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getNLevels

  ##### Positional Arguments
  - **self**: `Evision.CUDA.ORB.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getNLevels() -> retval
  ```
  """
  @spec getNLevels(Evision.CUDA.ORB.t()) :: integer() | {:error, String.t()}
  def getNLevels(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_ORB_getNLevels(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getPatchSize

  ##### Positional Arguments
  - **self**: `Evision.CUDA.ORB.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getPatchSize() -> retval
  ```
  """
  @spec getPatchSize(Evision.CUDA.ORB.t()) :: integer() | {:error, String.t()}
  def getPatchSize(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_ORB_getPatchSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getScaleFactor

  ##### Positional Arguments
  - **self**: `Evision.CUDA.ORB.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getScaleFactor() -> retval
  ```
  """
  @spec getScaleFactor(Evision.CUDA.ORB.t()) :: number() | {:error, String.t()}
  def getScaleFactor(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_ORB_getScaleFactor(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getScoreType

  ##### Positional Arguments
  - **self**: `Evision.CUDA.ORB.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getScoreType() -> retval
  ```
  """
  @spec getScoreType(Evision.CUDA.ORB.t()) :: integer() | {:error, String.t()}
  def getScoreType(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_ORB_getScoreType(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getWTA_K

  ##### Positional Arguments
  - **self**: `Evision.CUDA.ORB.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getWTA_K() -> retval
  ```
  """
  @spec getWTA_K(Evision.CUDA.ORB.t()) :: integer() | {:error, String.t()}
  def getWTA_K(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_ORB_getWTA_K(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  read

  ##### Positional Arguments
  - **self**: `Evision.CUDA.ORB.t()`
  - **arg1**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(arg1) -> None
  ```
  #### Variant 2:
  read

  ##### Positional Arguments
  - **self**: `Evision.CUDA.ORB.t()`
  - **fileName**: `String`

  Python prototype (for reference only):
  ```python3
  read(fileName) -> None
  ```

  """
  @spec read(Evision.CUDA.ORB.t(), Evision.FileNode.t()) :: Evision.CUDA.ORB.t() | {:error, String.t()}
  def read(self, arg1) when is_struct(arg1, Evision.FileNode)
  do
    positional = [
      arg1: Evision.Internal.Structurise.from_struct(arg1)
    ]
    :evision_nif.cuda_ORB_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec read(Evision.CUDA.ORB.t(), binary()) :: Evision.CUDA.ORB.t() | {:error, String.t()}
  def read(self, fileName) when is_binary(fileName)
  do
    positional = [
      fileName: Evision.Internal.Structurise.from_struct(fileName)
    ]
    :evision_nif.cuda_ORB_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setBlurForDescriptor

  ##### Positional Arguments
  - **self**: `Evision.CUDA.ORB.t()`
  - **blurForDescriptor**: `bool`

  Python prototype (for reference only):
  ```python3
  setBlurForDescriptor(blurForDescriptor) -> None
  ```
  """
  @spec setBlurForDescriptor(Evision.CUDA.ORB.t(), boolean()) :: Evision.CUDA.ORB.t() | {:error, String.t()}
  def setBlurForDescriptor(self, blurForDescriptor) when is_boolean(blurForDescriptor)
  do
    positional = [
      blurForDescriptor: Evision.Internal.Structurise.from_struct(blurForDescriptor)
    ]
    :evision_nif.cuda_cuda_ORB_setBlurForDescriptor(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setEdgeThreshold

  ##### Positional Arguments
  - **self**: `Evision.CUDA.ORB.t()`
  - **edgeThreshold**: `integer()`

  Python prototype (for reference only):
  ```python3
  setEdgeThreshold(edgeThreshold) -> None
  ```
  """
  @spec setEdgeThreshold(Evision.CUDA.ORB.t(), integer()) :: Evision.CUDA.ORB.t() | {:error, String.t()}
  def setEdgeThreshold(self, edgeThreshold) when is_integer(edgeThreshold)
  do
    positional = [
      edgeThreshold: Evision.Internal.Structurise.from_struct(edgeThreshold)
    ]
    :evision_nif.cuda_cuda_ORB_setEdgeThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setFastThreshold

  ##### Positional Arguments
  - **self**: `Evision.CUDA.ORB.t()`
  - **fastThreshold**: `integer()`

  Python prototype (for reference only):
  ```python3
  setFastThreshold(fastThreshold) -> None
  ```
  """
  @spec setFastThreshold(Evision.CUDA.ORB.t(), integer()) :: Evision.CUDA.ORB.t() | {:error, String.t()}
  def setFastThreshold(self, fastThreshold) when is_integer(fastThreshold)
  do
    positional = [
      fastThreshold: Evision.Internal.Structurise.from_struct(fastThreshold)
    ]
    :evision_nif.cuda_cuda_ORB_setFastThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setFirstLevel

  ##### Positional Arguments
  - **self**: `Evision.CUDA.ORB.t()`
  - **firstLevel**: `integer()`

  Python prototype (for reference only):
  ```python3
  setFirstLevel(firstLevel) -> None
  ```
  """
  @spec setFirstLevel(Evision.CUDA.ORB.t(), integer()) :: Evision.CUDA.ORB.t() | {:error, String.t()}
  def setFirstLevel(self, firstLevel) when is_integer(firstLevel)
  do
    positional = [
      firstLevel: Evision.Internal.Structurise.from_struct(firstLevel)
    ]
    :evision_nif.cuda_cuda_ORB_setFirstLevel(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMaxFeatures

  ##### Positional Arguments
  - **self**: `Evision.CUDA.ORB.t()`
  - **maxFeatures**: `integer()`

  Python prototype (for reference only):
  ```python3
  setMaxFeatures(maxFeatures) -> None
  ```
  """
  @spec setMaxFeatures(Evision.CUDA.ORB.t(), integer()) :: Evision.CUDA.ORB.t() | {:error, String.t()}
  def setMaxFeatures(self, maxFeatures) when is_integer(maxFeatures)
  do
    positional = [
      maxFeatures: Evision.Internal.Structurise.from_struct(maxFeatures)
    ]
    :evision_nif.cuda_cuda_ORB_setMaxFeatures(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setNLevels

  ##### Positional Arguments
  - **self**: `Evision.CUDA.ORB.t()`
  - **nlevels**: `integer()`

  Python prototype (for reference only):
  ```python3
  setNLevels(nlevels) -> None
  ```
  """
  @spec setNLevels(Evision.CUDA.ORB.t(), integer()) :: Evision.CUDA.ORB.t() | {:error, String.t()}
  def setNLevels(self, nlevels) when is_integer(nlevels)
  do
    positional = [
      nlevels: Evision.Internal.Structurise.from_struct(nlevels)
    ]
    :evision_nif.cuda_cuda_ORB_setNLevels(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setPatchSize

  ##### Positional Arguments
  - **self**: `Evision.CUDA.ORB.t()`
  - **patchSize**: `integer()`

  Python prototype (for reference only):
  ```python3
  setPatchSize(patchSize) -> None
  ```
  """
  @spec setPatchSize(Evision.CUDA.ORB.t(), integer()) :: Evision.CUDA.ORB.t() | {:error, String.t()}
  def setPatchSize(self, patchSize) when is_integer(patchSize)
  do
    positional = [
      patchSize: Evision.Internal.Structurise.from_struct(patchSize)
    ]
    :evision_nif.cuda_cuda_ORB_setPatchSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setScaleFactor

  ##### Positional Arguments
  - **self**: `Evision.CUDA.ORB.t()`
  - **scaleFactor**: `double`

  Python prototype (for reference only):
  ```python3
  setScaleFactor(scaleFactor) -> None
  ```
  """
  @spec setScaleFactor(Evision.CUDA.ORB.t(), number()) :: Evision.CUDA.ORB.t() | {:error, String.t()}
  def setScaleFactor(self, scaleFactor) when is_number(scaleFactor)
  do
    positional = [
      scaleFactor: Evision.Internal.Structurise.from_struct(scaleFactor)
    ]
    :evision_nif.cuda_cuda_ORB_setScaleFactor(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setScoreType

  ##### Positional Arguments
  - **self**: `Evision.CUDA.ORB.t()`
  - **scoreType**: `integer()`

  Python prototype (for reference only):
  ```python3
  setScoreType(scoreType) -> None
  ```
  """
  @spec setScoreType(Evision.CUDA.ORB.t(), integer()) :: Evision.CUDA.ORB.t() | {:error, String.t()}
  def setScoreType(self, scoreType) when is_integer(scoreType)
  do
    positional = [
      scoreType: Evision.Internal.Structurise.from_struct(scoreType)
    ]
    :evision_nif.cuda_cuda_ORB_setScoreType(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setWTA_K

  ##### Positional Arguments
  - **self**: `Evision.CUDA.ORB.t()`
  - **wta_k**: `integer()`

  Python prototype (for reference only):
  ```python3
  setWTA_K(wta_k) -> None
  ```
  """
  @spec setWTA_K(Evision.CUDA.ORB.t(), integer()) :: Evision.CUDA.ORB.t() | {:error, String.t()}
  def setWTA_K(self, wta_k) when is_integer(wta_k)
  do
    positional = [
      wta_k: Evision.Internal.Structurise.from_struct(wta_k)
    ]
    :evision_nif.cuda_cuda_ORB_setWTA_K(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.CUDA.ORB.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.CUDA.ORB.t(), Evision.FileStorage.t(), binary()) :: Evision.CUDA.ORB.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.cuda_ORB_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.CUDA.ORB.t()`
  - **fileName**: `String`

  Python prototype (for reference only):
  ```python3
  write(fileName) -> None
  ```
  """
  @spec write(Evision.CUDA.ORB.t(), binary()) :: Evision.CUDA.ORB.t() | {:error, String.t()}
  def write(self, fileName) when is_binary(fileName)
  do
    positional = [
      fileName: Evision.Internal.Structurise.from_struct(fileName)
    ]
    :evision_nif.cuda_ORB_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
