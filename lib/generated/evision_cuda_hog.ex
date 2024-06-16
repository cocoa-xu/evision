defmodule Evision.CUDA.HOG do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `CUDA.HOG` struct.

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
  def to_struct({:ok, %{class: Evision.CUDA.HOG, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.CUDA.HOG, ref: ref}) do
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
  Clears the algorithm state

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HOG.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.CUDA.HOG.t()) :: Evision.CUDA.HOG.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.cuda_HOG_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Returns block descriptors computed for the whole image.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HOG.t()`
  - **img**: `Evision.Mat`.

    Source image. See cuda::HOGDescriptor::detect for type limitations.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    CUDA stream.

  ##### Return
  - **descriptors**: `Evision.Mat.t()`.

    2D array of descriptors.

  Python prototype (for reference only):
  ```python3
  compute(img[, descriptors[, stream]]) -> descriptors
  ```
  #### Variant 2:
  Returns block descriptors computed for the whole image.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HOG.t()`
  - **img**: `Evision.CUDA.GpuMat.t()`.

    Source image. See cuda::HOGDescriptor::detect for type limitations.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    CUDA stream.

  ##### Return
  - **descriptors**: `Evision.CUDA.GpuMat.t()`.

    2D array of descriptors.

  Python prototype (for reference only):
  ```python3
  compute(img[, descriptors[, stream]]) -> descriptors
  ```

  """
  @spec compute(Evision.CUDA.HOG.t(), Evision.Mat.maybe_mat_in(), [{:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def compute(self, img, opts) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      img: Evision.Internal.Structurise.from_struct(img)
    ]
    :evision_nif.cuda_cuda_HOG_compute(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec compute(Evision.CUDA.HOG.t(), Evision.CUDA.GpuMat.t(), [{:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def compute(self, img, opts) when is_struct(img, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      img: Evision.Internal.Structurise.from_struct(img)
    ]
    :evision_nif.cuda_cuda_HOG_compute(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Returns block descriptors computed for the whole image.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HOG.t()`
  - **img**: `Evision.Mat`.

    Source image. See cuda::HOGDescriptor::detect for type limitations.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    CUDA stream.

  ##### Return
  - **descriptors**: `Evision.Mat.t()`.

    2D array of descriptors.

  Python prototype (for reference only):
  ```python3
  compute(img[, descriptors[, stream]]) -> descriptors
  ```
  #### Variant 2:
  Returns block descriptors computed for the whole image.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HOG.t()`
  - **img**: `Evision.CUDA.GpuMat.t()`.

    Source image. See cuda::HOGDescriptor::detect for type limitations.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    CUDA stream.

  ##### Return
  - **descriptors**: `Evision.CUDA.GpuMat.t()`.

    2D array of descriptors.

  Python prototype (for reference only):
  ```python3
  compute(img[, descriptors[, stream]]) -> descriptors
  ```

  """
  @spec compute(Evision.CUDA.HOG.t(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def compute(self, img) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img)
    ]
    :evision_nif.cuda_cuda_HOG_compute(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec compute(Evision.CUDA.HOG.t(), Evision.CUDA.GpuMat.t()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def compute(self, img) when is_struct(img, Evision.CUDA.GpuMat)
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img)
    ]
    :evision_nif.cuda_cuda_HOG_compute(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Creates the HOG descriptor and detector.
  ##### Keyword Arguments
  - **win_size**: `Size`.

    Detection window size. Align to block size and block stride.

  - **block_size**: `Size`.

    Block size in pixels. Align to cell size. Only (16,16) is supported for now.

  - **block_stride**: `Size`.

    Block stride. It must be a multiple of cell size.

  - **cell_size**: `Size`.

    Cell size. Only (8, 8) is supported for now.

  - **nbins**: `integer()`.

    Number of bins. Only 9 bins per cell are supported for now.

  ##### Return
  - **retval**: `HOG`

  Python prototype (for reference only):
  ```python3
  create([, win_size[, block_size[, block_stride[, cell_size[, nbins]]]]]) -> retval
  ```
  """
  @spec create([{:block_size, term()} | {:block_stride, term()} | {:cell_size, term()} | {:nbins, term()} | {:win_size, term()}] | nil) :: Evision.CUDA.HOG.t() | {:error, String.t()}
  def create(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:block_size, :block_stride, :cell_size, :nbins, :win_size])
    positional = [
    ]
    :evision_nif.cuda_cuda_HOG_create_static(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Creates the HOG descriptor and detector.
  ##### Keyword Arguments
  - **win_size**: `Size`.

    Detection window size. Align to block size and block stride.

  - **block_size**: `Size`.

    Block size in pixels. Align to cell size. Only (16,16) is supported for now.

  - **block_stride**: `Size`.

    Block stride. It must be a multiple of cell size.

  - **cell_size**: `Size`.

    Cell size. Only (8, 8) is supported for now.

  - **nbins**: `integer()`.

    Number of bins. Only 9 bins per cell are supported for now.

  ##### Return
  - **retval**: `HOG`

  Python prototype (for reference only):
  ```python3
  create([, win_size[, block_size[, block_stride[, cell_size[, nbins]]]]]) -> retval
  ```
  """
  @spec create() :: Evision.CUDA.HOG.t() | {:error, String.t()}
  def create() do
    positional = [
    ]
    :evision_nif.cuda_cuda_HOG_create_static(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Performs object detection without a multi-scale window.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HOG.t()`
  - **img**: `Evision.Mat`.

    Source image. CV_8UC1 and CV_8UC4 types are supported for now.

  ##### Return
  - **found_locations**: `[Point]`.

    Left-top corner points of detected objects boundaries.

  - **confidences**: `[double]`.

    Optional output array for confidences.

  Python prototype (for reference only):
  ```python3
  detect(img) -> found_locations, confidences
  ```
  #### Variant 2:
  Performs object detection without a multi-scale window.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HOG.t()`
  - **img**: `Evision.CUDA.GpuMat.t()`.

    Source image. CV_8UC1 and CV_8UC4 types are supported for now.

  ##### Return
  - **found_locations**: `[Point]`.

    Left-top corner points of detected objects boundaries.

  - **confidences**: `[double]`.

    Optional output array for confidences.

  Python prototype (for reference only):
  ```python3
  detect(img) -> found_locations, confidences
  ```

  """
  @spec detect(Evision.CUDA.HOG.t(), Evision.Mat.maybe_mat_in()) :: {list({number(), number()}), list(number())} | {:error, String.t()}
  def detect(self, img) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img)
    ]
    :evision_nif.cuda_cuda_HOG_detect(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec detect(Evision.CUDA.HOG.t(), Evision.CUDA.GpuMat.t()) :: {list({number(), number()}), list(number())} | {:error, String.t()}
  def detect(self, img) when is_struct(img, Evision.CUDA.GpuMat)
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img)
    ]
    :evision_nif.cuda_cuda_HOG_detect(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Performs object detection with a multi-scale window.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HOG.t()`
  - **img**: `Evision.Mat`.

    Source image. See cuda::HOGDescriptor::detect for type limitations.

  ##### Return
  - **found_locations**: `[Rect]`.

    Detected objects boundaries.

  - **confidences**: `[double]`.

    Optional output array for confidences.

  Python prototype (for reference only):
  ```python3
  detectMultiScale(img) -> found_locations, confidences
  ```
  #### Variant 2:
  Performs object detection with a multi-scale window.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HOG.t()`
  - **img**: `Evision.CUDA.GpuMat.t()`.

    Source image. See cuda::HOGDescriptor::detect for type limitations.

  ##### Return
  - **found_locations**: `[Rect]`.

    Detected objects boundaries.

  - **confidences**: `[double]`.

    Optional output array for confidences.

  Python prototype (for reference only):
  ```python3
  detectMultiScale(img) -> found_locations, confidences
  ```

  """
  @spec detectMultiScale(Evision.CUDA.HOG.t(), Evision.Mat.maybe_mat_in()) :: {list({number(), number(), number(), number()}), list(number())} | {:error, String.t()}
  def detectMultiScale(self, img) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img)
    ]
    :evision_nif.cuda_cuda_HOG_detectMultiScale(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec detectMultiScale(Evision.CUDA.HOG.t(), Evision.CUDA.GpuMat.t()) :: {list({number(), number(), number(), number()}), list(number())} | {:error, String.t()}
  def detectMultiScale(self, img) when is_struct(img, Evision.CUDA.GpuMat)
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img)
    ]
    :evision_nif.cuda_cuda_HOG_detectMultiScale(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Performs object detection with a multi-scale window.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HOG.t()`
  - **img**: `Evision.Mat`.

    Source image. See cuda::HOGDescriptor::detect for type limitations.

  ##### Return
  - **found_locations**: `[Rect]`.

    Detected objects boundaries.

  Python prototype (for reference only):
  ```python3
  detectMultiScaleWithoutConf(img) -> found_locations
  ```
  #### Variant 2:
  Performs object detection with a multi-scale window.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HOG.t()`
  - **img**: `Evision.CUDA.GpuMat.t()`.

    Source image. See cuda::HOGDescriptor::detect for type limitations.

  ##### Return
  - **found_locations**: `[Rect]`.

    Detected objects boundaries.

  Python prototype (for reference only):
  ```python3
  detectMultiScaleWithoutConf(img) -> found_locations
  ```

  """
  @spec detectMultiScaleWithoutConf(Evision.CUDA.HOG.t(), Evision.Mat.maybe_mat_in()) :: list({number(), number(), number(), number()}) | {:error, String.t()}
  def detectMultiScaleWithoutConf(self, img) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img)
    ]
    :evision_nif.cuda_cuda_HOG_detectMultiScaleWithoutConf(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec detectMultiScaleWithoutConf(Evision.CUDA.HOG.t(), Evision.CUDA.GpuMat.t()) :: list({number(), number(), number(), number()}) | {:error, String.t()}
  def detectMultiScaleWithoutConf(self, img) when is_struct(img, Evision.CUDA.GpuMat)
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img)
    ]
    :evision_nif.cuda_cuda_HOG_detectMultiScaleWithoutConf(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Performs object detection without a multi-scale window.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HOG.t()`
  - **img**: `Evision.Mat`.

    Source image. CV_8UC1 and CV_8UC4 types are supported for now.

  ##### Return
  - **found_locations**: `[Point]`.

    Left-top corner points of detected objects boundaries.

  Python prototype (for reference only):
  ```python3
  detectWithoutConf(img) -> found_locations
  ```
  #### Variant 2:
  Performs object detection without a multi-scale window.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HOG.t()`
  - **img**: `Evision.CUDA.GpuMat.t()`.

    Source image. CV_8UC1 and CV_8UC4 types are supported for now.

  ##### Return
  - **found_locations**: `[Point]`.

    Left-top corner points of detected objects boundaries.

  Python prototype (for reference only):
  ```python3
  detectWithoutConf(img) -> found_locations
  ```

  """
  @spec detectWithoutConf(Evision.CUDA.HOG.t(), Evision.Mat.maybe_mat_in()) :: list({number(), number()}) | {:error, String.t()}
  def detectWithoutConf(self, img) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img)
    ]
    :evision_nif.cuda_cuda_HOG_detectWithoutConf(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec detectWithoutConf(Evision.CUDA.HOG.t(), Evision.CUDA.GpuMat.t()) :: list({number(), number()}) | {:error, String.t()}
  def detectWithoutConf(self, img) when is_struct(img, Evision.CUDA.GpuMat)
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img)
    ]
    :evision_nif.cuda_cuda_HOG_detectWithoutConf(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HOG.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.CUDA.HOG.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.cuda_HOG_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns the block histogram size.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HOG.t()`

  ##### Return
  - **retval**: `size_t`

  Python prototype (for reference only):
  ```python3
  getBlockHistogramSize() -> retval
  ```
  """
  @spec getBlockHistogramSize(Evision.CUDA.HOG.t()) :: integer() | {:error, String.t()}
  def getBlockHistogramSize(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_HOG_getBlockHistogramSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HOG.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.CUDA.HOG.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.cuda_HOG_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns coefficients of the classifier trained for people detection.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HOG.t()`

  ##### Return
  - **retval**: `Evision.Mat.t()`

  Python prototype (for reference only):
  ```python3
  getDefaultPeopleDetector() -> retval
  ```
  """
  @spec getDefaultPeopleDetector(Evision.CUDA.HOG.t()) :: Evision.Mat.t() | {:error, String.t()}
  def getDefaultPeopleDetector(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_HOG_getDefaultPeopleDetector(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDescriptorFormat

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HOG.t()`

  ##### Return
  - **retval**: `HOGDescriptor::DescriptorStorageFormat`

  Python prototype (for reference only):
  ```python3
  getDescriptorFormat() -> retval
  ```
  """
  @spec getDescriptorFormat(Evision.CUDA.HOG.t()) :: Evision.CUDA.HOGDescriptor_DescriptorStorageFormat.t() | {:error, String.t()}
  def getDescriptorFormat(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_HOG_getDescriptorFormat(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns the number of coefficients required for the classification.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HOG.t()`

  ##### Return
  - **retval**: `size_t`

  Python prototype (for reference only):
  ```python3
  getDescriptorSize() -> retval
  ```
  """
  @spec getDescriptorSize(Evision.CUDA.HOG.t()) :: integer() | {:error, String.t()}
  def getDescriptorSize(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_HOG_getDescriptorSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getGammaCorrection

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HOG.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  getGammaCorrection() -> retval
  ```
  """
  @spec getGammaCorrection(Evision.CUDA.HOG.t()) :: boolean() | {:error, String.t()}
  def getGammaCorrection(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_HOG_getGammaCorrection(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getGroupThreshold

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HOG.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getGroupThreshold() -> retval
  ```
  """
  @spec getGroupThreshold(Evision.CUDA.HOG.t()) :: integer() | {:error, String.t()}
  def getGroupThreshold(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_HOG_getGroupThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getHitThreshold

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HOG.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getHitThreshold() -> retval
  ```
  """
  @spec getHitThreshold(Evision.CUDA.HOG.t()) :: number() | {:error, String.t()}
  def getHitThreshold(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_HOG_getHitThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getL2HysThreshold

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HOG.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getL2HysThreshold() -> retval
  ```
  """
  @spec getL2HysThreshold(Evision.CUDA.HOG.t()) :: number() | {:error, String.t()}
  def getL2HysThreshold(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_HOG_getL2HysThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getNumLevels

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HOG.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getNumLevels() -> retval
  ```
  """
  @spec getNumLevels(Evision.CUDA.HOG.t()) :: integer() | {:error, String.t()}
  def getNumLevels(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_HOG_getNumLevels(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getScaleFactor

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HOG.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getScaleFactor() -> retval
  ```
  """
  @spec getScaleFactor(Evision.CUDA.HOG.t()) :: number() | {:error, String.t()}
  def getScaleFactor(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_HOG_getScaleFactor(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getWinSigma

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HOG.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getWinSigma() -> retval
  ```
  """
  @spec getWinSigma(Evision.CUDA.HOG.t()) :: number() | {:error, String.t()}
  def getWinSigma(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_HOG_getWinSigma(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getWinStride

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HOG.t()`

  ##### Return
  - **retval**: `Size`

  Python prototype (for reference only):
  ```python3
  getWinStride() -> retval
  ```
  """
  @spec getWinStride(Evision.CUDA.HOG.t()) :: {number(), number()} | {:error, String.t()}
  def getWinStride(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_HOG_getWinStride(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HOG.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.CUDA.HOG.t(), Evision.FileNode.t()) :: Evision.CUDA.HOG.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.cuda_HOG_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HOG.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.CUDA.HOG.t(), binary()) :: Evision.CUDA.HOG.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.cuda_HOG_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setDescriptorFormat

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HOG.t()`
  - **descr_format**: `HOGDescriptor_DescriptorStorageFormat`

  Python prototype (for reference only):
  ```python3
  setDescriptorFormat(descr_format) -> None
  ```
  """
  @spec setDescriptorFormat(Evision.CUDA.HOG.t(), Evision.CUDA.HOGDescriptor_DescriptorStorageFormat.t()) :: Evision.CUDA.HOG.t() | {:error, String.t()}
  def setDescriptorFormat(self, descr_format) when is_struct(descr_format, Evision.CUDA.HOGDescriptor_DescriptorStorageFormat)
  do
    positional = [
      descr_format: Evision.Internal.Structurise.from_struct(descr_format)
    ]
    :evision_nif.cuda_cuda_HOG_setDescriptorFormat(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setGammaCorrection

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HOG.t()`
  - **gamma_correction**: `bool`

  Python prototype (for reference only):
  ```python3
  setGammaCorrection(gamma_correction) -> None
  ```
  """
  @spec setGammaCorrection(Evision.CUDA.HOG.t(), boolean()) :: Evision.CUDA.HOG.t() | {:error, String.t()}
  def setGammaCorrection(self, gamma_correction) when is_boolean(gamma_correction)
  do
    positional = [
      gamma_correction: Evision.Internal.Structurise.from_struct(gamma_correction)
    ]
    :evision_nif.cuda_cuda_HOG_setGammaCorrection(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setGroupThreshold

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HOG.t()`
  - **group_threshold**: `integer()`

  Python prototype (for reference only):
  ```python3
  setGroupThreshold(group_threshold) -> None
  ```
  """
  @spec setGroupThreshold(Evision.CUDA.HOG.t(), integer()) :: Evision.CUDA.HOG.t() | {:error, String.t()}
  def setGroupThreshold(self, group_threshold) when is_integer(group_threshold)
  do
    positional = [
      group_threshold: Evision.Internal.Structurise.from_struct(group_threshold)
    ]
    :evision_nif.cuda_cuda_HOG_setGroupThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setHitThreshold

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HOG.t()`
  - **hit_threshold**: `double`

  Python prototype (for reference only):
  ```python3
  setHitThreshold(hit_threshold) -> None
  ```
  """
  @spec setHitThreshold(Evision.CUDA.HOG.t(), number()) :: Evision.CUDA.HOG.t() | {:error, String.t()}
  def setHitThreshold(self, hit_threshold) when is_number(hit_threshold)
  do
    positional = [
      hit_threshold: Evision.Internal.Structurise.from_struct(hit_threshold)
    ]
    :evision_nif.cuda_cuda_HOG_setHitThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setL2HysThreshold

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HOG.t()`
  - **threshold_L2hys**: `double`

  Python prototype (for reference only):
  ```python3
  setL2HysThreshold(threshold_L2hys) -> None
  ```
  """
  @spec setL2HysThreshold(Evision.CUDA.HOG.t(), number()) :: Evision.CUDA.HOG.t() | {:error, String.t()}
  def setL2HysThreshold(self, threshold_L2hys) when is_number(threshold_L2hys)
  do
    positional = [
      threshold_L2hys: Evision.Internal.Structurise.from_struct(threshold_L2hys)
    ]
    :evision_nif.cuda_cuda_HOG_setL2HysThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setNumLevels

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HOG.t()`
  - **nlevels**: `integer()`

  Python prototype (for reference only):
  ```python3
  setNumLevels(nlevels) -> None
  ```
  """
  @spec setNumLevels(Evision.CUDA.HOG.t(), integer()) :: Evision.CUDA.HOG.t() | {:error, String.t()}
  def setNumLevels(self, nlevels) when is_integer(nlevels)
  do
    positional = [
      nlevels: Evision.Internal.Structurise.from_struct(nlevels)
    ]
    :evision_nif.cuda_cuda_HOG_setNumLevels(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Sets coefficients for the linear SVM classifier.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HOG.t()`
  - **detector**: `Evision.Mat`

  Python prototype (for reference only):
  ```python3
  setSVMDetector(detector) -> None
  ```
  #### Variant 2:
  Sets coefficients for the linear SVM classifier.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HOG.t()`
  - **detector**: `Evision.CUDA.GpuMat.t()`

  Python prototype (for reference only):
  ```python3
  setSVMDetector(detector) -> None
  ```

  """
  @spec setSVMDetector(Evision.CUDA.HOG.t(), Evision.Mat.maybe_mat_in()) :: Evision.CUDA.HOG.t() | {:error, String.t()}
  def setSVMDetector(self, detector) when (is_struct(detector, Evision.Mat) or is_struct(detector, Nx.Tensor) or is_number(detector) or is_tuple(detector))
  do
    positional = [
      detector: Evision.Internal.Structurise.from_struct(detector)
    ]
    :evision_nif.cuda_cuda_HOG_setSVMDetector(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec setSVMDetector(Evision.CUDA.HOG.t(), Evision.CUDA.GpuMat.t()) :: Evision.CUDA.HOG.t() | {:error, String.t()}
  def setSVMDetector(self, detector) when is_struct(detector, Evision.CUDA.GpuMat)
  do
    positional = [
      detector: Evision.Internal.Structurise.from_struct(detector)
    ]
    :evision_nif.cuda_cuda_HOG_setSVMDetector(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setScaleFactor

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HOG.t()`
  - **scale0**: `double`

  Python prototype (for reference only):
  ```python3
  setScaleFactor(scale0) -> None
  ```
  """
  @spec setScaleFactor(Evision.CUDA.HOG.t(), number()) :: Evision.CUDA.HOG.t() | {:error, String.t()}
  def setScaleFactor(self, scale0) when is_number(scale0)
  do
    positional = [
      scale0: Evision.Internal.Structurise.from_struct(scale0)
    ]
    :evision_nif.cuda_cuda_HOG_setScaleFactor(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setWinSigma

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HOG.t()`
  - **win_sigma**: `double`

  Python prototype (for reference only):
  ```python3
  setWinSigma(win_sigma) -> None
  ```
  """
  @spec setWinSigma(Evision.CUDA.HOG.t(), number()) :: Evision.CUDA.HOG.t() | {:error, String.t()}
  def setWinSigma(self, win_sigma) when is_number(win_sigma)
  do
    positional = [
      win_sigma: Evision.Internal.Structurise.from_struct(win_sigma)
    ]
    :evision_nif.cuda_cuda_HOG_setWinSigma(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setWinStride

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HOG.t()`
  - **win_stride**: `Size`

  Python prototype (for reference only):
  ```python3
  setWinStride(win_stride) -> None
  ```
  """
  @spec setWinStride(Evision.CUDA.HOG.t(), {number(), number()}) :: Evision.CUDA.HOG.t() | {:error, String.t()}
  def setWinStride(self, win_stride) when is_tuple(win_stride)
  do
    positional = [
      win_stride: Evision.Internal.Structurise.from_struct(win_stride)
    ]
    :evision_nif.cuda_cuda_HOG_setWinStride(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HOG.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.CUDA.HOG.t(), Evision.FileStorage.t(), binary()) :: Evision.CUDA.HOG.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.cuda_HOG_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HOG.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.CUDA.HOG.t(), Evision.FileStorage.t()) :: Evision.CUDA.HOG.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.cuda_HOG_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
