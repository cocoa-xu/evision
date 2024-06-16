defmodule Evision.Quality.QualityPSNR do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Quality.QualityPSNR` struct.

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
  def to_struct({:ok, %{class: Evision.Quality.QualityPSNR, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Quality.QualityPSNR, ref: ref}) do
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
  Implements Algorithm::clear()

  ##### Positional Arguments
  - **self**: `Evision.Quality.QualityPSNR.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.Quality.QualityPSNR.t()) :: Evision.Quality.QualityPSNR.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.quality_quality_QualityPSNR_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  static method for computing quality

  ##### Positional Arguments
  - **self**: `Evision.Quality.QualityPSNR.t()`
  - **ref**: `Evision.Mat`.

    reference image

  - **cmp**: `Evision.Mat`.

    comparison image

  ##### Keyword Arguments
  - **maxPixelValue**: `double`.

    maximum per-channel value for any individual pixel; eg 255 for uint8 image

  ##### Return
  - **retval**: `Evision.scalar().t()`
  - **qualityMap**: `Evision.Mat.t()`.

    output quality map, or cv::noArray()

  @returns PSNR value, or std::numeric_limits<double>::infinity() if the MSE between the two images == 0

  Python prototype (for reference only):
  ```python3
  compute(ref, cmp[, qualityMap[, maxPixelValue]]) -> retval, qualityMap
  ```
  """
  @spec compute(Evision.Quality.QualityPSNR.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:maxPixelValue, term()}] | nil) :: {Evision.scalar(), Evision.Mat.t()} | {:error, String.t()}
  def compute(self, ref, cmp, opts) when (is_struct(ref, Evision.Mat) or is_struct(ref, Nx.Tensor) or is_number(ref) or is_tuple(ref)) and (is_struct(cmp, Evision.Mat) or is_struct(cmp, Nx.Tensor) or is_number(cmp) or is_tuple(cmp)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:maxPixelValue])
    positional = [
      ref: Evision.Internal.Structurise.from_struct(ref),
      cmp: Evision.Internal.Structurise.from_struct(cmp)
    ]
    :evision_nif.quality_quality_QualityPSNR_compute(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  static method for computing quality

  ##### Positional Arguments
  - **self**: `Evision.Quality.QualityPSNR.t()`
  - **ref**: `Evision.Mat`.

    reference image

  - **cmp**: `Evision.Mat`.

    comparison image

  ##### Keyword Arguments
  - **maxPixelValue**: `double`.

    maximum per-channel value for any individual pixel; eg 255 for uint8 image

  ##### Return
  - **retval**: `Evision.scalar().t()`
  - **qualityMap**: `Evision.Mat.t()`.

    output quality map, or cv::noArray()

  @returns PSNR value, or std::numeric_limits<double>::infinity() if the MSE between the two images == 0

  Python prototype (for reference only):
  ```python3
  compute(ref, cmp[, qualityMap[, maxPixelValue]]) -> retval, qualityMap
  ```
  """
  @spec compute(Evision.Quality.QualityPSNR.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: {Evision.scalar(), Evision.Mat.t()} | {:error, String.t()}
  def compute(self, ref, cmp) when (is_struct(ref, Evision.Mat) or is_struct(ref, Nx.Tensor) or is_number(ref) or is_tuple(ref)) and (is_struct(cmp, Evision.Mat) or is_struct(cmp, Nx.Tensor) or is_number(cmp) or is_tuple(cmp))
  do
    positional = [
      ref: Evision.Internal.Structurise.from_struct(ref),
      cmp: Evision.Internal.Structurise.from_struct(cmp)
    ]
    :evision_nif.quality_quality_QualityPSNR_compute(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Compute the PSNR

  ##### Positional Arguments
  - **self**: `Evision.Quality.QualityPSNR.t()`
  - **cmp**: `Evision.Mat`.

    Comparison image

  ##### Return
  - **retval**: `Evision.scalar().t()`

  @returns Per-channel PSNR value, or std::numeric_limits<double>::infinity() if the MSE between the two images == 0

  Python prototype (for reference only):
  ```python3
  compute(cmp) -> retval
  ```
  """
  @spec compute(Evision.Quality.QualityPSNR.t(), Evision.Mat.maybe_mat_in()) :: Evision.scalar() | {:error, String.t()}
  def compute(self, cmp) when (is_struct(cmp, Evision.Mat) or is_struct(cmp, Nx.Tensor) or is_number(cmp) or is_tuple(cmp))
  do
    positional = [
      cmp: Evision.Internal.Structurise.from_struct(cmp)
    ]
    :evision_nif.quality_quality_QualityPSNR_compute(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Create an object which calculates quality

  ##### Positional Arguments
  - **ref**: `Evision.Mat`.

    input image to use as the source for comparison

  ##### Keyword Arguments
  - **maxPixelValue**: `double`.

    maximum per-channel value for any individual pixel; eg 255 for uint8 image

  ##### Return
  - **retval**: `QualityPSNR`

  Python prototype (for reference only):
  ```python3
  create(ref[, maxPixelValue]) -> retval
  ```
  """
  @spec create(Evision.Mat.maybe_mat_in(), [{:maxPixelValue, term()}] | nil) :: Evision.Quality.QualityPSNR.t() | {:error, String.t()}
  def create(ref, opts) when (is_struct(ref, Evision.Mat) or is_struct(ref, Nx.Tensor) or is_number(ref) or is_tuple(ref)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:maxPixelValue])
    positional = [
      ref: Evision.Internal.Structurise.from_struct(ref)
    ]
    :evision_nif.quality_quality_QualityPSNR_create_static(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Create an object which calculates quality

  ##### Positional Arguments
  - **ref**: `Evision.Mat`.

    input image to use as the source for comparison

  ##### Keyword Arguments
  - **maxPixelValue**: `double`.

    maximum per-channel value for any individual pixel; eg 255 for uint8 image

  ##### Return
  - **retval**: `QualityPSNR`

  Python prototype (for reference only):
  ```python3
  create(ref[, maxPixelValue]) -> retval
  ```
  """
  @spec create(Evision.Mat.maybe_mat_in()) :: Evision.Quality.QualityPSNR.t() | {:error, String.t()}
  def create(ref) when (is_struct(ref, Evision.Mat) or is_struct(ref, Nx.Tensor) or is_number(ref) or is_tuple(ref))
  do
    positional = [
      ref: Evision.Internal.Structurise.from_struct(ref)
    ]
    :evision_nif.quality_quality_QualityPSNR_create_static(positional)
    |> to_struct()
  end

  @doc """
  Implements Algorithm::empty()

  ##### Positional Arguments
  - **self**: `Evision.Quality.QualityPSNR.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.Quality.QualityPSNR.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.quality_quality_QualityPSNR_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  return the maximum pixel value used for PSNR computation

  ##### Positional Arguments
  - **self**: `Evision.Quality.QualityPSNR.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getMaxPixelValue() -> retval
  ```
  """
  @spec getMaxPixelValue(Evision.Quality.QualityPSNR.t()) :: number() | {:error, String.t()}
  def getMaxPixelValue(self) do
    positional = [
    ]
    :evision_nif.quality_quality_QualityPSNR_getMaxPixelValue(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  sets the maximum pixel value used for PSNR computation

  ##### Positional Arguments
  - **self**: `Evision.Quality.QualityPSNR.t()`
  - **val**: `double`.

    Maximum pixel value

  Python prototype (for reference only):
  ```python3
  setMaxPixelValue(val) -> None
  ```
  """
  @spec setMaxPixelValue(Evision.Quality.QualityPSNR.t(), number()) :: Evision.Quality.QualityPSNR.t() | {:error, String.t()}
  def setMaxPixelValue(self, val) when is_number(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.quality_quality_QualityPSNR_setMaxPixelValue(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
