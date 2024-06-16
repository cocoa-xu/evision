defmodule Evision.Quality.QualitySSIM do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Quality.QualitySSIM` struct.

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
  def to_struct({:ok, %{class: Evision.Quality.QualitySSIM, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Quality.QualitySSIM, ref: ref}) do
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
  - **self**: `Evision.Quality.QualitySSIM.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.Quality.QualitySSIM.t()) :: Evision.Quality.QualitySSIM.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.quality_quality_QualitySSIM_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  static method for computing quality

  ##### Positional Arguments
  - **self**: `Evision.Quality.QualitySSIM.t()`
  - **ref**: `Evision.Mat`.

    reference image

  - **cmp**: `Evision.Mat`.

    comparison image

  ##### Return
  - **retval**: `Evision.scalar().t()`
  - **qualityMap**: `Evision.Mat.t()`.

    output quality map, or cv::noArray()

  @returns cv::Scalar with per-channel quality values.  Values range from 0 (worst) to 1 (best)

  Python prototype (for reference only):
  ```python3
  compute(ref, cmp[, qualityMap]) -> retval, qualityMap
  ```
  """
  @spec compute(Evision.Quality.QualitySSIM.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: {Evision.scalar(), Evision.Mat.t()} | {:error, String.t()}
  def compute(self, ref, cmp, opts) when (is_struct(ref, Evision.Mat) or is_struct(ref, Nx.Tensor) or is_number(ref) or is_tuple(ref)) and (is_struct(cmp, Evision.Mat) or is_struct(cmp, Nx.Tensor) or is_number(cmp) or is_tuple(cmp)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      ref: Evision.Internal.Structurise.from_struct(ref),
      cmp: Evision.Internal.Structurise.from_struct(cmp)
    ]
    :evision_nif.quality_quality_QualitySSIM_compute(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  static method for computing quality

  ##### Positional Arguments
  - **self**: `Evision.Quality.QualitySSIM.t()`
  - **ref**: `Evision.Mat`.

    reference image

  - **cmp**: `Evision.Mat`.

    comparison image

  ##### Return
  - **retval**: `Evision.scalar().t()`
  - **qualityMap**: `Evision.Mat.t()`.

    output quality map, or cv::noArray()

  @returns cv::Scalar with per-channel quality values.  Values range from 0 (worst) to 1 (best)

  Python prototype (for reference only):
  ```python3
  compute(ref, cmp[, qualityMap]) -> retval, qualityMap
  ```
  """
  @spec compute(Evision.Quality.QualitySSIM.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: {Evision.scalar(), Evision.Mat.t()} | {:error, String.t()}
  def compute(self, ref, cmp) when (is_struct(ref, Evision.Mat) or is_struct(ref, Nx.Tensor) or is_number(ref) or is_tuple(ref)) and (is_struct(cmp, Evision.Mat) or is_struct(cmp, Nx.Tensor) or is_number(cmp) or is_tuple(cmp))
  do
    positional = [
      ref: Evision.Internal.Structurise.from_struct(ref),
      cmp: Evision.Internal.Structurise.from_struct(cmp)
    ]
    :evision_nif.quality_quality_QualitySSIM_compute(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Computes SSIM

  ##### Positional Arguments
  - **self**: `Evision.Quality.QualitySSIM.t()`
  - **cmp**: `Evision.Mat`.

    Comparison image

  ##### Return
  - **retval**: `Evision.scalar().t()`

  @returns cv::Scalar with per-channel quality values.  Values range from 0 (worst) to 1 (best)

  Python prototype (for reference only):
  ```python3
  compute(cmp) -> retval
  ```
  """
  @spec compute(Evision.Quality.QualitySSIM.t(), Evision.Mat.maybe_mat_in()) :: Evision.scalar() | {:error, String.t()}
  def compute(self, cmp) when (is_struct(cmp, Evision.Mat) or is_struct(cmp, Nx.Tensor) or is_number(cmp) or is_tuple(cmp))
  do
    positional = [
      cmp: Evision.Internal.Structurise.from_struct(cmp)
    ]
    :evision_nif.quality_quality_QualitySSIM_compute(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Create an object which calculates quality

  ##### Positional Arguments
  - **ref**: `Evision.Mat`.

    input image to use as the reference image for comparison

  ##### Return
  - **retval**: `QualitySSIM`

  Python prototype (for reference only):
  ```python3
  create(ref) -> retval
  ```
  """
  @spec create(Evision.Mat.maybe_mat_in()) :: Evision.Quality.QualitySSIM.t() | {:error, String.t()}
  def create(ref) when (is_struct(ref, Evision.Mat) or is_struct(ref, Nx.Tensor) or is_number(ref) or is_tuple(ref))
  do
    positional = [
      ref: Evision.Internal.Structurise.from_struct(ref)
    ]
    :evision_nif.quality_quality_QualitySSIM_create_static(positional)
    |> to_struct()
  end

  @doc """
  Implements Algorithm::empty()

  ##### Positional Arguments
  - **self**: `Evision.Quality.QualitySSIM.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.Quality.QualitySSIM.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.quality_quality_QualitySSIM_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
