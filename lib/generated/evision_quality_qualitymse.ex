defmodule Evision.Quality.QualityMSE do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Quality.QualityMSE` struct.

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
  def to_struct({:ok, %{class: Evision.Quality.QualityMSE, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Quality.QualityMSE, ref: ref}) do
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
  - **self**: `Evision.Quality.QualityMSE.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.Quality.QualityMSE.t()) :: Evision.Quality.QualityMSE.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.quality_quality_QualityMSE_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  static method for computing quality

  ##### Positional Arguments
  - **self**: `Evision.Quality.QualityMSE.t()`
  - **ref**: `Evision.Mat`.

    reference image

  - **cmp**: `Evision.Mat`.

    comparison image=

  ##### Return
  - **retval**: `Evision.scalar().t()`
  - **qualityMap**: `Evision.Mat.t()`.

    output quality map, or cv::noArray()

  @returns cv::Scalar with per-channel quality values.  Values range from 0 (best) to max float (worst)

  Python prototype (for reference only):
  ```python3
  compute(ref, cmp[, qualityMap]) -> retval, qualityMap
  ```
  """
  @spec compute(Evision.Quality.QualityMSE.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: {Evision.scalar(), Evision.Mat.t()} | {:error, String.t()}
  def compute(self, ref, cmp, opts) when (is_struct(ref, Evision.Mat) or is_struct(ref, Nx.Tensor) or is_number(ref) or is_tuple(ref)) and (is_struct(cmp, Evision.Mat) or is_struct(cmp, Nx.Tensor) or is_number(cmp) or is_tuple(cmp)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      ref: Evision.Internal.Structurise.from_struct(ref),
      cmp: Evision.Internal.Structurise.from_struct(cmp)
    ]
    :evision_nif.quality_quality_QualityMSE_compute(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  static method for computing quality

  ##### Positional Arguments
  - **self**: `Evision.Quality.QualityMSE.t()`
  - **ref**: `Evision.Mat`.

    reference image

  - **cmp**: `Evision.Mat`.

    comparison image=

  ##### Return
  - **retval**: `Evision.scalar().t()`
  - **qualityMap**: `Evision.Mat.t()`.

    output quality map, or cv::noArray()

  @returns cv::Scalar with per-channel quality values.  Values range from 0 (best) to max float (worst)

  Python prototype (for reference only):
  ```python3
  compute(ref, cmp[, qualityMap]) -> retval, qualityMap
  ```
  """
  @spec compute(Evision.Quality.QualityMSE.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: {Evision.scalar(), Evision.Mat.t()} | {:error, String.t()}
  def compute(self, ref, cmp) when (is_struct(ref, Evision.Mat) or is_struct(ref, Nx.Tensor) or is_number(ref) or is_tuple(ref)) and (is_struct(cmp, Evision.Mat) or is_struct(cmp, Nx.Tensor) or is_number(cmp) or is_tuple(cmp))
  do
    positional = [
      ref: Evision.Internal.Structurise.from_struct(ref),
      cmp: Evision.Internal.Structurise.from_struct(cmp)
    ]
    :evision_nif.quality_quality_QualityMSE_compute(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Computes MSE for reference images supplied in class constructor and provided comparison images

  ##### Positional Arguments
  - **self**: `Evision.Quality.QualityMSE.t()`
  - **cmpImgs**: `[Evision.Mat]`.

    Comparison image(s)

  ##### Return
  - **retval**: `Evision.scalar().t()`

  @returns cv::Scalar with per-channel quality values.  Values range from 0 (best) to potentially max float (worst)

  Python prototype (for reference only):
  ```python3
  compute(cmpImgs) -> retval
  ```
  """
  @spec compute(Evision.Quality.QualityMSE.t(), list(Evision.Mat.maybe_mat_in())) :: Evision.scalar() | {:error, String.t()}
  def compute(self, cmpImgs) when is_list(cmpImgs)
  do
    positional = [
      cmpImgs: Evision.Internal.Structurise.from_struct(cmpImgs)
    ]
    :evision_nif.quality_quality_QualityMSE_compute(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Create an object which calculates quality

  ##### Positional Arguments
  - **ref**: `Evision.Mat`.

    input image to use as the reference for comparison

  ##### Return
  - **retval**: `QualityMSE`

  Python prototype (for reference only):
  ```python3
  create(ref) -> retval
  ```
  """
  @spec create(Evision.Mat.maybe_mat_in()) :: Evision.Quality.QualityMSE.t() | {:error, String.t()}
  def create(ref) when (is_struct(ref, Evision.Mat) or is_struct(ref, Nx.Tensor) or is_number(ref) or is_tuple(ref))
  do
    positional = [
      ref: Evision.Internal.Structurise.from_struct(ref)
    ]
    :evision_nif.quality_quality_QualityMSE_create_static(positional)
    |> to_struct()
  end

  @doc """
  Implements Algorithm::empty()

  ##### Positional Arguments
  - **self**: `Evision.Quality.QualityMSE.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.Quality.QualityMSE.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.quality_quality_QualityMSE_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
