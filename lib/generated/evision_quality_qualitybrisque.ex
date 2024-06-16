defmodule Evision.Quality.QualityBRISQUE do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Quality.QualityBRISQUE` struct.

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
  def to_struct({:ok, %{class: Evision.Quality.QualityBRISQUE, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Quality.QualityBRISQUE, ref: ref}) do
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
  static method for computing quality

  ##### Positional Arguments
  - **self**: `Evision.Quality.QualityBRISQUE.t()`
  - **img**: `Evision.Mat`.

    image for which to compute quality

  - **model_file_path**: `String`.

    cv::String which contains a path to the BRISQUE model data, eg. /path/to/brisque_model_live.yml

  - **range_file_path**: `String`.

    cv::String which contains a path to the BRISQUE range data, eg. /path/to/brisque_range_live.yml

  ##### Return
  - **retval**: `Evision.scalar().t()`

  @returns cv::Scalar with the score in the first element.  The score ranges from 0 (best quality) to 100 (worst quality)

  Python prototype (for reference only):
  ```python3
  compute(img, model_file_path, range_file_path) -> retval
  ```
  """
  @spec compute(Evision.Quality.QualityBRISQUE.t(), Evision.Mat.maybe_mat_in(), binary(), binary()) :: Evision.scalar() | {:error, String.t()}
  def compute(self, img, model_file_path, range_file_path) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img)) and is_binary(model_file_path) and is_binary(range_file_path)
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img),
      model_file_path: Evision.Internal.Structurise.from_struct(model_file_path),
      range_file_path: Evision.Internal.Structurise.from_struct(range_file_path)
    ]
    :evision_nif.quality_quality_QualityBRISQUE_compute(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Computes BRISQUE quality score for input image

  ##### Positional Arguments
  - **self**: `Evision.Quality.QualityBRISQUE.t()`
  - **img**: `Evision.Mat`.

    Image for which to compute quality

  ##### Return
  - **retval**: `Evision.scalar().t()`

  @returns cv::Scalar with the score in the first element.  The score ranges from 0 (best quality) to 100 (worst quality)

  Python prototype (for reference only):
  ```python3
  compute(img) -> retval
  ```
  """
  @spec compute(Evision.Quality.QualityBRISQUE.t(), Evision.Mat.maybe_mat_in()) :: Evision.scalar() | {:error, String.t()}
  def compute(self, img) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img)
    ]
    :evision_nif.quality_quality_QualityBRISQUE_compute(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  static method for computing image features used by the BRISQUE algorithm

  ##### Positional Arguments
  - **img**: `Evision.Mat`.

    image (BGR(A) or grayscale) for which to compute features

  ##### Return
  - **features**: `Evision.Mat.t()`.

    output row vector of features to cv::Mat or cv::UMat

  Python prototype (for reference only):
  ```python3
  computeFeatures(img[, features]) -> features
  ```
  """
  @spec computeFeatures(Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def computeFeatures(img, opts) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img)
    ]
    :evision_nif.quality_quality_QualityBRISQUE_computeFeatures_static(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  static method for computing image features used by the BRISQUE algorithm

  ##### Positional Arguments
  - **img**: `Evision.Mat`.

    image (BGR(A) or grayscale) for which to compute features

  ##### Return
  - **features**: `Evision.Mat.t()`.

    output row vector of features to cv::Mat or cv::UMat

  Python prototype (for reference only):
  ```python3
  computeFeatures(img[, features]) -> features
  ```
  """
  @spec computeFeatures(Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def computeFeatures(img) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img)
    ]
    :evision_nif.quality_quality_QualityBRISQUE_computeFeatures_static(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Create an object which calculates quality

  ##### Positional Arguments
  - **model**: `ml::SVM`.

    cv::Ptr<cv::ml::SVM> which contains a loaded BRISQUE model

  - **range**: `Evision.Mat`.

    cv::Mat which contains BRISQUE range data

  ##### Return
  - **retval**: `QualityBRISQUE`

  Python prototype (for reference only):
  ```python3
  create(model, range) -> retval
  ```
  #### Variant 2:
  Create an object which calculates quality

  ##### Positional Arguments
  - **model_file_path**: `String`.

    cv::String which contains a path to the BRISQUE model data, eg. /path/to/brisque_model_live.yml

  - **range_file_path**: `String`.

    cv::String which contains a path to the BRISQUE range data, eg. /path/to/brisque_range_live.yml

  ##### Return
  - **retval**: `QualityBRISQUE`

  Python prototype (for reference only):
  ```python3
  create(model_file_path, range_file_path) -> retval
  ```

  """
  @spec create(Evision.ML.SVM.t(), Evision.Mat.maybe_mat_in()) :: Evision.Quality.QualityBRISQUE.t() | {:error, String.t()}
  def create(model, range) when is_struct(model, Evision.ML.SVM) and (is_struct(range, Evision.Mat) or is_struct(range, Nx.Tensor) or is_number(range) or is_tuple(range))
  do
    positional = [
      model: Evision.Internal.Structurise.from_struct(model),
      range: Evision.Internal.Structurise.from_struct(range)
    ]
    :evision_nif.quality_quality_QualityBRISQUE_create_static(positional)
    |> to_struct()
  end
  @spec create(binary(), binary()) :: Evision.Quality.QualityBRISQUE.t() | {:error, String.t()}
  def create(model_file_path, range_file_path) when is_binary(model_file_path) and is_binary(range_file_path)
  do
    positional = [
      model_file_path: Evision.Internal.Structurise.from_struct(model_file_path),
      range_file_path: Evision.Internal.Structurise.from_struct(range_file_path)
    ]
    :evision_nif.quality_quality_QualityBRISQUE_create_static(positional)
    |> to_struct()
  end
end
