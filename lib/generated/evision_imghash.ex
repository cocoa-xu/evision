defmodule Evision.ImgHash do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `ImgHash` struct.

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
  def to_struct({:ok, %{class: Evision.ImgHash, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.ImgHash, ref: ref}) do
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
  Calculates img_hash::AverageHash in one call

  ##### Positional Arguments
  - **inputArr**: `Evision.Mat`.

    input image want to compute hash value, type should be CV_8UC4, CV_8UC3 or CV_8UC1.

  ##### Return
  - **outputArr**: `Evision.Mat.t()`.

    Hash value of input, it will contain 16 hex decimal number, return type is CV_8U

  Python prototype (for reference only):
  ```python3
  averageHash(inputArr[, outputArr]) -> outputArr
  ```
  """
  @spec averageHash(Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def averageHash(inputArr, opts) when (is_struct(inputArr, Evision.Mat) or is_struct(inputArr, Nx.Tensor) or is_number(inputArr) or is_tuple(inputArr)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      inputArr: Evision.Internal.Structurise.from_struct(inputArr)
    ]
    :evision_nif.img_hash_averageHash(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Calculates img_hash::AverageHash in one call

  ##### Positional Arguments
  - **inputArr**: `Evision.Mat`.

    input image want to compute hash value, type should be CV_8UC4, CV_8UC3 or CV_8UC1.

  ##### Return
  - **outputArr**: `Evision.Mat.t()`.

    Hash value of input, it will contain 16 hex decimal number, return type is CV_8U

  Python prototype (for reference only):
  ```python3
  averageHash(inputArr[, outputArr]) -> outputArr
  ```
  """
  @spec averageHash(Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def averageHash(inputArr) when (is_struct(inputArr, Evision.Mat) or is_struct(inputArr, Nx.Tensor) or is_number(inputArr) or is_tuple(inputArr))
  do
    positional = [
      inputArr: Evision.Internal.Structurise.from_struct(inputArr)
    ]
    :evision_nif.img_hash_averageHash(positional)
    |> to_struct()
  end

  @doc """
  Computes block mean hash of the input image

  ##### Positional Arguments
  - **inputArr**: `Evision.Mat`.

    input image want to compute hash value, type should be CV_8UC4, CV_8UC3 or CV_8UC1.

  ##### Keyword Arguments
  - **mode**: `integer()`.

    the mode

  ##### Return
  - **outputArr**: `Evision.Mat.t()`.

    Hash value of input, it will contain 16 hex decimal number, return type is CV_8U

  Python prototype (for reference only):
  ```python3
  blockMeanHash(inputArr[, outputArr[, mode]]) -> outputArr
  ```
  """
  @spec blockMeanHash(Evision.Mat.maybe_mat_in(), [{:mode, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def blockMeanHash(inputArr, opts) when (is_struct(inputArr, Evision.Mat) or is_struct(inputArr, Nx.Tensor) or is_number(inputArr) or is_tuple(inputArr)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:mode])
    positional = [
      inputArr: Evision.Internal.Structurise.from_struct(inputArr)
    ]
    :evision_nif.img_hash_blockMeanHash(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Computes block mean hash of the input image

  ##### Positional Arguments
  - **inputArr**: `Evision.Mat`.

    input image want to compute hash value, type should be CV_8UC4, CV_8UC3 or CV_8UC1.

  ##### Keyword Arguments
  - **mode**: `integer()`.

    the mode

  ##### Return
  - **outputArr**: `Evision.Mat.t()`.

    Hash value of input, it will contain 16 hex decimal number, return type is CV_8U

  Python prototype (for reference only):
  ```python3
  blockMeanHash(inputArr[, outputArr[, mode]]) -> outputArr
  ```
  """
  @spec blockMeanHash(Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def blockMeanHash(inputArr) when (is_struct(inputArr, Evision.Mat) or is_struct(inputArr, Nx.Tensor) or is_number(inputArr) or is_tuple(inputArr))
  do
    positional = [
      inputArr: Evision.Internal.Structurise.from_struct(inputArr)
    ]
    :evision_nif.img_hash_blockMeanHash(positional)
    |> to_struct()
  end

  @doc """
  Computes color moment hash of the input, the algorithm
  is come from the paper "Perceptual  Hashing  for  Color  Images
  Using  Invariant Moments"

  ##### Positional Arguments
  - **inputArr**: `Evision.Mat`.

    input image want to compute hash value,
    type should be CV_8UC4, CV_8UC3 or CV_8UC1.

  ##### Return
  - **outputArr**: `Evision.Mat.t()`.

    42 hash values with type CV_64F(double)

  Python prototype (for reference only):
  ```python3
  colorMomentHash(inputArr[, outputArr]) -> outputArr
  ```
  """
  @spec colorMomentHash(Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def colorMomentHash(inputArr, opts) when (is_struct(inputArr, Evision.Mat) or is_struct(inputArr, Nx.Tensor) or is_number(inputArr) or is_tuple(inputArr)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      inputArr: Evision.Internal.Structurise.from_struct(inputArr)
    ]
    :evision_nif.img_hash_colorMomentHash(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Computes color moment hash of the input, the algorithm
  is come from the paper "Perceptual  Hashing  for  Color  Images
  Using  Invariant Moments"

  ##### Positional Arguments
  - **inputArr**: `Evision.Mat`.

    input image want to compute hash value,
    type should be CV_8UC4, CV_8UC3 or CV_8UC1.

  ##### Return
  - **outputArr**: `Evision.Mat.t()`.

    42 hash values with type CV_64F(double)

  Python prototype (for reference only):
  ```python3
  colorMomentHash(inputArr[, outputArr]) -> outputArr
  ```
  """
  @spec colorMomentHash(Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def colorMomentHash(inputArr) when (is_struct(inputArr, Evision.Mat) or is_struct(inputArr, Nx.Tensor) or is_number(inputArr) or is_tuple(inputArr))
  do
    positional = [
      inputArr: Evision.Internal.Structurise.from_struct(inputArr)
    ]
    :evision_nif.img_hash_colorMomentHash(positional)
    |> to_struct()
  end

  @doc """
  Computes average hash value of the input image

  ##### Positional Arguments
  - **inputArr**: `Evision.Mat`.

    input image want to compute hash value,
    type should be CV_8UC4, CV_8UC3, CV_8UC1.

  ##### Keyword Arguments
  - **alpha**: `float`.

    int scale factor for marr wavelet (default=2).

  - **scale**: `float`.

    int level of scale factor (default = 1)

  ##### Return
  - **outputArr**: `Evision.Mat.t()`.

    Hash value of input, it will contain 16 hex
    decimal number, return type is CV_8U

  Python prototype (for reference only):
  ```python3
  marrHildrethHash(inputArr[, outputArr[, alpha[, scale]]]) -> outputArr
  ```
  """
  @spec marrHildrethHash(Evision.Mat.maybe_mat_in(), [{:alpha, term()} | {:scale, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def marrHildrethHash(inputArr, opts) when (is_struct(inputArr, Evision.Mat) or is_struct(inputArr, Nx.Tensor) or is_number(inputArr) or is_tuple(inputArr)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:alpha, :scale])
    positional = [
      inputArr: Evision.Internal.Structurise.from_struct(inputArr)
    ]
    :evision_nif.img_hash_marrHildrethHash(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Computes average hash value of the input image

  ##### Positional Arguments
  - **inputArr**: `Evision.Mat`.

    input image want to compute hash value,
    type should be CV_8UC4, CV_8UC3, CV_8UC1.

  ##### Keyword Arguments
  - **alpha**: `float`.

    int scale factor for marr wavelet (default=2).

  - **scale**: `float`.

    int level of scale factor (default = 1)

  ##### Return
  - **outputArr**: `Evision.Mat.t()`.

    Hash value of input, it will contain 16 hex
    decimal number, return type is CV_8U

  Python prototype (for reference only):
  ```python3
  marrHildrethHash(inputArr[, outputArr[, alpha[, scale]]]) -> outputArr
  ```
  """
  @spec marrHildrethHash(Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def marrHildrethHash(inputArr) when (is_struct(inputArr, Evision.Mat) or is_struct(inputArr, Nx.Tensor) or is_number(inputArr) or is_tuple(inputArr))
  do
    positional = [
      inputArr: Evision.Internal.Structurise.from_struct(inputArr)
    ]
    :evision_nif.img_hash_marrHildrethHash(positional)
    |> to_struct()
  end

  @doc """
  Computes pHash value of the input image

  ##### Positional Arguments
  - **inputArr**: `Evision.Mat`.

    input image want to compute hash value,
    type should be CV_8UC4, CV_8UC3, CV_8UC1.

  ##### Return
  - **outputArr**: `Evision.Mat.t()`.

    Hash value of input, it will contain 8 uchar value

  Python prototype (for reference only):
  ```python3
  pHash(inputArr[, outputArr]) -> outputArr
  ```
  """
  @spec pHash(Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def pHash(inputArr, opts) when (is_struct(inputArr, Evision.Mat) or is_struct(inputArr, Nx.Tensor) or is_number(inputArr) or is_tuple(inputArr)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      inputArr: Evision.Internal.Structurise.from_struct(inputArr)
    ]
    :evision_nif.img_hash_pHash(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Computes pHash value of the input image

  ##### Positional Arguments
  - **inputArr**: `Evision.Mat`.

    input image want to compute hash value,
    type should be CV_8UC4, CV_8UC3, CV_8UC1.

  ##### Return
  - **outputArr**: `Evision.Mat.t()`.

    Hash value of input, it will contain 8 uchar value

  Python prototype (for reference only):
  ```python3
  pHash(inputArr[, outputArr]) -> outputArr
  ```
  """
  @spec pHash(Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def pHash(inputArr) when (is_struct(inputArr, Evision.Mat) or is_struct(inputArr, Nx.Tensor) or is_number(inputArr) or is_tuple(inputArr))
  do
    positional = [
      inputArr: Evision.Internal.Structurise.from_struct(inputArr)
    ]
    :evision_nif.img_hash_pHash(positional)
    |> to_struct()
  end

  @doc """
  Computes radial variance hash of the input image

  ##### Positional Arguments
  - **inputArr**: `Evision.Mat`.

    input image want to compute hash value,
    type should be CV_8UC4, CV_8UC3, CV_8UC1.

  ##### Keyword Arguments
  - **sigma**: `double`.

    Gaussian kernel standard deviation

  - **numOfAngleLine**: `integer()`.

    The number of angles to consider

  ##### Return
  - **outputArr**: `Evision.Mat.t()`.

    Hash value of input

  Python prototype (for reference only):
  ```python3
  radialVarianceHash(inputArr[, outputArr[, sigma[, numOfAngleLine]]]) -> outputArr
  ```
  """
  @spec radialVarianceHash(Evision.Mat.maybe_mat_in(), [{:numOfAngleLine, term()} | {:sigma, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def radialVarianceHash(inputArr, opts) when (is_struct(inputArr, Evision.Mat) or is_struct(inputArr, Nx.Tensor) or is_number(inputArr) or is_tuple(inputArr)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:numOfAngleLine, :sigma])
    positional = [
      inputArr: Evision.Internal.Structurise.from_struct(inputArr)
    ]
    :evision_nif.img_hash_radialVarianceHash(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Computes radial variance hash of the input image

  ##### Positional Arguments
  - **inputArr**: `Evision.Mat`.

    input image want to compute hash value,
    type should be CV_8UC4, CV_8UC3, CV_8UC1.

  ##### Keyword Arguments
  - **sigma**: `double`.

    Gaussian kernel standard deviation

  - **numOfAngleLine**: `integer()`.

    The number of angles to consider

  ##### Return
  - **outputArr**: `Evision.Mat.t()`.

    Hash value of input

  Python prototype (for reference only):
  ```python3
  radialVarianceHash(inputArr[, outputArr[, sigma[, numOfAngleLine]]]) -> outputArr
  ```
  """
  @spec radialVarianceHash(Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def radialVarianceHash(inputArr) when (is_struct(inputArr, Evision.Mat) or is_struct(inputArr, Nx.Tensor) or is_number(inputArr) or is_tuple(inputArr))
  do
    positional = [
      inputArr: Evision.Internal.Structurise.from_struct(inputArr)
    ]
    :evision_nif.img_hash_radialVarianceHash(positional)
    |> to_struct()
  end
end
