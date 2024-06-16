defmodule Evision.Intensitytransform do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Intensitytransform` struct.

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
  def to_struct({:ok, %{class: Evision.Intensitytransform, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Intensitytransform, ref: ref}) do
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
  Given an input color image, enhance low-light images using the BIMEF method (@cite ying2017bio @cite ying2017new).

  ##### Positional Arguments
  - **input**: `Evision.Mat`.

    input color image.

  ##### Keyword Arguments
  - **mu**: `float`.

    enhancement ratio.

  - **a**: `float`.

    a-parameter in the Camera Response Function (CRF).

  - **b**: `float`.

    b-parameter in the Camera Response Function (CRF).

  ##### Return
  - **output**: `Evision.Mat.t()`.

    resulting image.

  @warning This is a C++ implementation of the [original MATLAB algorithm](https://github.com/baidut/BIMEF).
   Compared to the original code, this implementation is a little bit slower and does not provide the same results.
   In particular, quality of the image enhancement is degraded for the bright areas in certain conditions.

  Python prototype (for reference only):
  ```python3
  BIMEF(input[, output[, mu[, a[, b]]]]) -> output
  ```
  """
  @spec bimef(Evision.Mat.maybe_mat_in(), [{:a, term()} | {:b, term()} | {:mu, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def bimef(input, opts) when (is_struct(input, Evision.Mat) or is_struct(input, Nx.Tensor) or is_number(input) or is_tuple(input)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:a, :b, :mu])
    positional = [
      input: Evision.Internal.Structurise.from_struct(input)
    ]
    :evision_nif.intensity_transform_BIMEF(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Given an input color image, enhance low-light images using the BIMEF method (@cite ying2017bio @cite ying2017new).

  ##### Positional Arguments
  - **input**: `Evision.Mat`.

    input color image.

  ##### Keyword Arguments
  - **mu**: `float`.

    enhancement ratio.

  - **a**: `float`.

    a-parameter in the Camera Response Function (CRF).

  - **b**: `float`.

    b-parameter in the Camera Response Function (CRF).

  ##### Return
  - **output**: `Evision.Mat.t()`.

    resulting image.

  @warning This is a C++ implementation of the [original MATLAB algorithm](https://github.com/baidut/BIMEF).
   Compared to the original code, this implementation is a little bit slower and does not provide the same results.
   In particular, quality of the image enhancement is degraded for the bright areas in certain conditions.

  Python prototype (for reference only):
  ```python3
  BIMEF(input[, output[, mu[, a[, b]]]]) -> output
  ```
  """
  @spec bimef(Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def bimef(input) when (is_struct(input, Evision.Mat) or is_struct(input, Nx.Tensor) or is_number(input) or is_tuple(input))
  do
    positional = [
      input: Evision.Internal.Structurise.from_struct(input)
    ]
    :evision_nif.intensity_transform_BIMEF(positional)
    |> to_struct()
  end

  @doc """
  Given an input color image, enhance low-light images using the BIMEF method (@cite ying2017bio @cite ying2017new).

  ##### Positional Arguments
  - **input**: `Evision.Mat`.

    input color image.

  - **k**: `float`.

    exposure ratio.

  - **mu**: `float`.

    enhancement ratio.

  - **a**: `float`.

    a-parameter in the Camera Response Function (CRF).

  - **b**: `float`.

    b-parameter in the Camera Response Function (CRF).

  ##### Return
  - **output**: `Evision.Mat.t()`.

    resulting image.

   This is an overloaded function with the exposure ratio given as parameter.

  @warning This is a C++ implementation of the [original MATLAB algorithm](https://github.com/baidut/BIMEF).
   Compared to the original code, this implementation is a little bit slower and does not provide the same results.
   In particular, quality of the image enhancement is degraded for the bright areas in certain conditions.

  Python prototype (for reference only):
  ```python3
  BIMEF2(input, k, mu, a, b[, output]) -> output
  ```
  """
  @spec bimef2(Evision.Mat.maybe_mat_in(), number(), number(), number(), number(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def bimef2(input, k, mu, a, b, opts) when (is_struct(input, Evision.Mat) or is_struct(input, Nx.Tensor) or is_number(input) or is_tuple(input)) and is_float(k) and is_float(mu) and is_float(a) and is_float(b) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      input: Evision.Internal.Structurise.from_struct(input),
      k: Evision.Internal.Structurise.from_struct(k),
      mu: Evision.Internal.Structurise.from_struct(mu),
      a: Evision.Internal.Structurise.from_struct(a),
      b: Evision.Internal.Structurise.from_struct(b)
    ]
    :evision_nif.intensity_transform_BIMEF2(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Given an input color image, enhance low-light images using the BIMEF method (@cite ying2017bio @cite ying2017new).

  ##### Positional Arguments
  - **input**: `Evision.Mat`.

    input color image.

  - **k**: `float`.

    exposure ratio.

  - **mu**: `float`.

    enhancement ratio.

  - **a**: `float`.

    a-parameter in the Camera Response Function (CRF).

  - **b**: `float`.

    b-parameter in the Camera Response Function (CRF).

  ##### Return
  - **output**: `Evision.Mat.t()`.

    resulting image.

   This is an overloaded function with the exposure ratio given as parameter.

  @warning This is a C++ implementation of the [original MATLAB algorithm](https://github.com/baidut/BIMEF).
   Compared to the original code, this implementation is a little bit slower and does not provide the same results.
   In particular, quality of the image enhancement is degraded for the bright areas in certain conditions.

  Python prototype (for reference only):
  ```python3
  BIMEF2(input, k, mu, a, b[, output]) -> output
  ```
  """
  @spec bimef2(Evision.Mat.maybe_mat_in(), number(), number(), number(), number()) :: Evision.Mat.t() | {:error, String.t()}
  def bimef2(input, k, mu, a, b) when (is_struct(input, Evision.Mat) or is_struct(input, Nx.Tensor) or is_number(input) or is_tuple(input)) and is_float(k) and is_float(mu) and is_float(a) and is_float(b)
  do
    positional = [
      input: Evision.Internal.Structurise.from_struct(input),
      k: Evision.Internal.Structurise.from_struct(k),
      mu: Evision.Internal.Structurise.from_struct(mu),
      a: Evision.Internal.Structurise.from_struct(a),
      b: Evision.Internal.Structurise.from_struct(b)
    ]
    :evision_nif.intensity_transform_BIMEF2(positional)
    |> to_struct()
  end

  @doc """
  Given an input bgr or grayscale image, apply autoscaling on domain [0, 255] to increase
  the contrast of the input image and return the resulting image.

  ##### Positional Arguments
  - **input**: `Evision.Mat`.

    input bgr or grayscale image.

  - **output**: `Evision.Mat`.

    resulting image of autoscaling.

  Python prototype (for reference only):
  ```python3
  autoscaling(input, output) -> None
  ```
  """
  @spec autoscaling(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: :ok | {:error, String.t()}
  def autoscaling(input, output) when (is_struct(input, Evision.Mat) or is_struct(input, Nx.Tensor) or is_number(input) or is_tuple(input)) and (is_struct(output, Evision.Mat) or is_struct(output, Nx.Tensor) or is_number(output) or is_tuple(output))
  do
    positional = [
      input: Evision.Internal.Structurise.from_struct(input),
      output: Evision.Internal.Structurise.from_struct(output)
    ]
    :evision_nif.intensity_transform_autoscaling(positional)
    |> to_struct()
  end

  @doc """
  Given an input bgr or grayscale image, apply linear contrast stretching on domain [0, 255]
  and return the resulting image.

  ##### Positional Arguments
  - **input**: `Evision.Mat`.

    input bgr or grayscale image.

  - **output**: `Evision.Mat`.

    resulting image of contrast stretching.

  - **r1**: `integer()`.

    x coordinate of first point (r1, s1) in the transformation function.

  - **s1**: `integer()`.

    y coordinate of first point (r1, s1) in the transformation function.

  - **r2**: `integer()`.

    x coordinate of second point (r2, s2) in the transformation function.

  - **s2**: `integer()`.

    y coordinate of second point (r2, s2) in the transformation function.

  Python prototype (for reference only):
  ```python3
  contrastStretching(input, output, r1, s1, r2, s2) -> None
  ```
  """
  @spec contrastStretching(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), integer(), integer(), integer(), integer()) :: :ok | {:error, String.t()}
  def contrastStretching(input, output, r1, s1, r2, s2) when (is_struct(input, Evision.Mat) or is_struct(input, Nx.Tensor) or is_number(input) or is_tuple(input)) and (is_struct(output, Evision.Mat) or is_struct(output, Nx.Tensor) or is_number(output) or is_tuple(output)) and is_integer(r1) and is_integer(s1) and is_integer(r2) and is_integer(s2)
  do
    positional = [
      input: Evision.Internal.Structurise.from_struct(input),
      output: Evision.Internal.Structurise.from_struct(output),
      r1: Evision.Internal.Structurise.from_struct(r1),
      s1: Evision.Internal.Structurise.from_struct(s1),
      r2: Evision.Internal.Structurise.from_struct(r2),
      s2: Evision.Internal.Structurise.from_struct(s2)
    ]
    :evision_nif.intensity_transform_contrastStretching(positional)
    |> to_struct()
  end

  @doc """
  Given an input bgr or grayscale image and constant gamma, apply power-law transformation,
  a.k.a. gamma correction to the image on domain [0, 255] and return the resulting image.

  ##### Positional Arguments
  - **input**: `Evision.Mat`.

    input bgr or grayscale image.

  - **output**: `Evision.Mat`.

    resulting image of gamma corrections.

  - **gamma**: `float`.

    constant in c*r^gamma where r is pixel value.

  Python prototype (for reference only):
  ```python3
  gammaCorrection(input, output, gamma) -> None
  ```
  """
  @spec gammaCorrection(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), number()) :: :ok | {:error, String.t()}
  def gammaCorrection(input, output, gamma) when (is_struct(input, Evision.Mat) or is_struct(input, Nx.Tensor) or is_number(input) or is_tuple(input)) and (is_struct(output, Evision.Mat) or is_struct(output, Nx.Tensor) or is_number(output) or is_tuple(output)) and is_float(gamma)
  do
    positional = [
      input: Evision.Internal.Structurise.from_struct(input),
      output: Evision.Internal.Structurise.from_struct(output),
      gamma: Evision.Internal.Structurise.from_struct(gamma)
    ]
    :evision_nif.intensity_transform_gammaCorrection(positional)
    |> to_struct()
  end

  @doc """
  Given an input bgr or grayscale image and constant c, apply log transformation to the image
  on domain [0, 255] and return the resulting image.

  ##### Positional Arguments
  - **input**: `Evision.Mat`.

    input bgr or grayscale image.

  - **output**: `Evision.Mat`.

    resulting image of log transformations.

  Python prototype (for reference only):
  ```python3
  logTransform(input, output) -> None
  ```
  """
  @spec logTransform(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: :ok | {:error, String.t()}
  def logTransform(input, output) when (is_struct(input, Evision.Mat) or is_struct(input, Nx.Tensor) or is_number(input) or is_tuple(input)) and (is_struct(output, Evision.Mat) or is_struct(output, Nx.Tensor) or is_number(output) or is_tuple(output))
  do
    positional = [
      input: Evision.Internal.Structurise.from_struct(input),
      output: Evision.Internal.Structurise.from_struct(output)
    ]
    :evision_nif.intensity_transform_logTransform(positional)
    |> to_struct()
  end
end
