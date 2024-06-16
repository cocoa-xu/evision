defmodule Evision.Ft do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Ft` struct.

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
  def to_struct({:ok, %{class: Evision.Ft, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Ft, ref: ref}) do
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
  @type enum :: integer()
  @doc enum: true
  def cv_LINEAR, do: 1
  @doc enum: true
  def cv_SINUS, do: 2
  @doc enum: true
  def cv_ONE_STEP, do: 1
  @doc enum: true
  def cv_MULTI_STEP, do: 2
  @doc enum: true
  def cv_ITERATIVE, do: 3


  @doc """
  Creates kernel from general functions.

  ##### Positional Arguments
  - **function**: `integer()`.

    Function type could be one of the following:
    - **LINEAR** Linear basic function.

  - **radius**: `integer()`.

    Radius of the basic function.

  - **chn**: `integer()`.

    Number of kernel channels.

  ##### Return
  - **kernel**: `Evision.Mat.t()`.

    Final 32-bit kernel.

  The function creates kernel from predefined functions.

  Python prototype (for reference only):
  ```python3
  createKernel(function, radius, chn[, kernel]) -> kernel
  ```
  """
  @spec createKernel(integer(), integer(), integer(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def createKernel(function, radius, chn, opts) when is_integer(function) and is_integer(radius) and is_integer(chn) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      function: Evision.Internal.Structurise.from_struct(function),
      radius: Evision.Internal.Structurise.from_struct(radius),
      chn: Evision.Internal.Structurise.from_struct(chn)
    ]
    :evision_nif.ft_createKernel(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Creates kernel from general functions.

  ##### Positional Arguments
  - **function**: `integer()`.

    Function type could be one of the following:
    - **LINEAR** Linear basic function.

  - **radius**: `integer()`.

    Radius of the basic function.

  - **chn**: `integer()`.

    Number of kernel channels.

  ##### Return
  - **kernel**: `Evision.Mat.t()`.

    Final 32-bit kernel.

  The function creates kernel from predefined functions.

  Python prototype (for reference only):
  ```python3
  createKernel(function, radius, chn[, kernel]) -> kernel
  ```
  """
  @spec createKernel(integer(), integer(), integer()) :: Evision.Mat.t() | {:error, String.t()}
  def createKernel(function, radius, chn) when is_integer(function) and is_integer(radius) and is_integer(chn)
  do
    positional = [
      function: Evision.Internal.Structurise.from_struct(function),
      radius: Evision.Internal.Structurise.from_struct(radius),
      chn: Evision.Internal.Structurise.from_struct(chn)
    ]
    :evision_nif.ft_createKernel(positional)
    |> to_struct()
  end

  @doc """
  Creates kernel from basic functions.

  ##### Positional Arguments
  - **a**: `Evision.Mat`.

    Basic function used in axis **x**.

  - **b**: `Evision.Mat`.

    Basic function used in axis **y**.

  - **chn**: `integer()`.

    Number of kernel channels.

  ##### Return
  - **kernel**: `Evision.Mat.t()`.

    Final 32-bit kernel derived from **A** and **B**.

  The function creates kernel usable for latter fuzzy image processing.

  Python prototype (for reference only):
  ```python3
  createKernel1(A, B, chn[, kernel]) -> kernel
  ```
  """
  @spec createKernel1(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), integer(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def createKernel1(a, b, chn, opts) when (is_struct(a, Evision.Mat) or is_struct(a, Nx.Tensor) or is_number(a) or is_tuple(a)) and (is_struct(b, Evision.Mat) or is_struct(b, Nx.Tensor) or is_number(b) or is_tuple(b)) and is_integer(chn) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      a: Evision.Internal.Structurise.from_struct(a),
      b: Evision.Internal.Structurise.from_struct(b),
      chn: Evision.Internal.Structurise.from_struct(chn)
    ]
    :evision_nif.ft_createKernel1(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Creates kernel from basic functions.

  ##### Positional Arguments
  - **a**: `Evision.Mat`.

    Basic function used in axis **x**.

  - **b**: `Evision.Mat`.

    Basic function used in axis **y**.

  - **chn**: `integer()`.

    Number of kernel channels.

  ##### Return
  - **kernel**: `Evision.Mat.t()`.

    Final 32-bit kernel derived from **A** and **B**.

  The function creates kernel usable for latter fuzzy image processing.

  Python prototype (for reference only):
  ```python3
  createKernel1(A, B, chn[, kernel]) -> kernel
  ```
  """
  @spec createKernel1(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), integer()) :: Evision.Mat.t() | {:error, String.t()}
  def createKernel1(a, b, chn) when (is_struct(a, Evision.Mat) or is_struct(a, Nx.Tensor) or is_number(a) or is_tuple(a)) and (is_struct(b, Evision.Mat) or is_struct(b, Nx.Tensor) or is_number(b) or is_tuple(b)) and is_integer(chn)
  do
    positional = [
      a: Evision.Internal.Structurise.from_struct(a),
      b: Evision.Internal.Structurise.from_struct(b),
      chn: Evision.Internal.Structurise.from_struct(chn)
    ]
    :evision_nif.ft_createKernel1(positional)
    |> to_struct()
  end

  @doc """
  Image filtering

  ##### Positional Arguments
  - **image**: `Evision.Mat`.

    Input image.

  - **kernel**: `Evision.Mat`.

    Final 32-bit kernel.

  ##### Return
  - **output**: `Evision.Mat.t()`.

    Output 32-bit image.

  Filtering of the input image by means of F-transform.

  Python prototype (for reference only):
  ```python3
  filter(image, kernel[, output]) -> output
  ```
  """
  @spec filter(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def filter(image, kernel, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (is_struct(kernel, Evision.Mat) or is_struct(kernel, Nx.Tensor) or is_number(kernel) or is_tuple(kernel)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      kernel: Evision.Internal.Structurise.from_struct(kernel)
    ]
    :evision_nif.ft_filter(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Image filtering

  ##### Positional Arguments
  - **image**: `Evision.Mat`.

    Input image.

  - **kernel**: `Evision.Mat`.

    Final 32-bit kernel.

  ##### Return
  - **output**: `Evision.Mat.t()`.

    Output 32-bit image.

  Filtering of the input image by means of F-transform.

  Python prototype (for reference only):
  ```python3
  filter(image, kernel[, output]) -> output
  ```
  """
  @spec filter(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def filter(image, kernel) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (is_struct(kernel, Evision.Mat) or is_struct(kernel, Nx.Tensor) or is_number(kernel) or is_tuple(kernel))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      kernel: Evision.Internal.Structurise.from_struct(kernel)
    ]
    :evision_nif.ft_filter(positional)
    |> to_struct()
  end

  @doc """
  Image inpainting

  ##### Positional Arguments
  - **image**: `Evision.Mat`.

    Input image.

  - **mask**: `Evision.Mat`.

    Mask used for unwanted area marking.

  - **radius**: `integer()`.

    Radius of the basic function.

  - **function**: `integer()`.

    Function type could be one of the following:
    - `ft::LINEAR` Linear basic function.

  - **algorithm**: `integer()`.

    Algorithm could be one of the following:
    - `ft::ONE_STEP` One step algorithm.
    - `ft::MULTI_STEP` This algorithm automaticaly increases radius of the basic function.
    - `ft::ITERATIVE` Iterative algorithm running in more steps using partial computations.

  ##### Return
  - **output**: `Evision.Mat.t()`.

    Output 32-bit image.

  This function provides inpainting technique based on the fuzzy mathematic.
  **Note**: 
  The algorithms are described in paper @cite Perf:rec.

  Python prototype (for reference only):
  ```python3
  inpaint(image, mask, radius, function, algorithm[, output]) -> output
  ```
  """
  @spec inpaint(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), integer(), integer(), integer(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def inpaint(image, mask, radius, function, algorithm, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (is_struct(mask, Evision.Mat) or is_struct(mask, Nx.Tensor) or is_number(mask) or is_tuple(mask)) and is_integer(radius) and is_integer(function) and is_integer(algorithm) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      mask: Evision.Internal.Structurise.from_struct(mask),
      radius: Evision.Internal.Structurise.from_struct(radius),
      function: Evision.Internal.Structurise.from_struct(function),
      algorithm: Evision.Internal.Structurise.from_struct(algorithm)
    ]
    :evision_nif.ft_inpaint(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Image inpainting

  ##### Positional Arguments
  - **image**: `Evision.Mat`.

    Input image.

  - **mask**: `Evision.Mat`.

    Mask used for unwanted area marking.

  - **radius**: `integer()`.

    Radius of the basic function.

  - **function**: `integer()`.

    Function type could be one of the following:
    - `ft::LINEAR` Linear basic function.

  - **algorithm**: `integer()`.

    Algorithm could be one of the following:
    - `ft::ONE_STEP` One step algorithm.
    - `ft::MULTI_STEP` This algorithm automaticaly increases radius of the basic function.
    - `ft::ITERATIVE` Iterative algorithm running in more steps using partial computations.

  ##### Return
  - **output**: `Evision.Mat.t()`.

    Output 32-bit image.

  This function provides inpainting technique based on the fuzzy mathematic.
  **Note**: 
  The algorithms are described in paper @cite Perf:rec.

  Python prototype (for reference only):
  ```python3
  inpaint(image, mask, radius, function, algorithm[, output]) -> output
  ```
  """
  @spec inpaint(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), integer(), integer(), integer()) :: Evision.Mat.t() | {:error, String.t()}
  def inpaint(image, mask, radius, function, algorithm) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (is_struct(mask, Evision.Mat) or is_struct(mask, Nx.Tensor) or is_number(mask) or is_tuple(mask)) and is_integer(radius) and is_integer(function) and is_integer(algorithm)
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      mask: Evision.Internal.Structurise.from_struct(mask),
      radius: Evision.Internal.Structurise.from_struct(radius),
      function: Evision.Internal.Structurise.from_struct(function),
      algorithm: Evision.Internal.Structurise.from_struct(algorithm)
    ]
    :evision_nif.ft_inpaint(positional)
    |> to_struct()
  end
end
