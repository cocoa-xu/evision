defmodule Evision.CUDA do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `CUDA` struct.

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
  def to_struct({:ok, %{class: Evision.CUDA, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.CUDA, ref: ref}) do
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
  Computes an absolute value of each matrix element.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source matrix.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination matrix with the same size and type as src .

  @sa abs

  Python prototype (for reference only):
  ```python3
  abs(src[, dst[, stream]]) -> dst
  ```
  #### Variant 2:
  Computes an absolute value of each matrix element.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source matrix.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination matrix with the same size and type as src .

  @sa abs

  Python prototype (for reference only):
  ```python3
  abs(src[, dst[, stream]]) -> dst
  ```

  """
  @spec abs(Evision.Mat.maybe_mat_in(), [{:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def abs(src, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_abs(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec abs(Evision.CUDA.GpuMat.t(), [{:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def abs(src, opts) when is_struct(src, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_abs(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Computes an absolute value of each matrix element.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source matrix.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination matrix with the same size and type as src .

  @sa abs

  Python prototype (for reference only):
  ```python3
  abs(src[, dst[, stream]]) -> dst
  ```
  #### Variant 2:
  Computes an absolute value of each matrix element.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source matrix.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination matrix with the same size and type as src .

  @sa abs

  Python prototype (for reference only):
  ```python3
  abs(src[, dst[, stream]]) -> dst
  ```

  """
  @spec abs(Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def abs(src) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_abs(positional)
    |> to_struct()
  end
  @spec abs(Evision.CUDA.GpuMat.t()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def abs(src) when is_struct(src, Evision.CUDA.GpuMat)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_abs(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Returns the sum of absolute values for matrix elements.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source image of any depth except for CV_64F .

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.

    optional operation mask; it must have the same size as src1 and CV_8UC1 type.

  ##### Return
  - **retval**: `Evision.scalar().t()`

  Python prototype (for reference only):
  ```python3
  absSum(src[, mask]) -> retval
  ```
  #### Variant 2:
  Returns the sum of absolute values for matrix elements.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source image of any depth except for CV_64F .

  ##### Keyword Arguments
  - **mask**: `Evision.CUDA.GpuMat.t()`.

    optional operation mask; it must have the same size as src1 and CV_8UC1 type.

  ##### Return
  - **retval**: `Evision.scalar().t()`

  Python prototype (for reference only):
  ```python3
  absSum(src[, mask]) -> retval
  ```

  """
  @spec absSum(Evision.Mat.maybe_mat_in(), [{:mask, term()}] | nil) :: Evision.scalar() | {:error, String.t()}
  def absSum(src, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:mask])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_absSum(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec absSum(Evision.CUDA.GpuMat.t(), [{:mask, term()}] | nil) :: Evision.scalar() | {:error, String.t()}
  def absSum(src, opts) when is_struct(src, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:mask])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_absSum(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Returns the sum of absolute values for matrix elements.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source image of any depth except for CV_64F .

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.

    optional operation mask; it must have the same size as src1 and CV_8UC1 type.

  ##### Return
  - **retval**: `Evision.scalar().t()`

  Python prototype (for reference only):
  ```python3
  absSum(src[, mask]) -> retval
  ```
  #### Variant 2:
  Returns the sum of absolute values for matrix elements.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source image of any depth except for CV_64F .

  ##### Keyword Arguments
  - **mask**: `Evision.CUDA.GpuMat.t()`.

    optional operation mask; it must have the same size as src1 and CV_8UC1 type.

  ##### Return
  - **retval**: `Evision.scalar().t()`

  Python prototype (for reference only):
  ```python3
  absSum(src[, mask]) -> retval
  ```

  """
  @spec absSum(Evision.Mat.maybe_mat_in()) :: Evision.scalar() | {:error, String.t()}
  def absSum(src) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_absSum(positional)
    |> to_struct()
  end
  @spec absSum(Evision.CUDA.GpuMat.t()) :: Evision.scalar() | {:error, String.t()}
  def absSum(src) when is_struct(src, Evision.CUDA.GpuMat)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_absSum(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Computes per-element absolute difference of two matrices (or of a matrix and scalar).

  ##### Positional Arguments
  - **src1**: `Evision.Mat`.

    First source matrix or scalar.

  - **src2**: `Evision.Mat`.

    Second source matrix or scalar.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination matrix that has the same size and type as the input array(s).

  @sa absdiff

  Python prototype (for reference only):
  ```python3
  absdiff(src1, src2[, dst[, stream]]) -> dst
  ```
  #### Variant 2:
  Computes per-element absolute difference of two matrices (or of a matrix and scalar).

  ##### Positional Arguments
  - **src1**: `Evision.CUDA.GpuMat.t()`.

    First source matrix or scalar.

  - **src2**: `Evision.CUDA.GpuMat.t()`.

    Second source matrix or scalar.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination matrix that has the same size and type as the input array(s).

  @sa absdiff

  Python prototype (for reference only):
  ```python3
  absdiff(src1, src2[, dst[, stream]]) -> dst
  ```

  """
  @spec absdiff(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def absdiff(src1, src2, opts) when (is_struct(src1, Evision.Mat) or is_struct(src1, Nx.Tensor) or is_number(src1) or is_tuple(src1)) and (is_struct(src2, Evision.Mat) or is_struct(src2, Nx.Tensor) or is_number(src2) or is_tuple(src2)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1),
      src2: Evision.Internal.Structurise.from_struct(src2)
    ]
    :evision_nif.cuda_absdiff(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec absdiff(Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t(), [{:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def absdiff(src1, src2, opts) when is_struct(src1, Evision.CUDA.GpuMat) and is_struct(src2, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1),
      src2: Evision.Internal.Structurise.from_struct(src2)
    ]
    :evision_nif.cuda_absdiff(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Computes per-element absolute difference of two matrices (or of a matrix and scalar).

  ##### Positional Arguments
  - **src1**: `Evision.Mat`.

    First source matrix or scalar.

  - **src2**: `Evision.Mat`.

    Second source matrix or scalar.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination matrix that has the same size and type as the input array(s).

  @sa absdiff

  Python prototype (for reference only):
  ```python3
  absdiff(src1, src2[, dst[, stream]]) -> dst
  ```
  #### Variant 2:
  Computes per-element absolute difference of two matrices (or of a matrix and scalar).

  ##### Positional Arguments
  - **src1**: `Evision.CUDA.GpuMat.t()`.

    First source matrix or scalar.

  - **src2**: `Evision.CUDA.GpuMat.t()`.

    Second source matrix or scalar.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination matrix that has the same size and type as the input array(s).

  @sa absdiff

  Python prototype (for reference only):
  ```python3
  absdiff(src1, src2[, dst[, stream]]) -> dst
  ```

  """
  @spec absdiff(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def absdiff(src1, src2) when (is_struct(src1, Evision.Mat) or is_struct(src1, Nx.Tensor) or is_number(src1) or is_tuple(src1)) and (is_struct(src2, Evision.Mat) or is_struct(src2, Nx.Tensor) or is_number(src2) or is_tuple(src2))
  do
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1),
      src2: Evision.Internal.Structurise.from_struct(src2)
    ]
    :evision_nif.cuda_absdiff(positional)
    |> to_struct()
  end
  @spec absdiff(Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def absdiff(src1, src2) when is_struct(src1, Evision.CUDA.GpuMat) and is_struct(src2, Evision.CUDA.GpuMat)
  do
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1),
      src2: Evision.Internal.Structurise.from_struct(src2)
    ]
    :evision_nif.cuda_absdiff(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Computes a matrix-matrix or matrix-scalar sum.

  ##### Positional Arguments
  - **src1**: `Evision.Mat`.

    First source matrix or scalar.

  - **src2**: `Evision.Mat`.

    Second source matrix or scalar. Matrix should have the same size and type as src1 .

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.

    Optional operation mask, 8-bit single channel array, that specifies elements of the
    destination array to be changed. The mask can be used only with single channel images.

  - **dtype**: `integer()`.

    Optional depth of the output array.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination matrix that has the same size and number of channels as the input array(s).
    The depth is defined by dtype or src1 depth.

  @sa add

  Python prototype (for reference only):
  ```python3
  add(src1, src2[, dst[, mask[, dtype[, stream]]]]) -> dst
  ```
  #### Variant 2:
  Computes a matrix-matrix or matrix-scalar sum.

  ##### Positional Arguments
  - **src1**: `Evision.CUDA.GpuMat.t()`.

    First source matrix or scalar.

  - **src2**: `Evision.CUDA.GpuMat.t()`.

    Second source matrix or scalar. Matrix should have the same size and type as src1 .

  ##### Keyword Arguments
  - **mask**: `Evision.CUDA.GpuMat.t()`.

    Optional operation mask, 8-bit single channel array, that specifies elements of the
    destination array to be changed. The mask can be used only with single channel images.

  - **dtype**: `integer()`.

    Optional depth of the output array.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination matrix that has the same size and number of channels as the input array(s).
    The depth is defined by dtype or src1 depth.

  @sa add

  Python prototype (for reference only):
  ```python3
  add(src1, src2[, dst[, mask[, dtype[, stream]]]]) -> dst
  ```

  """
  @spec add(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:dtype, term()} | {:mask, term()} | {:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def add(src1, src2, opts) when (is_struct(src1, Evision.Mat) or is_struct(src1, Nx.Tensor) or is_number(src1) or is_tuple(src1)) and (is_struct(src2, Evision.Mat) or is_struct(src2, Nx.Tensor) or is_number(src2) or is_tuple(src2)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:dtype, :mask, :stream])
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1),
      src2: Evision.Internal.Structurise.from_struct(src2)
    ]
    :evision_nif.cuda_add(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec add(Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t(), [{:dtype, term()} | {:mask, term()} | {:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def add(src1, src2, opts) when is_struct(src1, Evision.CUDA.GpuMat) and is_struct(src2, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:dtype, :mask, :stream])
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1),
      src2: Evision.Internal.Structurise.from_struct(src2)
    ]
    :evision_nif.cuda_add(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Computes a matrix-matrix or matrix-scalar sum.

  ##### Positional Arguments
  - **src1**: `Evision.Mat`.

    First source matrix or scalar.

  - **src2**: `Evision.Mat`.

    Second source matrix or scalar. Matrix should have the same size and type as src1 .

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.

    Optional operation mask, 8-bit single channel array, that specifies elements of the
    destination array to be changed. The mask can be used only with single channel images.

  - **dtype**: `integer()`.

    Optional depth of the output array.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination matrix that has the same size and number of channels as the input array(s).
    The depth is defined by dtype or src1 depth.

  @sa add

  Python prototype (for reference only):
  ```python3
  add(src1, src2[, dst[, mask[, dtype[, stream]]]]) -> dst
  ```
  #### Variant 2:
  Computes a matrix-matrix or matrix-scalar sum.

  ##### Positional Arguments
  - **src1**: `Evision.CUDA.GpuMat.t()`.

    First source matrix or scalar.

  - **src2**: `Evision.CUDA.GpuMat.t()`.

    Second source matrix or scalar. Matrix should have the same size and type as src1 .

  ##### Keyword Arguments
  - **mask**: `Evision.CUDA.GpuMat.t()`.

    Optional operation mask, 8-bit single channel array, that specifies elements of the
    destination array to be changed. The mask can be used only with single channel images.

  - **dtype**: `integer()`.

    Optional depth of the output array.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination matrix that has the same size and number of channels as the input array(s).
    The depth is defined by dtype or src1 depth.

  @sa add

  Python prototype (for reference only):
  ```python3
  add(src1, src2[, dst[, mask[, dtype[, stream]]]]) -> dst
  ```

  """
  @spec add(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def add(src1, src2) when (is_struct(src1, Evision.Mat) or is_struct(src1, Nx.Tensor) or is_number(src1) or is_tuple(src1)) and (is_struct(src2, Evision.Mat) or is_struct(src2, Nx.Tensor) or is_number(src2) or is_tuple(src2))
  do
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1),
      src2: Evision.Internal.Structurise.from_struct(src2)
    ]
    :evision_nif.cuda_add(positional)
    |> to_struct()
  end
  @spec add(Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def add(src1, src2) when is_struct(src1, Evision.CUDA.GpuMat) and is_struct(src2, Evision.CUDA.GpuMat)
  do
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1),
      src2: Evision.Internal.Structurise.from_struct(src2)
    ]
    :evision_nif.cuda_add(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Computes the weighted sum of two arrays.

  ##### Positional Arguments
  - **src1**: `Evision.Mat`.

    First source array.

  - **alpha**: `double`.

    Weight for the first array elements.

  - **src2**: `Evision.Mat`.

    Second source array of the same size and channel number as src1 .

  - **beta**: `double`.

    Weight for the second array elements.

  - **gamma**: `double`.

    Scalar added to each sum.

  ##### Keyword Arguments
  - **dtype**: `integer()`.

    Optional depth of the destination array. When both input arrays have the same depth,
    dtype can be set to -1, which will be equivalent to src1.depth().

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination array that has the same size and number of channels as the input arrays.

  The function addWeighted calculates the weighted sum of two arrays as follows:
  \\f[\\texttt{dst} (I)= \\texttt{saturate} ( \\texttt{src1} (I)\\* \\texttt{alpha} +  \\texttt{src2} (I)\\* \\texttt{beta} +  \\texttt{gamma} )\\f]
  where I is a multi-dimensional index of array elements. In case of multi-channel arrays, each
  channel is processed independently.
  @sa addWeighted

  Python prototype (for reference only):
  ```python3
  addWeighted(src1, alpha, src2, beta, gamma[, dst[, dtype[, stream]]]) -> dst
  ```
  #### Variant 2:
  Computes the weighted sum of two arrays.

  ##### Positional Arguments
  - **src1**: `Evision.CUDA.GpuMat.t()`.

    First source array.

  - **alpha**: `double`.

    Weight for the first array elements.

  - **src2**: `Evision.CUDA.GpuMat.t()`.

    Second source array of the same size and channel number as src1 .

  - **beta**: `double`.

    Weight for the second array elements.

  - **gamma**: `double`.

    Scalar added to each sum.

  ##### Keyword Arguments
  - **dtype**: `integer()`.

    Optional depth of the destination array. When both input arrays have the same depth,
    dtype can be set to -1, which will be equivalent to src1.depth().

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination array that has the same size and number of channels as the input arrays.

  The function addWeighted calculates the weighted sum of two arrays as follows:
  \\f[\\texttt{dst} (I)= \\texttt{saturate} ( \\texttt{src1} (I)\\* \\texttt{alpha} +  \\texttt{src2} (I)\\* \\texttt{beta} +  \\texttt{gamma} )\\f]
  where I is a multi-dimensional index of array elements. In case of multi-channel arrays, each
  channel is processed independently.
  @sa addWeighted

  Python prototype (for reference only):
  ```python3
  addWeighted(src1, alpha, src2, beta, gamma[, dst[, dtype[, stream]]]) -> dst
  ```

  """
  @spec addWeighted(Evision.Mat.maybe_mat_in(), number(), Evision.Mat.maybe_mat_in(), number(), number(), [{:dtype, term()} | {:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def addWeighted(src1, alpha, src2, beta, gamma, opts) when (is_struct(src1, Evision.Mat) or is_struct(src1, Nx.Tensor) or is_number(src1) or is_tuple(src1)) and is_number(alpha) and (is_struct(src2, Evision.Mat) or is_struct(src2, Nx.Tensor) or is_number(src2) or is_tuple(src2)) and is_number(beta) and is_number(gamma) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:dtype, :stream])
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1),
      alpha: Evision.Internal.Structurise.from_struct(alpha),
      src2: Evision.Internal.Structurise.from_struct(src2),
      beta: Evision.Internal.Structurise.from_struct(beta),
      gamma: Evision.Internal.Structurise.from_struct(gamma)
    ]
    :evision_nif.cuda_addWeighted(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec addWeighted(Evision.CUDA.GpuMat.t(), number(), Evision.CUDA.GpuMat.t(), number(), number(), [{:dtype, term()} | {:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def addWeighted(src1, alpha, src2, beta, gamma, opts) when is_struct(src1, Evision.CUDA.GpuMat) and is_number(alpha) and is_struct(src2, Evision.CUDA.GpuMat) and is_number(beta) and is_number(gamma) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:dtype, :stream])
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1),
      alpha: Evision.Internal.Structurise.from_struct(alpha),
      src2: Evision.Internal.Structurise.from_struct(src2),
      beta: Evision.Internal.Structurise.from_struct(beta),
      gamma: Evision.Internal.Structurise.from_struct(gamma)
    ]
    :evision_nif.cuda_addWeighted(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Computes the weighted sum of two arrays.

  ##### Positional Arguments
  - **src1**: `Evision.Mat`.

    First source array.

  - **alpha**: `double`.

    Weight for the first array elements.

  - **src2**: `Evision.Mat`.

    Second source array of the same size and channel number as src1 .

  - **beta**: `double`.

    Weight for the second array elements.

  - **gamma**: `double`.

    Scalar added to each sum.

  ##### Keyword Arguments
  - **dtype**: `integer()`.

    Optional depth of the destination array. When both input arrays have the same depth,
    dtype can be set to -1, which will be equivalent to src1.depth().

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination array that has the same size and number of channels as the input arrays.

  The function addWeighted calculates the weighted sum of two arrays as follows:
  \\f[\\texttt{dst} (I)= \\texttt{saturate} ( \\texttt{src1} (I)\\* \\texttt{alpha} +  \\texttt{src2} (I)\\* \\texttt{beta} +  \\texttt{gamma} )\\f]
  where I is a multi-dimensional index of array elements. In case of multi-channel arrays, each
  channel is processed independently.
  @sa addWeighted

  Python prototype (for reference only):
  ```python3
  addWeighted(src1, alpha, src2, beta, gamma[, dst[, dtype[, stream]]]) -> dst
  ```
  #### Variant 2:
  Computes the weighted sum of two arrays.

  ##### Positional Arguments
  - **src1**: `Evision.CUDA.GpuMat.t()`.

    First source array.

  - **alpha**: `double`.

    Weight for the first array elements.

  - **src2**: `Evision.CUDA.GpuMat.t()`.

    Second source array of the same size and channel number as src1 .

  - **beta**: `double`.

    Weight for the second array elements.

  - **gamma**: `double`.

    Scalar added to each sum.

  ##### Keyword Arguments
  - **dtype**: `integer()`.

    Optional depth of the destination array. When both input arrays have the same depth,
    dtype can be set to -1, which will be equivalent to src1.depth().

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination array that has the same size and number of channels as the input arrays.

  The function addWeighted calculates the weighted sum of two arrays as follows:
  \\f[\\texttt{dst} (I)= \\texttt{saturate} ( \\texttt{src1} (I)\\* \\texttt{alpha} +  \\texttt{src2} (I)\\* \\texttt{beta} +  \\texttt{gamma} )\\f]
  where I is a multi-dimensional index of array elements. In case of multi-channel arrays, each
  channel is processed independently.
  @sa addWeighted

  Python prototype (for reference only):
  ```python3
  addWeighted(src1, alpha, src2, beta, gamma[, dst[, dtype[, stream]]]) -> dst
  ```

  """
  @spec addWeighted(Evision.Mat.maybe_mat_in(), number(), Evision.Mat.maybe_mat_in(), number(), number()) :: Evision.Mat.t() | {:error, String.t()}
  def addWeighted(src1, alpha, src2, beta, gamma) when (is_struct(src1, Evision.Mat) or is_struct(src1, Nx.Tensor) or is_number(src1) or is_tuple(src1)) and is_number(alpha) and (is_struct(src2, Evision.Mat) or is_struct(src2, Nx.Tensor) or is_number(src2) or is_tuple(src2)) and is_number(beta) and is_number(gamma)
  do
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1),
      alpha: Evision.Internal.Structurise.from_struct(alpha),
      src2: Evision.Internal.Structurise.from_struct(src2),
      beta: Evision.Internal.Structurise.from_struct(beta),
      gamma: Evision.Internal.Structurise.from_struct(gamma)
    ]
    :evision_nif.cuda_addWeighted(positional)
    |> to_struct()
  end
  @spec addWeighted(Evision.CUDA.GpuMat.t(), number(), Evision.CUDA.GpuMat.t(), number(), number()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def addWeighted(src1, alpha, src2, beta, gamma) when is_struct(src1, Evision.CUDA.GpuMat) and is_number(alpha) and is_struct(src2, Evision.CUDA.GpuMat) and is_number(beta) and is_number(gamma)
  do
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1),
      alpha: Evision.Internal.Structurise.from_struct(alpha),
      src2: Evision.Internal.Structurise.from_struct(src2),
      beta: Evision.Internal.Structurise.from_struct(beta),
      gamma: Evision.Internal.Structurise.from_struct(gamma)
    ]
    :evision_nif.cuda_addWeighted(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Composites two images using alpha opacity values contained in each image.

  ##### Positional Arguments
  - **img1**: `Evision.Mat`.

    First image. Supports CV_8UC4 , CV_16UC4 , CV_32SC4 and CV_32FC4 types.

  - **img2**: `Evision.Mat`.

    Second image. Must have the same size and the same type as img1 .

  - **alpha_op**: `integer()`.

    Flag specifying the alpha-blending operation:
    - **ALPHA_OVER**
    - **ALPHA_IN**
    - **ALPHA_OUT**
    - **ALPHA_ATOP**
    - **ALPHA_XOR**
    - **ALPHA_PLUS**
    - **ALPHA_OVER_PREMUL**
    - **ALPHA_IN_PREMUL**
    - **ALPHA_OUT_PREMUL**
    - **ALPHA_ATOP_PREMUL**
    - **ALPHA_XOR_PREMUL**
    - **ALPHA_PLUS_PREMUL**
    - **ALPHA_PREMUL**

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination image.

  **Note**: 
  - An example demonstrating the use of alphaComp can be found at
    opencv_source_code/samples/gpu/alpha_comp.cpp

  Python prototype (for reference only):
  ```python3
  alphaComp(img1, img2, alpha_op[, dst[, stream]]) -> dst
  ```
  #### Variant 2:
  Composites two images using alpha opacity values contained in each image.

  ##### Positional Arguments
  - **img1**: `Evision.CUDA.GpuMat.t()`.

    First image. Supports CV_8UC4 , CV_16UC4 , CV_32SC4 and CV_32FC4 types.

  - **img2**: `Evision.CUDA.GpuMat.t()`.

    Second image. Must have the same size and the same type as img1 .

  - **alpha_op**: `integer()`.

    Flag specifying the alpha-blending operation:
    - **ALPHA_OVER**
    - **ALPHA_IN**
    - **ALPHA_OUT**
    - **ALPHA_ATOP**
    - **ALPHA_XOR**
    - **ALPHA_PLUS**
    - **ALPHA_OVER_PREMUL**
    - **ALPHA_IN_PREMUL**
    - **ALPHA_OUT_PREMUL**
    - **ALPHA_ATOP_PREMUL**
    - **ALPHA_XOR_PREMUL**
    - **ALPHA_PLUS_PREMUL**
    - **ALPHA_PREMUL**

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination image.

  **Note**: 
  - An example demonstrating the use of alphaComp can be found at
    opencv_source_code/samples/gpu/alpha_comp.cpp

  Python prototype (for reference only):
  ```python3
  alphaComp(img1, img2, alpha_op[, dst[, stream]]) -> dst
  ```

  """
  @spec alphaComp(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), integer(), [{:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def alphaComp(img1, img2, alpha_op, opts) when (is_struct(img1, Evision.Mat) or is_struct(img1, Nx.Tensor) or is_number(img1) or is_tuple(img1)) and (is_struct(img2, Evision.Mat) or is_struct(img2, Nx.Tensor) or is_number(img2) or is_tuple(img2)) and is_integer(alpha_op) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      img1: Evision.Internal.Structurise.from_struct(img1),
      img2: Evision.Internal.Structurise.from_struct(img2),
      alpha_op: Evision.Internal.Structurise.from_struct(alpha_op)
    ]
    :evision_nif.cuda_alphaComp(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec alphaComp(Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t(), integer(), [{:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def alphaComp(img1, img2, alpha_op, opts) when is_struct(img1, Evision.CUDA.GpuMat) and is_struct(img2, Evision.CUDA.GpuMat) and is_integer(alpha_op) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      img1: Evision.Internal.Structurise.from_struct(img1),
      img2: Evision.Internal.Structurise.from_struct(img2),
      alpha_op: Evision.Internal.Structurise.from_struct(alpha_op)
    ]
    :evision_nif.cuda_alphaComp(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Composites two images using alpha opacity values contained in each image.

  ##### Positional Arguments
  - **img1**: `Evision.Mat`.

    First image. Supports CV_8UC4 , CV_16UC4 , CV_32SC4 and CV_32FC4 types.

  - **img2**: `Evision.Mat`.

    Second image. Must have the same size and the same type as img1 .

  - **alpha_op**: `integer()`.

    Flag specifying the alpha-blending operation:
    - **ALPHA_OVER**
    - **ALPHA_IN**
    - **ALPHA_OUT**
    - **ALPHA_ATOP**
    - **ALPHA_XOR**
    - **ALPHA_PLUS**
    - **ALPHA_OVER_PREMUL**
    - **ALPHA_IN_PREMUL**
    - **ALPHA_OUT_PREMUL**
    - **ALPHA_ATOP_PREMUL**
    - **ALPHA_XOR_PREMUL**
    - **ALPHA_PLUS_PREMUL**
    - **ALPHA_PREMUL**

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination image.

  **Note**: 
  - An example demonstrating the use of alphaComp can be found at
    opencv_source_code/samples/gpu/alpha_comp.cpp

  Python prototype (for reference only):
  ```python3
  alphaComp(img1, img2, alpha_op[, dst[, stream]]) -> dst
  ```
  #### Variant 2:
  Composites two images using alpha opacity values contained in each image.

  ##### Positional Arguments
  - **img1**: `Evision.CUDA.GpuMat.t()`.

    First image. Supports CV_8UC4 , CV_16UC4 , CV_32SC4 and CV_32FC4 types.

  - **img2**: `Evision.CUDA.GpuMat.t()`.

    Second image. Must have the same size and the same type as img1 .

  - **alpha_op**: `integer()`.

    Flag specifying the alpha-blending operation:
    - **ALPHA_OVER**
    - **ALPHA_IN**
    - **ALPHA_OUT**
    - **ALPHA_ATOP**
    - **ALPHA_XOR**
    - **ALPHA_PLUS**
    - **ALPHA_OVER_PREMUL**
    - **ALPHA_IN_PREMUL**
    - **ALPHA_OUT_PREMUL**
    - **ALPHA_ATOP_PREMUL**
    - **ALPHA_XOR_PREMUL**
    - **ALPHA_PLUS_PREMUL**
    - **ALPHA_PREMUL**

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination image.

  **Note**: 
  - An example demonstrating the use of alphaComp can be found at
    opencv_source_code/samples/gpu/alpha_comp.cpp

  Python prototype (for reference only):
  ```python3
  alphaComp(img1, img2, alpha_op[, dst[, stream]]) -> dst
  ```

  """
  @spec alphaComp(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), integer()) :: Evision.Mat.t() | {:error, String.t()}
  def alphaComp(img1, img2, alpha_op) when (is_struct(img1, Evision.Mat) or is_struct(img1, Nx.Tensor) or is_number(img1) or is_tuple(img1)) and (is_struct(img2, Evision.Mat) or is_struct(img2, Nx.Tensor) or is_number(img2) or is_tuple(img2)) and is_integer(alpha_op)
  do
    positional = [
      img1: Evision.Internal.Structurise.from_struct(img1),
      img2: Evision.Internal.Structurise.from_struct(img2),
      alpha_op: Evision.Internal.Structurise.from_struct(alpha_op)
    ]
    :evision_nif.cuda_alphaComp(positional)
    |> to_struct()
  end
  @spec alphaComp(Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t(), integer()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def alphaComp(img1, img2, alpha_op) when is_struct(img1, Evision.CUDA.GpuMat) and is_struct(img2, Evision.CUDA.GpuMat) and is_integer(alpha_op)
  do
    positional = [
      img1: Evision.Internal.Structurise.from_struct(img1),
      img2: Evision.Internal.Structurise.from_struct(img2),
      alpha_op: Evision.Internal.Structurise.from_struct(alpha_op)
    ]
    :evision_nif.cuda_alphaComp(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Performs bilateral filtering of passed image

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source image. Supports only (channels != 2 && depth() != CV_8S && depth() != CV_32S
    && depth() != CV_64F).

  - **kernel_size**: `integer()`.

    Kernel window size.

  - **sigma_color**: `float`.

    Filter sigma in the color space.

  - **sigma_spatial**: `float`.

    Filter sigma in the coordinate space.

  ##### Keyword Arguments
  - **borderMode**: `integer()`.

    Border type. See borderInterpolate for details. BORDER_REFLECT101 ,
    BORDER_REPLICATE , BORDER_CONSTANT , BORDER_REFLECT and BORDER_WRAP are supported for now.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination imagwe.

  @sa bilateralFilter

  Python prototype (for reference only):
  ```python3
  bilateralFilter(src, kernel_size, sigma_color, sigma_spatial[, dst[, borderMode[, stream]]]) -> dst
  ```
  #### Variant 2:
  Performs bilateral filtering of passed image

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source image. Supports only (channels != 2 && depth() != CV_8S && depth() != CV_32S
    && depth() != CV_64F).

  - **kernel_size**: `integer()`.

    Kernel window size.

  - **sigma_color**: `float`.

    Filter sigma in the color space.

  - **sigma_spatial**: `float`.

    Filter sigma in the coordinate space.

  ##### Keyword Arguments
  - **borderMode**: `integer()`.

    Border type. See borderInterpolate for details. BORDER_REFLECT101 ,
    BORDER_REPLICATE , BORDER_CONSTANT , BORDER_REFLECT and BORDER_WRAP are supported for now.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination imagwe.

  @sa bilateralFilter

  Python prototype (for reference only):
  ```python3
  bilateralFilter(src, kernel_size, sigma_color, sigma_spatial[, dst[, borderMode[, stream]]]) -> dst
  ```

  """
  @spec bilateralFilter(Evision.Mat.maybe_mat_in(), integer(), number(), number(), [{:borderMode, term()} | {:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def bilateralFilter(src, kernel_size, sigma_color, sigma_spatial, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_integer(kernel_size) and is_float(sigma_color) and is_float(sigma_spatial) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:borderMode, :stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      kernel_size: Evision.Internal.Structurise.from_struct(kernel_size),
      sigma_color: Evision.Internal.Structurise.from_struct(sigma_color),
      sigma_spatial: Evision.Internal.Structurise.from_struct(sigma_spatial)
    ]
    :evision_nif.cuda_bilateralFilter(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec bilateralFilter(Evision.CUDA.GpuMat.t(), integer(), number(), number(), [{:borderMode, term()} | {:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def bilateralFilter(src, kernel_size, sigma_color, sigma_spatial, opts) when is_struct(src, Evision.CUDA.GpuMat) and is_integer(kernel_size) and is_float(sigma_color) and is_float(sigma_spatial) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:borderMode, :stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      kernel_size: Evision.Internal.Structurise.from_struct(kernel_size),
      sigma_color: Evision.Internal.Structurise.from_struct(sigma_color),
      sigma_spatial: Evision.Internal.Structurise.from_struct(sigma_spatial)
    ]
    :evision_nif.cuda_bilateralFilter(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Performs bilateral filtering of passed image

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source image. Supports only (channels != 2 && depth() != CV_8S && depth() != CV_32S
    && depth() != CV_64F).

  - **kernel_size**: `integer()`.

    Kernel window size.

  - **sigma_color**: `float`.

    Filter sigma in the color space.

  - **sigma_spatial**: `float`.

    Filter sigma in the coordinate space.

  ##### Keyword Arguments
  - **borderMode**: `integer()`.

    Border type. See borderInterpolate for details. BORDER_REFLECT101 ,
    BORDER_REPLICATE , BORDER_CONSTANT , BORDER_REFLECT and BORDER_WRAP are supported for now.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination imagwe.

  @sa bilateralFilter

  Python prototype (for reference only):
  ```python3
  bilateralFilter(src, kernel_size, sigma_color, sigma_spatial[, dst[, borderMode[, stream]]]) -> dst
  ```
  #### Variant 2:
  Performs bilateral filtering of passed image

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source image. Supports only (channels != 2 && depth() != CV_8S && depth() != CV_32S
    && depth() != CV_64F).

  - **kernel_size**: `integer()`.

    Kernel window size.

  - **sigma_color**: `float`.

    Filter sigma in the color space.

  - **sigma_spatial**: `float`.

    Filter sigma in the coordinate space.

  ##### Keyword Arguments
  - **borderMode**: `integer()`.

    Border type. See borderInterpolate for details. BORDER_REFLECT101 ,
    BORDER_REPLICATE , BORDER_CONSTANT , BORDER_REFLECT and BORDER_WRAP are supported for now.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination imagwe.

  @sa bilateralFilter

  Python prototype (for reference only):
  ```python3
  bilateralFilter(src, kernel_size, sigma_color, sigma_spatial[, dst[, borderMode[, stream]]]) -> dst
  ```

  """
  @spec bilateralFilter(Evision.Mat.maybe_mat_in(), integer(), number(), number()) :: Evision.Mat.t() | {:error, String.t()}
  def bilateralFilter(src, kernel_size, sigma_color, sigma_spatial) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_integer(kernel_size) and is_float(sigma_color) and is_float(sigma_spatial)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      kernel_size: Evision.Internal.Structurise.from_struct(kernel_size),
      sigma_color: Evision.Internal.Structurise.from_struct(sigma_color),
      sigma_spatial: Evision.Internal.Structurise.from_struct(sigma_spatial)
    ]
    :evision_nif.cuda_bilateralFilter(positional)
    |> to_struct()
  end
  @spec bilateralFilter(Evision.CUDA.GpuMat.t(), integer(), number(), number()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def bilateralFilter(src, kernel_size, sigma_color, sigma_spatial) when is_struct(src, Evision.CUDA.GpuMat) and is_integer(kernel_size) and is_float(sigma_color) and is_float(sigma_spatial)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      kernel_size: Evision.Internal.Structurise.from_struct(kernel_size),
      sigma_color: Evision.Internal.Structurise.from_struct(sigma_color),
      sigma_spatial: Evision.Internal.Structurise.from_struct(sigma_spatial)
    ]
    :evision_nif.cuda_bilateralFilter(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Performs linear blending of two images.

  ##### Positional Arguments
  - **img1**: `Evision.Mat`.

    First image. Supports only CV_8U and CV_32F depth.

  - **img2**: `Evision.Mat`.

    Second image. Must have the same size and the same type as img1 .

  - **weights1**: `Evision.Mat`.

    Weights for first image. Must have tha same size as img1 . Supports only CV_32F
    type.

  - **weights2**: `Evision.Mat`.

    Weights for second image. Must have tha same size as img2 . Supports only CV_32F
    type.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **result**: `Evision.Mat.t()`.

    Destination image.

  Python prototype (for reference only):
  ```python3
  blendLinear(img1, img2, weights1, weights2[, result[, stream]]) -> result
  ```
  #### Variant 2:
  Performs linear blending of two images.

  ##### Positional Arguments
  - **img1**: `Evision.CUDA.GpuMat.t()`.

    First image. Supports only CV_8U and CV_32F depth.

  - **img2**: `Evision.CUDA.GpuMat.t()`.

    Second image. Must have the same size and the same type as img1 .

  - **weights1**: `Evision.CUDA.GpuMat.t()`.

    Weights for first image. Must have tha same size as img1 . Supports only CV_32F
    type.

  - **weights2**: `Evision.CUDA.GpuMat.t()`.

    Weights for second image. Must have tha same size as img2 . Supports only CV_32F
    type.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **result**: `Evision.CUDA.GpuMat.t()`.

    Destination image.

  Python prototype (for reference only):
  ```python3
  blendLinear(img1, img2, weights1, weights2[, result[, stream]]) -> result
  ```

  """
  @spec blendLinear(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def blendLinear(img1, img2, weights1, weights2, opts) when (is_struct(img1, Evision.Mat) or is_struct(img1, Nx.Tensor) or is_number(img1) or is_tuple(img1)) and (is_struct(img2, Evision.Mat) or is_struct(img2, Nx.Tensor) or is_number(img2) or is_tuple(img2)) and (is_struct(weights1, Evision.Mat) or is_struct(weights1, Nx.Tensor) or is_number(weights1) or is_tuple(weights1)) and (is_struct(weights2, Evision.Mat) or is_struct(weights2, Nx.Tensor) or is_number(weights2) or is_tuple(weights2)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      img1: Evision.Internal.Structurise.from_struct(img1),
      img2: Evision.Internal.Structurise.from_struct(img2),
      weights1: Evision.Internal.Structurise.from_struct(weights1),
      weights2: Evision.Internal.Structurise.from_struct(weights2)
    ]
    :evision_nif.cuda_blendLinear(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec blendLinear(Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t(), [{:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def blendLinear(img1, img2, weights1, weights2, opts) when is_struct(img1, Evision.CUDA.GpuMat) and is_struct(img2, Evision.CUDA.GpuMat) and is_struct(weights1, Evision.CUDA.GpuMat) and is_struct(weights2, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      img1: Evision.Internal.Structurise.from_struct(img1),
      img2: Evision.Internal.Structurise.from_struct(img2),
      weights1: Evision.Internal.Structurise.from_struct(weights1),
      weights2: Evision.Internal.Structurise.from_struct(weights2)
    ]
    :evision_nif.cuda_blendLinear(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Performs linear blending of two images.

  ##### Positional Arguments
  - **img1**: `Evision.Mat`.

    First image. Supports only CV_8U and CV_32F depth.

  - **img2**: `Evision.Mat`.

    Second image. Must have the same size and the same type as img1 .

  - **weights1**: `Evision.Mat`.

    Weights for first image. Must have tha same size as img1 . Supports only CV_32F
    type.

  - **weights2**: `Evision.Mat`.

    Weights for second image. Must have tha same size as img2 . Supports only CV_32F
    type.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **result**: `Evision.Mat.t()`.

    Destination image.

  Python prototype (for reference only):
  ```python3
  blendLinear(img1, img2, weights1, weights2[, result[, stream]]) -> result
  ```
  #### Variant 2:
  Performs linear blending of two images.

  ##### Positional Arguments
  - **img1**: `Evision.CUDA.GpuMat.t()`.

    First image. Supports only CV_8U and CV_32F depth.

  - **img2**: `Evision.CUDA.GpuMat.t()`.

    Second image. Must have the same size and the same type as img1 .

  - **weights1**: `Evision.CUDA.GpuMat.t()`.

    Weights for first image. Must have tha same size as img1 . Supports only CV_32F
    type.

  - **weights2**: `Evision.CUDA.GpuMat.t()`.

    Weights for second image. Must have tha same size as img2 . Supports only CV_32F
    type.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **result**: `Evision.CUDA.GpuMat.t()`.

    Destination image.

  Python prototype (for reference only):
  ```python3
  blendLinear(img1, img2, weights1, weights2[, result[, stream]]) -> result
  ```

  """
  @spec blendLinear(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def blendLinear(img1, img2, weights1, weights2) when (is_struct(img1, Evision.Mat) or is_struct(img1, Nx.Tensor) or is_number(img1) or is_tuple(img1)) and (is_struct(img2, Evision.Mat) or is_struct(img2, Nx.Tensor) or is_number(img2) or is_tuple(img2)) and (is_struct(weights1, Evision.Mat) or is_struct(weights1, Nx.Tensor) or is_number(weights1) or is_tuple(weights1)) and (is_struct(weights2, Evision.Mat) or is_struct(weights2, Nx.Tensor) or is_number(weights2) or is_tuple(weights2))
  do
    positional = [
      img1: Evision.Internal.Structurise.from_struct(img1),
      img2: Evision.Internal.Structurise.from_struct(img2),
      weights1: Evision.Internal.Structurise.from_struct(weights1),
      weights2: Evision.Internal.Structurise.from_struct(weights2)
    ]
    :evision_nif.cuda_blendLinear(positional)
    |> to_struct()
  end
  @spec blendLinear(Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def blendLinear(img1, img2, weights1, weights2) when is_struct(img1, Evision.CUDA.GpuMat) and is_struct(img2, Evision.CUDA.GpuMat) and is_struct(weights1, Evision.CUDA.GpuMat) and is_struct(weights2, Evision.CUDA.GpuMat)
  do
    positional = [
      img1: Evision.Internal.Structurise.from_struct(img1),
      img2: Evision.Internal.Structurise.from_struct(img2),
      weights1: Evision.Internal.Structurise.from_struct(weights1),
      weights2: Evision.Internal.Structurise.from_struct(weights2)
    ]
    :evision_nif.cuda_blendLinear(positional)
    |> to_struct()
  end

  @doc """
  buildWarpAffineMaps

  ##### Positional Arguments
  - **m**: `Evision.Mat`
  - **inverse**: `bool`
  - **dsize**: `Size`

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **xmap**: `Evision.CUDA.GpuMat.t()`.
  - **ymap**: `Evision.CUDA.GpuMat.t()`.

  Python prototype (for reference only):
  ```python3
  buildWarpAffineMaps(M, inverse, dsize[, xmap[, ymap[, stream]]]) -> xmap, ymap
  ```
  """
  @spec buildWarpAffineMaps(Evision.Mat.maybe_mat_in(), boolean(), {number(), number()}, [{:stream, term()}] | nil) :: {Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t()} | {:error, String.t()}
  def buildWarpAffineMaps(m, inverse, dsize, opts) when (is_struct(m, Evision.Mat) or is_struct(m, Nx.Tensor) or is_number(m) or is_tuple(m)) and is_boolean(inverse) and is_tuple(dsize) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      m: Evision.Internal.Structurise.from_struct(m),
      inverse: Evision.Internal.Structurise.from_struct(inverse),
      dsize: Evision.Internal.Structurise.from_struct(dsize)
    ]
    :evision_nif.cuda_buildWarpAffineMaps(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  buildWarpAffineMaps

  ##### Positional Arguments
  - **m**: `Evision.Mat`
  - **inverse**: `bool`
  - **dsize**: `Size`

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **xmap**: `Evision.CUDA.GpuMat.t()`.
  - **ymap**: `Evision.CUDA.GpuMat.t()`.

  Python prototype (for reference only):
  ```python3
  buildWarpAffineMaps(M, inverse, dsize[, xmap[, ymap[, stream]]]) -> xmap, ymap
  ```
  """
  @spec buildWarpAffineMaps(Evision.Mat.maybe_mat_in(), boolean(), {number(), number()}) :: {Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t()} | {:error, String.t()}
  def buildWarpAffineMaps(m, inverse, dsize) when (is_struct(m, Evision.Mat) or is_struct(m, Nx.Tensor) or is_number(m) or is_tuple(m)) and is_boolean(inverse) and is_tuple(dsize)
  do
    positional = [
      m: Evision.Internal.Structurise.from_struct(m),
      inverse: Evision.Internal.Structurise.from_struct(inverse),
      dsize: Evision.Internal.Structurise.from_struct(dsize)
    ]
    :evision_nif.cuda_buildWarpAffineMaps(positional)
    |> to_struct()
  end

  @doc """
  buildWarpPerspectiveMaps

  ##### Positional Arguments
  - **m**: `Evision.Mat`
  - **inverse**: `bool`
  - **dsize**: `Size`

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **xmap**: `Evision.CUDA.GpuMat.t()`.
  - **ymap**: `Evision.CUDA.GpuMat.t()`.

  Python prototype (for reference only):
  ```python3
  buildWarpPerspectiveMaps(M, inverse, dsize[, xmap[, ymap[, stream]]]) -> xmap, ymap
  ```
  """
  @spec buildWarpPerspectiveMaps(Evision.Mat.maybe_mat_in(), boolean(), {number(), number()}, [{:stream, term()}] | nil) :: {Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t()} | {:error, String.t()}
  def buildWarpPerspectiveMaps(m, inverse, dsize, opts) when (is_struct(m, Evision.Mat) or is_struct(m, Nx.Tensor) or is_number(m) or is_tuple(m)) and is_boolean(inverse) and is_tuple(dsize) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      m: Evision.Internal.Structurise.from_struct(m),
      inverse: Evision.Internal.Structurise.from_struct(inverse),
      dsize: Evision.Internal.Structurise.from_struct(dsize)
    ]
    :evision_nif.cuda_buildWarpPerspectiveMaps(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  buildWarpPerspectiveMaps

  ##### Positional Arguments
  - **m**: `Evision.Mat`
  - **inverse**: `bool`
  - **dsize**: `Size`

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **xmap**: `Evision.CUDA.GpuMat.t()`.
  - **ymap**: `Evision.CUDA.GpuMat.t()`.

  Python prototype (for reference only):
  ```python3
  buildWarpPerspectiveMaps(M, inverse, dsize[, xmap[, ymap[, stream]]]) -> xmap, ymap
  ```
  """
  @spec buildWarpPerspectiveMaps(Evision.Mat.maybe_mat_in(), boolean(), {number(), number()}) :: {Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t()} | {:error, String.t()}
  def buildWarpPerspectiveMaps(m, inverse, dsize) when (is_struct(m, Evision.Mat) or is_struct(m, Nx.Tensor) or is_number(m) or is_tuple(m)) and is_boolean(inverse) and is_tuple(dsize)
  do
    positional = [
      m: Evision.Internal.Structurise.from_struct(m),
      inverse: Evision.Internal.Structurise.from_struct(inverse),
      dsize: Evision.Internal.Structurise.from_struct(dsize)
    ]
    :evision_nif.cuda_buildWarpPerspectiveMaps(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  calcAbsSum

  ##### Positional Arguments
  - **src**: `Evision.Mat`

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  calcAbsSum(src[, dst[, mask[, stream]]]) -> dst
  ```
  #### Variant 2:
  calcAbsSum

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`

  ##### Keyword Arguments
  - **mask**: `Evision.CUDA.GpuMat.t()`.
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  calcAbsSum(src[, dst[, mask[, stream]]]) -> dst
  ```

  """
  @spec calcAbsSum(Evision.Mat.maybe_mat_in(), [{:mask, term()} | {:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def calcAbsSum(src, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:mask, :stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_calcAbsSum(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec calcAbsSum(Evision.CUDA.GpuMat.t(), [{:mask, term()} | {:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def calcAbsSum(src, opts) when is_struct(src, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:mask, :stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_calcAbsSum(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  calcAbsSum

  ##### Positional Arguments
  - **src**: `Evision.Mat`

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  calcAbsSum(src[, dst[, mask[, stream]]]) -> dst
  ```
  #### Variant 2:
  calcAbsSum

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`

  ##### Keyword Arguments
  - **mask**: `Evision.CUDA.GpuMat.t()`.
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  calcAbsSum(src[, dst[, mask[, stream]]]) -> dst
  ```

  """
  @spec calcAbsSum(Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def calcAbsSum(src) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_calcAbsSum(positional)
    |> to_struct()
  end
  @spec calcAbsSum(Evision.CUDA.GpuMat.t()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def calcAbsSum(src) when is_struct(src, Evision.CUDA.GpuMat)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_calcAbsSum(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Calculates histogram for one channel 8-bit image confined in given mask.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source image with CV_8UC1 type.

  - **mask**: `Evision.Mat`.

    A mask image same size as src and of type CV_8UC1.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **hist**: `Evision.Mat.t()`.

    Destination histogram with one row, 256 columns, and the CV_32SC1 type.

  Python prototype (for reference only):
  ```python3
  calcHist(src, mask[, hist[, stream]]) -> hist
  ```
  #### Variant 2:
  Calculates histogram for one channel 8-bit image confined in given mask.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source image with CV_8UC1 type.

  - **mask**: `Evision.CUDA.GpuMat.t()`.

    A mask image same size as src and of type CV_8UC1.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **hist**: `Evision.CUDA.GpuMat.t()`.

    Destination histogram with one row, 256 columns, and the CV_32SC1 type.

  Python prototype (for reference only):
  ```python3
  calcHist(src, mask[, hist[, stream]]) -> hist
  ```

  """
  @spec calcHist(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def calcHist(src, mask, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (is_struct(mask, Evision.Mat) or is_struct(mask, Nx.Tensor) or is_number(mask) or is_tuple(mask)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      mask: Evision.Internal.Structurise.from_struct(mask)
    ]
    :evision_nif.cuda_calcHist(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec calcHist(Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t(), [{:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def calcHist(src, mask, opts) when is_struct(src, Evision.CUDA.GpuMat) and is_struct(mask, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      mask: Evision.Internal.Structurise.from_struct(mask)
    ]
    :evision_nif.cuda_calcHist(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Calculates histogram for one channel 8-bit image confined in given mask.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source image with CV_8UC1 type.

  - **mask**: `Evision.Mat`.

    A mask image same size as src and of type CV_8UC1.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **hist**: `Evision.Mat.t()`.

    Destination histogram with one row, 256 columns, and the CV_32SC1 type.

  Python prototype (for reference only):
  ```python3
  calcHist(src, mask[, hist[, stream]]) -> hist
  ```
  #### Variant 2:
  Calculates histogram for one channel 8-bit image confined in given mask.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source image with CV_8UC1 type.

  - **mask**: `Evision.CUDA.GpuMat.t()`.

    A mask image same size as src and of type CV_8UC1.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **hist**: `Evision.CUDA.GpuMat.t()`.

    Destination histogram with one row, 256 columns, and the CV_32SC1 type.

  Python prototype (for reference only):
  ```python3
  calcHist(src, mask[, hist[, stream]]) -> hist
  ```
  #### Variant 3:
  Calculates histogram for one channel 8-bit image.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source image with CV_8UC1 type.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **hist**: `Evision.Mat.t()`.

    Destination histogram with one row, 256 columns, and the CV_32SC1 type.

  Python prototype (for reference only):
  ```python3
  calcHist(src[, hist[, stream]]) -> hist
  ```
  #### Variant 4:
  Calculates histogram for one channel 8-bit image.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source image with CV_8UC1 type.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **hist**: `Evision.CUDA.GpuMat.t()`.

    Destination histogram with one row, 256 columns, and the CV_32SC1 type.

  Python prototype (for reference only):
  ```python3
  calcHist(src[, hist[, stream]]) -> hist
  ```

  """
  @spec calcHist(Evision.Mat.maybe_mat_in(), [{:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def calcHist(src, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_calcHist(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec calcHist(Evision.CUDA.GpuMat.t(), [{:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def calcHist(src, opts) when is_struct(src, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_calcHist(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec calcHist(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def calcHist(src, mask) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (is_struct(mask, Evision.Mat) or is_struct(mask, Nx.Tensor) or is_number(mask) or is_tuple(mask))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      mask: Evision.Internal.Structurise.from_struct(mask)
    ]
    :evision_nif.cuda_calcHist(positional)
    |> to_struct()
  end
  @spec calcHist(Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def calcHist(src, mask) when is_struct(src, Evision.CUDA.GpuMat) and is_struct(mask, Evision.CUDA.GpuMat)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      mask: Evision.Internal.Structurise.from_struct(mask)
    ]
    :evision_nif.cuda_calcHist(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Calculates histogram for one channel 8-bit image.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source image with CV_8UC1 type.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **hist**: `Evision.Mat.t()`.

    Destination histogram with one row, 256 columns, and the CV_32SC1 type.

  Python prototype (for reference only):
  ```python3
  calcHist(src[, hist[, stream]]) -> hist
  ```
  #### Variant 2:
  Calculates histogram for one channel 8-bit image.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source image with CV_8UC1 type.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **hist**: `Evision.CUDA.GpuMat.t()`.

    Destination histogram with one row, 256 columns, and the CV_32SC1 type.

  Python prototype (for reference only):
  ```python3
  calcHist(src[, hist[, stream]]) -> hist
  ```

  """
  @spec calcHist(Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def calcHist(src) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_calcHist(positional)
    |> to_struct()
  end
  @spec calcHist(Evision.CUDA.GpuMat.t()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def calcHist(src) when is_struct(src, Evision.CUDA.GpuMat)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_calcHist(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  calcNorm

  ##### Positional Arguments
  - **src**: `Evision.Mat`
  - **normType**: `integer()`

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  calcNorm(src, normType[, dst[, mask[, stream]]]) -> dst
  ```
  #### Variant 2:
  calcNorm

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`
  - **normType**: `integer()`

  ##### Keyword Arguments
  - **mask**: `Evision.CUDA.GpuMat.t()`.
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  calcNorm(src, normType[, dst[, mask[, stream]]]) -> dst
  ```

  """
  @spec calcNorm(Evision.Mat.maybe_mat_in(), integer(), [{:mask, term()} | {:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def calcNorm(src, normType, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_integer(normType) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:mask, :stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      normType: Evision.Internal.Structurise.from_struct(normType)
    ]
    :evision_nif.cuda_calcNorm(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec calcNorm(Evision.CUDA.GpuMat.t(), integer(), [{:mask, term()} | {:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def calcNorm(src, normType, opts) when is_struct(src, Evision.CUDA.GpuMat) and is_integer(normType) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:mask, :stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      normType: Evision.Internal.Structurise.from_struct(normType)
    ]
    :evision_nif.cuda_calcNorm(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  calcNorm

  ##### Positional Arguments
  - **src**: `Evision.Mat`
  - **normType**: `integer()`

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  calcNorm(src, normType[, dst[, mask[, stream]]]) -> dst
  ```
  #### Variant 2:
  calcNorm

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`
  - **normType**: `integer()`

  ##### Keyword Arguments
  - **mask**: `Evision.CUDA.GpuMat.t()`.
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  calcNorm(src, normType[, dst[, mask[, stream]]]) -> dst
  ```

  """
  @spec calcNorm(Evision.Mat.maybe_mat_in(), integer()) :: Evision.Mat.t() | {:error, String.t()}
  def calcNorm(src, normType) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_integer(normType)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      normType: Evision.Internal.Structurise.from_struct(normType)
    ]
    :evision_nif.cuda_calcNorm(positional)
    |> to_struct()
  end
  @spec calcNorm(Evision.CUDA.GpuMat.t(), integer()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def calcNorm(src, normType) when is_struct(src, Evision.CUDA.GpuMat) and is_integer(normType)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      normType: Evision.Internal.Structurise.from_struct(normType)
    ]
    :evision_nif.cuda_calcNorm(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  calcNormDiff

  ##### Positional Arguments
  - **src1**: `Evision.Mat`
  - **src2**: `Evision.Mat`

  ##### Keyword Arguments
  - **normType**: `integer()`.
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  calcNormDiff(src1, src2[, dst[, normType[, stream]]]) -> dst
  ```
  #### Variant 2:
  calcNormDiff

  ##### Positional Arguments
  - **src1**: `Evision.CUDA.GpuMat.t()`
  - **src2**: `Evision.CUDA.GpuMat.t()`

  ##### Keyword Arguments
  - **normType**: `integer()`.
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  calcNormDiff(src1, src2[, dst[, normType[, stream]]]) -> dst
  ```

  """
  @spec calcNormDiff(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:normType, term()} | {:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def calcNormDiff(src1, src2, opts) when (is_struct(src1, Evision.Mat) or is_struct(src1, Nx.Tensor) or is_number(src1) or is_tuple(src1)) and (is_struct(src2, Evision.Mat) or is_struct(src2, Nx.Tensor) or is_number(src2) or is_tuple(src2)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:normType, :stream])
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1),
      src2: Evision.Internal.Structurise.from_struct(src2)
    ]
    :evision_nif.cuda_calcNormDiff(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec calcNormDiff(Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t(), [{:normType, term()} | {:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def calcNormDiff(src1, src2, opts) when is_struct(src1, Evision.CUDA.GpuMat) and is_struct(src2, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:normType, :stream])
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1),
      src2: Evision.Internal.Structurise.from_struct(src2)
    ]
    :evision_nif.cuda_calcNormDiff(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  calcNormDiff

  ##### Positional Arguments
  - **src1**: `Evision.Mat`
  - **src2**: `Evision.Mat`

  ##### Keyword Arguments
  - **normType**: `integer()`.
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  calcNormDiff(src1, src2[, dst[, normType[, stream]]]) -> dst
  ```
  #### Variant 2:
  calcNormDiff

  ##### Positional Arguments
  - **src1**: `Evision.CUDA.GpuMat.t()`
  - **src2**: `Evision.CUDA.GpuMat.t()`

  ##### Keyword Arguments
  - **normType**: `integer()`.
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  calcNormDiff(src1, src2[, dst[, normType[, stream]]]) -> dst
  ```

  """
  @spec calcNormDiff(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def calcNormDiff(src1, src2) when (is_struct(src1, Evision.Mat) or is_struct(src1, Nx.Tensor) or is_number(src1) or is_tuple(src1)) and (is_struct(src2, Evision.Mat) or is_struct(src2, Nx.Tensor) or is_number(src2) or is_tuple(src2))
  do
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1),
      src2: Evision.Internal.Structurise.from_struct(src2)
    ]
    :evision_nif.cuda_calcNormDiff(positional)
    |> to_struct()
  end
  @spec calcNormDiff(Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def calcNormDiff(src1, src2) when is_struct(src1, Evision.CUDA.GpuMat) and is_struct(src2, Evision.CUDA.GpuMat)
  do
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1),
      src2: Evision.Internal.Structurise.from_struct(src2)
    ]
    :evision_nif.cuda_calcNormDiff(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  calcSqrSum

  ##### Positional Arguments
  - **src**: `Evision.Mat`

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  calcSqrSum(src[, dst[, mask[, stream]]]) -> dst
  ```
  #### Variant 2:
  calcSqrSum

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`

  ##### Keyword Arguments
  - **mask**: `Evision.CUDA.GpuMat.t()`.
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  calcSqrSum(src[, dst[, mask[, stream]]]) -> dst
  ```

  """
  @spec calcSqrSum(Evision.Mat.maybe_mat_in(), [{:mask, term()} | {:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def calcSqrSum(src, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:mask, :stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_calcSqrSum(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec calcSqrSum(Evision.CUDA.GpuMat.t(), [{:mask, term()} | {:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def calcSqrSum(src, opts) when is_struct(src, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:mask, :stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_calcSqrSum(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  calcSqrSum

  ##### Positional Arguments
  - **src**: `Evision.Mat`

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  calcSqrSum(src[, dst[, mask[, stream]]]) -> dst
  ```
  #### Variant 2:
  calcSqrSum

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`

  ##### Keyword Arguments
  - **mask**: `Evision.CUDA.GpuMat.t()`.
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  calcSqrSum(src[, dst[, mask[, stream]]]) -> dst
  ```

  """
  @spec calcSqrSum(Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def calcSqrSum(src) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_calcSqrSum(positional)
    |> to_struct()
  end
  @spec calcSqrSum(Evision.CUDA.GpuMat.t()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def calcSqrSum(src) when is_struct(src, Evision.CUDA.GpuMat)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_calcSqrSum(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  calcSum

  ##### Positional Arguments
  - **src**: `Evision.Mat`

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  calcSum(src[, dst[, mask[, stream]]]) -> dst
  ```
  #### Variant 2:
  calcSum

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`

  ##### Keyword Arguments
  - **mask**: `Evision.CUDA.GpuMat.t()`.
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  calcSum(src[, dst[, mask[, stream]]]) -> dst
  ```

  """
  @spec calcSum(Evision.Mat.maybe_mat_in(), [{:mask, term()} | {:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def calcSum(src, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:mask, :stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_calcSum(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec calcSum(Evision.CUDA.GpuMat.t(), [{:mask, term()} | {:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def calcSum(src, opts) when is_struct(src, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:mask, :stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_calcSum(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  calcSum

  ##### Positional Arguments
  - **src**: `Evision.Mat`

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  calcSum(src[, dst[, mask[, stream]]]) -> dst
  ```
  #### Variant 2:
  calcSum

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`

  ##### Keyword Arguments
  - **mask**: `Evision.CUDA.GpuMat.t()`.
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  calcSum(src[, dst[, mask[, stream]]]) -> dst
  ```

  """
  @spec calcSum(Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def calcSum(src) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_calcSum(positional)
    |> to_struct()
  end
  @spec calcSum(Evision.CUDA.GpuMat.t()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def calcSum(src) when is_struct(src, Evision.CUDA.GpuMat)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_calcSum(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Converts Cartesian coordinates into polar.

  ##### Positional Arguments
  - **x**: `Evision.Mat`.

    Source matrix containing real components ( CV_32FC1 ).

  - **y**: `Evision.Mat`.

    Source matrix containing imaginary components ( CV_32FC1 ).

  ##### Keyword Arguments
  - **angleInDegrees**: `bool`.

    Flag for angles that must be evaluated in degrees.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **magnitude**: `Evision.Mat.t()`.

    Destination matrix of float magnitudes ( CV_32FC1 ).

  - **angle**: `Evision.Mat.t()`.

    Destination matrix of angles ( CV_32FC1 ).

  @sa cartToPolar

  Python prototype (for reference only):
  ```python3
  cartToPolar(x, y[, magnitude[, angle[, angleInDegrees[, stream]]]]) -> magnitude, angle
  ```
  #### Variant 2:
  Converts Cartesian coordinates into polar.

  ##### Positional Arguments
  - **x**: `Evision.CUDA.GpuMat.t()`.

    Source matrix containing real components ( CV_32FC1 ).

  - **y**: `Evision.CUDA.GpuMat.t()`.

    Source matrix containing imaginary components ( CV_32FC1 ).

  ##### Keyword Arguments
  - **angleInDegrees**: `bool`.

    Flag for angles that must be evaluated in degrees.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **magnitude**: `Evision.CUDA.GpuMat.t()`.

    Destination matrix of float magnitudes ( CV_32FC1 ).

  - **angle**: `Evision.CUDA.GpuMat.t()`.

    Destination matrix of angles ( CV_32FC1 ).

  @sa cartToPolar

  Python prototype (for reference only):
  ```python3
  cartToPolar(x, y[, magnitude[, angle[, angleInDegrees[, stream]]]]) -> magnitude, angle
  ```

  """
  @spec cartToPolar(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:angleInDegrees, term()} | {:stream, term()}] | nil) :: {Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def cartToPolar(x, y, opts) when (is_struct(x, Evision.Mat) or is_struct(x, Nx.Tensor) or is_number(x) or is_tuple(x)) and (is_struct(y, Evision.Mat) or is_struct(y, Nx.Tensor) or is_number(y) or is_tuple(y)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:angleInDegrees, :stream])
    positional = [
      x: Evision.Internal.Structurise.from_struct(x),
      y: Evision.Internal.Structurise.from_struct(y)
    ]
    :evision_nif.cuda_cartToPolar(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec cartToPolar(Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t(), [{:angleInDegrees, term()} | {:stream, term()}] | nil) :: {Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t()} | {:error, String.t()}
  def cartToPolar(x, y, opts) when is_struct(x, Evision.CUDA.GpuMat) and is_struct(y, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:angleInDegrees, :stream])
    positional = [
      x: Evision.Internal.Structurise.from_struct(x),
      y: Evision.Internal.Structurise.from_struct(y)
    ]
    :evision_nif.cuda_cartToPolar(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Converts Cartesian coordinates into polar.

  ##### Positional Arguments
  - **x**: `Evision.Mat`.

    Source matrix containing real components ( CV_32FC1 ).

  - **y**: `Evision.Mat`.

    Source matrix containing imaginary components ( CV_32FC1 ).

  ##### Keyword Arguments
  - **angleInDegrees**: `bool`.

    Flag for angles that must be evaluated in degrees.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **magnitude**: `Evision.Mat.t()`.

    Destination matrix of float magnitudes ( CV_32FC1 ).

  - **angle**: `Evision.Mat.t()`.

    Destination matrix of angles ( CV_32FC1 ).

  @sa cartToPolar

  Python prototype (for reference only):
  ```python3
  cartToPolar(x, y[, magnitude[, angle[, angleInDegrees[, stream]]]]) -> magnitude, angle
  ```
  #### Variant 2:
  Converts Cartesian coordinates into polar.

  ##### Positional Arguments
  - **x**: `Evision.CUDA.GpuMat.t()`.

    Source matrix containing real components ( CV_32FC1 ).

  - **y**: `Evision.CUDA.GpuMat.t()`.

    Source matrix containing imaginary components ( CV_32FC1 ).

  ##### Keyword Arguments
  - **angleInDegrees**: `bool`.

    Flag for angles that must be evaluated in degrees.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **magnitude**: `Evision.CUDA.GpuMat.t()`.

    Destination matrix of float magnitudes ( CV_32FC1 ).

  - **angle**: `Evision.CUDA.GpuMat.t()`.

    Destination matrix of angles ( CV_32FC1 ).

  @sa cartToPolar

  Python prototype (for reference only):
  ```python3
  cartToPolar(x, y[, magnitude[, angle[, angleInDegrees[, stream]]]]) -> magnitude, angle
  ```

  """
  @spec cartToPolar(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: {Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def cartToPolar(x, y) when (is_struct(x, Evision.Mat) or is_struct(x, Nx.Tensor) or is_number(x) or is_tuple(x)) and (is_struct(y, Evision.Mat) or is_struct(y, Nx.Tensor) or is_number(y) or is_tuple(y))
  do
    positional = [
      x: Evision.Internal.Structurise.from_struct(x),
      y: Evision.Internal.Structurise.from_struct(y)
    ]
    :evision_nif.cuda_cartToPolar(positional)
    |> to_struct()
  end
  @spec cartToPolar(Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t()) :: {Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t()} | {:error, String.t()}
  def cartToPolar(x, y) when is_struct(x, Evision.CUDA.GpuMat) and is_struct(y, Evision.CUDA.GpuMat)
  do
    positional = [
      x: Evision.Internal.Structurise.from_struct(x),
      y: Evision.Internal.Structurise.from_struct(y)
    ]
    :evision_nif.cuda_cartToPolar(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Compares elements of two matrices (or of a matrix and scalar).

  ##### Positional Arguments
  - **src1**: `Evision.Mat`.

    First source matrix or scalar.

  - **src2**: `Evision.Mat`.

    Second source matrix or scalar.

  - **cmpop**: `integer()`.

    Flag specifying the relation between the elements to be checked:
    - **CMP_EQ:** a(.) == b(.)
    - **CMP_GT:** a(.) \\> b(.)
    - **CMP_GE:** a(.) \\>= b(.)
    - **CMP_LT:** a(.) \\< b(.)
    - **CMP_LE:** a(.) \\<= b(.)
    - **CMP_NE:** a(.) != b(.)

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination matrix that has the same size as the input array(s) and type CV_8U.

  @sa compare

  Python prototype (for reference only):
  ```python3
  compare(src1, src2, cmpop[, dst[, stream]]) -> dst
  ```
  #### Variant 2:
  Compares elements of two matrices (or of a matrix and scalar).

  ##### Positional Arguments
  - **src1**: `Evision.CUDA.GpuMat.t()`.

    First source matrix or scalar.

  - **src2**: `Evision.CUDA.GpuMat.t()`.

    Second source matrix or scalar.

  - **cmpop**: `integer()`.

    Flag specifying the relation between the elements to be checked:
    - **CMP_EQ:** a(.) == b(.)
    - **CMP_GT:** a(.) \\> b(.)
    - **CMP_GE:** a(.) \\>= b(.)
    - **CMP_LT:** a(.) \\< b(.)
    - **CMP_LE:** a(.) \\<= b(.)
    - **CMP_NE:** a(.) != b(.)

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination matrix that has the same size as the input array(s) and type CV_8U.

  @sa compare

  Python prototype (for reference only):
  ```python3
  compare(src1, src2, cmpop[, dst[, stream]]) -> dst
  ```

  """
  @spec compare(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), integer(), [{:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def compare(src1, src2, cmpop, opts) when (is_struct(src1, Evision.Mat) or is_struct(src1, Nx.Tensor) or is_number(src1) or is_tuple(src1)) and (is_struct(src2, Evision.Mat) or is_struct(src2, Nx.Tensor) or is_number(src2) or is_tuple(src2)) and is_integer(cmpop) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1),
      src2: Evision.Internal.Structurise.from_struct(src2),
      cmpop: Evision.Internal.Structurise.from_struct(cmpop)
    ]
    :evision_nif.cuda_compare(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec compare(Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t(), integer(), [{:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def compare(src1, src2, cmpop, opts) when is_struct(src1, Evision.CUDA.GpuMat) and is_struct(src2, Evision.CUDA.GpuMat) and is_integer(cmpop) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1),
      src2: Evision.Internal.Structurise.from_struct(src2),
      cmpop: Evision.Internal.Structurise.from_struct(cmpop)
    ]
    :evision_nif.cuda_compare(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Compares elements of two matrices (or of a matrix and scalar).

  ##### Positional Arguments
  - **src1**: `Evision.Mat`.

    First source matrix or scalar.

  - **src2**: `Evision.Mat`.

    Second source matrix or scalar.

  - **cmpop**: `integer()`.

    Flag specifying the relation between the elements to be checked:
    - **CMP_EQ:** a(.) == b(.)
    - **CMP_GT:** a(.) \\> b(.)
    - **CMP_GE:** a(.) \\>= b(.)
    - **CMP_LT:** a(.) \\< b(.)
    - **CMP_LE:** a(.) \\<= b(.)
    - **CMP_NE:** a(.) != b(.)

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination matrix that has the same size as the input array(s) and type CV_8U.

  @sa compare

  Python prototype (for reference only):
  ```python3
  compare(src1, src2, cmpop[, dst[, stream]]) -> dst
  ```
  #### Variant 2:
  Compares elements of two matrices (or of a matrix and scalar).

  ##### Positional Arguments
  - **src1**: `Evision.CUDA.GpuMat.t()`.

    First source matrix or scalar.

  - **src2**: `Evision.CUDA.GpuMat.t()`.

    Second source matrix or scalar.

  - **cmpop**: `integer()`.

    Flag specifying the relation between the elements to be checked:
    - **CMP_EQ:** a(.) == b(.)
    - **CMP_GT:** a(.) \\> b(.)
    - **CMP_GE:** a(.) \\>= b(.)
    - **CMP_LT:** a(.) \\< b(.)
    - **CMP_LE:** a(.) \\<= b(.)
    - **CMP_NE:** a(.) != b(.)

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination matrix that has the same size as the input array(s) and type CV_8U.

  @sa compare

  Python prototype (for reference only):
  ```python3
  compare(src1, src2, cmpop[, dst[, stream]]) -> dst
  ```

  """
  @spec compare(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), integer()) :: Evision.Mat.t() | {:error, String.t()}
  def compare(src1, src2, cmpop) when (is_struct(src1, Evision.Mat) or is_struct(src1, Nx.Tensor) or is_number(src1) or is_tuple(src1)) and (is_struct(src2, Evision.Mat) or is_struct(src2, Nx.Tensor) or is_number(src2) or is_tuple(src2)) and is_integer(cmpop)
  do
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1),
      src2: Evision.Internal.Structurise.from_struct(src2),
      cmpop: Evision.Internal.Structurise.from_struct(cmpop)
    ]
    :evision_nif.cuda_compare(positional)
    |> to_struct()
  end
  @spec compare(Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t(), integer()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def compare(src1, src2, cmpop) when is_struct(src1, Evision.CUDA.GpuMat) and is_struct(src2, Evision.CUDA.GpuMat) and is_integer(cmpop)
  do
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1),
      src2: Evision.Internal.Structurise.from_struct(src2),
      cmpop: Evision.Internal.Structurise.from_struct(cmpop)
    ]
    :evision_nif.cuda_compare(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  connectedComponents

  ##### Positional Arguments
  - **image**: `Evision.Mat`.

    The 8-bit single-channel image to be labeled.

  ##### Keyword Arguments
  - **connectivity**: `integer()`.

    Connectivity to use for the labeling procedure. 8 for 8-way connectivity is supported.

  - **ltype**: `integer()`.

    Output image label type. Currently CV_32S is supported.

  ##### Return
  - **labels**: `Evision.Mat.t()`.

    Destination labeled image.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  connectedComponents(image[, labels[, connectivity[, ltype]]]) -> labels
  ```
  #### Variant 2:
  connectedComponents

  ##### Positional Arguments
  - **image**: `Evision.CUDA.GpuMat.t()`.

    The 8-bit single-channel image to be labeled.

  ##### Keyword Arguments
  - **connectivity**: `integer()`.

    Connectivity to use for the labeling procedure. 8 for 8-way connectivity is supported.

  - **ltype**: `integer()`.

    Output image label type. Currently CV_32S is supported.

  ##### Return
  - **labels**: `Evision.CUDA.GpuMat.t()`.

    Destination labeled image.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  connectedComponents(image[, labels[, connectivity[, ltype]]]) -> labels
  ```

  """
  @spec connectedComponents(Evision.Mat.maybe_mat_in(), [{:connectivity, term()} | {:ltype, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def connectedComponents(image, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:connectivity, :ltype])
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.cuda_connectedComponents(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec connectedComponents(Evision.CUDA.GpuMat.t(), [{:connectivity, term()} | {:ltype, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def connectedComponents(image, opts) when is_struct(image, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:connectivity, :ltype])
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.cuda_connectedComponents(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  connectedComponents

  ##### Positional Arguments
  - **image**: `Evision.Mat`.

    The 8-bit single-channel image to be labeled.

  ##### Keyword Arguments
  - **connectivity**: `integer()`.

    Connectivity to use for the labeling procedure. 8 for 8-way connectivity is supported.

  - **ltype**: `integer()`.

    Output image label type. Currently CV_32S is supported.

  ##### Return
  - **labels**: `Evision.Mat.t()`.

    Destination labeled image.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  connectedComponents(image[, labels[, connectivity[, ltype]]]) -> labels
  ```
  #### Variant 2:
  connectedComponents

  ##### Positional Arguments
  - **image**: `Evision.CUDA.GpuMat.t()`.

    The 8-bit single-channel image to be labeled.

  ##### Keyword Arguments
  - **connectivity**: `integer()`.

    Connectivity to use for the labeling procedure. 8 for 8-way connectivity is supported.

  - **ltype**: `integer()`.

    Output image label type. Currently CV_32S is supported.

  ##### Return
  - **labels**: `Evision.CUDA.GpuMat.t()`.

    Destination labeled image.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  connectedComponents(image[, labels[, connectivity[, ltype]]]) -> labels
  ```

  """
  @spec connectedComponents(Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def connectedComponents(image) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.cuda_connectedComponents(positional)
    |> to_struct()
  end
  @spec connectedComponents(Evision.CUDA.GpuMat.t()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def connectedComponents(image) when is_struct(image, Evision.CUDA.GpuMat)
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.cuda_connectedComponents(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Computes the Connected Components Labeled image of a binary image.

  ##### Positional Arguments
  - **image**: `Evision.Mat`.

    The 8-bit single-channel image to be labeled.

  - **connectivity**: `integer()`.

    Connectivity to use for the labeling procedure. 8 for 8-way connectivity is supported.

  - **ltype**: `integer()`.

    Output image label type. Currently CV_32S is supported.

  - **ccltype**: `cuda_ConnectedComponentsAlgorithmsTypes`.

    Connected components algorithm type (see the #ConnectedComponentsAlgorithmsTypes).

  ##### Return
  - **labels**: `Evision.Mat.t()`.

    Destination labeled image.

  The function takes as input a binary image and performs Connected Components Labeling. The output
  is an image where each Connected Component is assigned a unique label (integer value).
  ltype specifies the output label image type, an important consideration based on the total
  number of labels or alternatively the total number of pixels in the source image.
  ccltype specifies the connected components labeling algorithm to use, currently
  BKE @cite Allegretti2019 is supported, see the #ConnectedComponentsAlgorithmsTypes
  for details. Note that labels in the output are not required to be sequential.

  **Note**: A sample program demonstrating Connected Components Labeling in CUDA can be found at\\n
  opencv_contrib_source_code/modules/cudaimgproc/samples/connected_components.cpp

  Python prototype (for reference only):
  ```python3
  connectedComponentsWithAlgorithm(image, connectivity, ltype, ccltype[, labels]) -> labels
  ```
  #### Variant 2:
  Computes the Connected Components Labeled image of a binary image.

  ##### Positional Arguments
  - **image**: `Evision.CUDA.GpuMat.t()`.

    The 8-bit single-channel image to be labeled.

  - **connectivity**: `integer()`.

    Connectivity to use for the labeling procedure. 8 for 8-way connectivity is supported.

  - **ltype**: `integer()`.

    Output image label type. Currently CV_32S is supported.

  - **ccltype**: `cuda_ConnectedComponentsAlgorithmsTypes`.

    Connected components algorithm type (see the #ConnectedComponentsAlgorithmsTypes).

  ##### Return
  - **labels**: `Evision.CUDA.GpuMat.t()`.

    Destination labeled image.

  The function takes as input a binary image and performs Connected Components Labeling. The output
  is an image where each Connected Component is assigned a unique label (integer value).
  ltype specifies the output label image type, an important consideration based on the total
  number of labels or alternatively the total number of pixels in the source image.
  ccltype specifies the connected components labeling algorithm to use, currently
  BKE @cite Allegretti2019 is supported, see the #ConnectedComponentsAlgorithmsTypes
  for details. Note that labels in the output are not required to be sequential.

  **Note**: A sample program demonstrating Connected Components Labeling in CUDA can be found at\\n
  opencv_contrib_source_code/modules/cudaimgproc/samples/connected_components.cpp

  Python prototype (for reference only):
  ```python3
  connectedComponentsWithAlgorithm(image, connectivity, ltype, ccltype[, labels]) -> labels
  ```

  """
  @spec connectedComponentsWithAlgorithm(Evision.Mat.maybe_mat_in(), integer(), integer(), Evision.ConnectedComponentsAlgorithmsTypes.enum(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def connectedComponentsWithAlgorithm(image, connectivity, ltype, ccltype, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and is_integer(connectivity) and is_integer(ltype) and is_integer(ccltype) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      connectivity: Evision.Internal.Structurise.from_struct(connectivity),
      ltype: Evision.Internal.Structurise.from_struct(ltype),
      ccltype: Evision.Internal.Structurise.from_struct(ccltype)
    ]
    :evision_nif.cuda_connectedComponentsWithAlgorithm(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec connectedComponentsWithAlgorithm(Evision.CUDA.GpuMat.t(), integer(), integer(), Evision.ConnectedComponentsAlgorithmsTypes.enum(), [{atom(), term()},...] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def connectedComponentsWithAlgorithm(image, connectivity, ltype, ccltype, opts) when is_struct(image, Evision.CUDA.GpuMat) and is_integer(connectivity) and is_integer(ltype) and is_integer(ccltype) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      connectivity: Evision.Internal.Structurise.from_struct(connectivity),
      ltype: Evision.Internal.Structurise.from_struct(ltype),
      ccltype: Evision.Internal.Structurise.from_struct(ccltype)
    ]
    :evision_nif.cuda_connectedComponentsWithAlgorithm(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Computes the Connected Components Labeled image of a binary image.

  ##### Positional Arguments
  - **image**: `Evision.Mat`.

    The 8-bit single-channel image to be labeled.

  - **connectivity**: `integer()`.

    Connectivity to use for the labeling procedure. 8 for 8-way connectivity is supported.

  - **ltype**: `integer()`.

    Output image label type. Currently CV_32S is supported.

  - **ccltype**: `cuda_ConnectedComponentsAlgorithmsTypes`.

    Connected components algorithm type (see the #ConnectedComponentsAlgorithmsTypes).

  ##### Return
  - **labels**: `Evision.Mat.t()`.

    Destination labeled image.

  The function takes as input a binary image and performs Connected Components Labeling. The output
  is an image where each Connected Component is assigned a unique label (integer value).
  ltype specifies the output label image type, an important consideration based on the total
  number of labels or alternatively the total number of pixels in the source image.
  ccltype specifies the connected components labeling algorithm to use, currently
  BKE @cite Allegretti2019 is supported, see the #ConnectedComponentsAlgorithmsTypes
  for details. Note that labels in the output are not required to be sequential.

  **Note**: A sample program demonstrating Connected Components Labeling in CUDA can be found at\\n
  opencv_contrib_source_code/modules/cudaimgproc/samples/connected_components.cpp

  Python prototype (for reference only):
  ```python3
  connectedComponentsWithAlgorithm(image, connectivity, ltype, ccltype[, labels]) -> labels
  ```
  #### Variant 2:
  Computes the Connected Components Labeled image of a binary image.

  ##### Positional Arguments
  - **image**: `Evision.CUDA.GpuMat.t()`.

    The 8-bit single-channel image to be labeled.

  - **connectivity**: `integer()`.

    Connectivity to use for the labeling procedure. 8 for 8-way connectivity is supported.

  - **ltype**: `integer()`.

    Output image label type. Currently CV_32S is supported.

  - **ccltype**: `cuda_ConnectedComponentsAlgorithmsTypes`.

    Connected components algorithm type (see the #ConnectedComponentsAlgorithmsTypes).

  ##### Return
  - **labels**: `Evision.CUDA.GpuMat.t()`.

    Destination labeled image.

  The function takes as input a binary image and performs Connected Components Labeling. The output
  is an image where each Connected Component is assigned a unique label (integer value).
  ltype specifies the output label image type, an important consideration based on the total
  number of labels or alternatively the total number of pixels in the source image.
  ccltype specifies the connected components labeling algorithm to use, currently
  BKE @cite Allegretti2019 is supported, see the #ConnectedComponentsAlgorithmsTypes
  for details. Note that labels in the output are not required to be sequential.

  **Note**: A sample program demonstrating Connected Components Labeling in CUDA can be found at\\n
  opencv_contrib_source_code/modules/cudaimgproc/samples/connected_components.cpp

  Python prototype (for reference only):
  ```python3
  connectedComponentsWithAlgorithm(image, connectivity, ltype, ccltype[, labels]) -> labels
  ```

  """
  @spec connectedComponentsWithAlgorithm(Evision.Mat.maybe_mat_in(), integer(), integer(), Evision.ConnectedComponentsAlgorithmsTypes.enum()) :: Evision.Mat.t() | {:error, String.t()}
  def connectedComponentsWithAlgorithm(image, connectivity, ltype, ccltype) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and is_integer(connectivity) and is_integer(ltype) and is_integer(ccltype)
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      connectivity: Evision.Internal.Structurise.from_struct(connectivity),
      ltype: Evision.Internal.Structurise.from_struct(ltype),
      ccltype: Evision.Internal.Structurise.from_struct(ccltype)
    ]
    :evision_nif.cuda_connectedComponentsWithAlgorithm(positional)
    |> to_struct()
  end
  @spec connectedComponentsWithAlgorithm(Evision.CUDA.GpuMat.t(), integer(), integer(), Evision.ConnectedComponentsAlgorithmsTypes.enum()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def connectedComponentsWithAlgorithm(image, connectivity, ltype, ccltype) when is_struct(image, Evision.CUDA.GpuMat) and is_integer(connectivity) and is_integer(ltype) and is_integer(ccltype)
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      connectivity: Evision.Internal.Structurise.from_struct(connectivity),
      ltype: Evision.Internal.Structurise.from_struct(ltype),
      ccltype: Evision.Internal.Structurise.from_struct(ccltype)
    ]
    :evision_nif.cuda_connectedComponentsWithAlgorithm(positional)
    |> to_struct()
  end

  @doc """
  Converts the spatial image moments returned from cuda::spatialMoments to cv::Moments.

  ##### Positional Arguments
  - **spatialMoments**: `Evision.Mat`.

    Spatial moments returned from cuda::spatialMoments.

  - **order**: `MomentsOrder`.

    Order used when calculating image moments with cuda::spatialMoments.

  - **momentsType**: `integer()`.

    Precision used when calculating image moments with cuda::spatialMoments.

  ##### Return
  - **retval**: `Moments`

  @returns cv::Moments.
  @sa cuda::spatialMoments, cuda::moments, cuda::convertSpatialMoments, cuda::numMoments, cuda::MomentsOrder

  Python prototype (for reference only):
  ```python3
  convertSpatialMoments(spatialMoments, order, momentsType) -> retval
  ```
  """
  @spec convertSpatialMoments(Evision.Mat.maybe_mat_in(), Evision.CUDA.MomentsOrder.t(), integer()) :: map() | {:error, String.t()}
  def convertSpatialMoments(spatialMoments, order, momentsType) when (is_struct(spatialMoments, Evision.Mat) or is_struct(spatialMoments, Nx.Tensor) or is_number(spatialMoments) or is_tuple(spatialMoments)) and is_integer(momentsType)
  do
    positional = [
      spatialMoments: Evision.Internal.Structurise.from_struct(spatialMoments),
      order: Evision.Internal.Structurise.from_struct(order),
      momentsType: Evision.Internal.Structurise.from_struct(momentsType)
    ]
    :evision_nif.cuda_convertSpatialMoments(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Forms a border around an image.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source image. CV_8UC1 , CV_8UC4 , CV_32SC1 , and CV_32FC1 types are supported.

  - **top**: `integer()`.

    Number of top pixels

  - **bottom**: `integer()`.

    Number of bottom pixels

  - **left**: `integer()`.

    Number of left pixels

  - **right**: `integer()`.

    Number of pixels in each direction from the source image rectangle to extrapolate.
    For example: top=1, bottom=1, left=1, right=1 mean that 1 pixel-wide border needs to be built.

  - **borderType**: `integer()`.

    Border type. See borderInterpolate for details. BORDER_REFLECT101 ,
    BORDER_REPLICATE , BORDER_CONSTANT , BORDER_REFLECT and BORDER_WRAP are supported for now.

  ##### Keyword Arguments
  - **value**: `Evision.scalar()`.

    Border value.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination image with the same type as src. The size is
    Size(src.cols+left+right, src.rows+top+bottom) .

  Python prototype (for reference only):
  ```python3
  copyMakeBorder(src, top, bottom, left, right, borderType[, dst[, value[, stream]]]) -> dst
  ```
  #### Variant 2:
  Forms a border around an image.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source image. CV_8UC1 , CV_8UC4 , CV_32SC1 , and CV_32FC1 types are supported.

  - **top**: `integer()`.

    Number of top pixels

  - **bottom**: `integer()`.

    Number of bottom pixels

  - **left**: `integer()`.

    Number of left pixels

  - **right**: `integer()`.

    Number of pixels in each direction from the source image rectangle to extrapolate.
    For example: top=1, bottom=1, left=1, right=1 mean that 1 pixel-wide border needs to be built.

  - **borderType**: `integer()`.

    Border type. See borderInterpolate for details. BORDER_REFLECT101 ,
    BORDER_REPLICATE , BORDER_CONSTANT , BORDER_REFLECT and BORDER_WRAP are supported for now.

  ##### Keyword Arguments
  - **value**: `Evision.scalar()`.

    Border value.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination image with the same type as src. The size is
    Size(src.cols+left+right, src.rows+top+bottom) .

  Python prototype (for reference only):
  ```python3
  copyMakeBorder(src, top, bottom, left, right, borderType[, dst[, value[, stream]]]) -> dst
  ```

  """
  @spec copyMakeBorder(Evision.Mat.maybe_mat_in(), integer(), integer(), integer(), integer(), integer(), [{:stream, term()} | {:value, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def copyMakeBorder(src, top, bottom, left, right, borderType, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_integer(top) and is_integer(bottom) and is_integer(left) and is_integer(right) and is_integer(borderType) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream, :value])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      top: Evision.Internal.Structurise.from_struct(top),
      bottom: Evision.Internal.Structurise.from_struct(bottom),
      left: Evision.Internal.Structurise.from_struct(left),
      right: Evision.Internal.Structurise.from_struct(right),
      borderType: Evision.Internal.Structurise.from_struct(borderType)
    ]
    :evision_nif.cuda_copyMakeBorder(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec copyMakeBorder(Evision.CUDA.GpuMat.t(), integer(), integer(), integer(), integer(), integer(), [{:stream, term()} | {:value, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def copyMakeBorder(src, top, bottom, left, right, borderType, opts) when is_struct(src, Evision.CUDA.GpuMat) and is_integer(top) and is_integer(bottom) and is_integer(left) and is_integer(right) and is_integer(borderType) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream, :value])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      top: Evision.Internal.Structurise.from_struct(top),
      bottom: Evision.Internal.Structurise.from_struct(bottom),
      left: Evision.Internal.Structurise.from_struct(left),
      right: Evision.Internal.Structurise.from_struct(right),
      borderType: Evision.Internal.Structurise.from_struct(borderType)
    ]
    :evision_nif.cuda_copyMakeBorder(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Forms a border around an image.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source image. CV_8UC1 , CV_8UC4 , CV_32SC1 , and CV_32FC1 types are supported.

  - **top**: `integer()`.

    Number of top pixels

  - **bottom**: `integer()`.

    Number of bottom pixels

  - **left**: `integer()`.

    Number of left pixels

  - **right**: `integer()`.

    Number of pixels in each direction from the source image rectangle to extrapolate.
    For example: top=1, bottom=1, left=1, right=1 mean that 1 pixel-wide border needs to be built.

  - **borderType**: `integer()`.

    Border type. See borderInterpolate for details. BORDER_REFLECT101 ,
    BORDER_REPLICATE , BORDER_CONSTANT , BORDER_REFLECT and BORDER_WRAP are supported for now.

  ##### Keyword Arguments
  - **value**: `Evision.scalar()`.

    Border value.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination image with the same type as src. The size is
    Size(src.cols+left+right, src.rows+top+bottom) .

  Python prototype (for reference only):
  ```python3
  copyMakeBorder(src, top, bottom, left, right, borderType[, dst[, value[, stream]]]) -> dst
  ```
  #### Variant 2:
  Forms a border around an image.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source image. CV_8UC1 , CV_8UC4 , CV_32SC1 , and CV_32FC1 types are supported.

  - **top**: `integer()`.

    Number of top pixels

  - **bottom**: `integer()`.

    Number of bottom pixels

  - **left**: `integer()`.

    Number of left pixels

  - **right**: `integer()`.

    Number of pixels in each direction from the source image rectangle to extrapolate.
    For example: top=1, bottom=1, left=1, right=1 mean that 1 pixel-wide border needs to be built.

  - **borderType**: `integer()`.

    Border type. See borderInterpolate for details. BORDER_REFLECT101 ,
    BORDER_REPLICATE , BORDER_CONSTANT , BORDER_REFLECT and BORDER_WRAP are supported for now.

  ##### Keyword Arguments
  - **value**: `Evision.scalar()`.

    Border value.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination image with the same type as src. The size is
    Size(src.cols+left+right, src.rows+top+bottom) .

  Python prototype (for reference only):
  ```python3
  copyMakeBorder(src, top, bottom, left, right, borderType[, dst[, value[, stream]]]) -> dst
  ```

  """
  @spec copyMakeBorder(Evision.Mat.maybe_mat_in(), integer(), integer(), integer(), integer(), integer()) :: Evision.Mat.t() | {:error, String.t()}
  def copyMakeBorder(src, top, bottom, left, right, borderType) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_integer(top) and is_integer(bottom) and is_integer(left) and is_integer(right) and is_integer(borderType)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      top: Evision.Internal.Structurise.from_struct(top),
      bottom: Evision.Internal.Structurise.from_struct(bottom),
      left: Evision.Internal.Structurise.from_struct(left),
      right: Evision.Internal.Structurise.from_struct(right),
      borderType: Evision.Internal.Structurise.from_struct(borderType)
    ]
    :evision_nif.cuda_copyMakeBorder(positional)
    |> to_struct()
  end
  @spec copyMakeBorder(Evision.CUDA.GpuMat.t(), integer(), integer(), integer(), integer(), integer()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def copyMakeBorder(src, top, bottom, left, right, borderType) when is_struct(src, Evision.CUDA.GpuMat) and is_integer(top) and is_integer(bottom) and is_integer(left) and is_integer(right) and is_integer(borderType)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      top: Evision.Internal.Structurise.from_struct(top),
      bottom: Evision.Internal.Structurise.from_struct(bottom),
      left: Evision.Internal.Structurise.from_struct(left),
      right: Evision.Internal.Structurise.from_struct(right),
      borderType: Evision.Internal.Structurise.from_struct(borderType)
    ]
    :evision_nif.cuda_copyMakeBorder(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  countNonZero

  ##### Positional Arguments
  - **src**: `Evision.Mat`

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  countNonZero(src[, dst[, stream]]) -> dst
  ```
  #### Variant 2:
  countNonZero

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  countNonZero(src[, dst[, stream]]) -> dst
  ```

  """
  @spec countNonZero(Evision.Mat.maybe_mat_in(), [{:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def countNonZero(src, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_countNonZero(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec countNonZero(Evision.CUDA.GpuMat.t(), [{:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def countNonZero(src, opts) when is_struct(src, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_countNonZero(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  countNonZero

  ##### Positional Arguments
  - **src**: `Evision.Mat`

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  countNonZero(src[, dst[, stream]]) -> dst
  ```
  #### Variant 2:
  countNonZero

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  countNonZero(src[, dst[, stream]]) -> dst
  ```

  """
  @spec countNonZero(Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def countNonZero(src) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_countNonZero(positional)
    |> to_struct()
  end
  @spec countNonZero(Evision.CUDA.GpuMat.t()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def countNonZero(src) when is_struct(src, Evision.CUDA.GpuMat)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_countNonZero(positional)
    |> to_struct()
  end

  @doc """
  Creates mixture-of-gaussian background subtractor
  ##### Keyword Arguments
  - **history**: `integer()`.

    Length of the history.

  - **nmixtures**: `integer()`.

    Number of Gaussian mixtures.

  - **backgroundRatio**: `double`.

    Background ratio.

  - **noiseSigma**: `double`.

    Noise strength (standard deviation of the brightness or each color channel). 0
    means some automatic value.

  ##### Return
  - **retval**: `Evision.CUDA.BackgroundSubtractorMOG.t()`

  Python prototype (for reference only):
  ```python3
  createBackgroundSubtractorMOG([, history[, nmixtures[, backgroundRatio[, noiseSigma]]]]) -> retval
  ```
  """
  @spec createBackgroundSubtractorMOG([{:backgroundRatio, term()} | {:history, term()} | {:nmixtures, term()} | {:noiseSigma, term()}] | nil) :: Evision.CUDA.BackgroundSubtractorMOG.t() | {:error, String.t()}
  def createBackgroundSubtractorMOG(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:backgroundRatio, :history, :nmixtures, :noiseSigma])
    positional = [
    ]
    :evision_nif.cuda_createBackgroundSubtractorMOG(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Creates mixture-of-gaussian background subtractor
  ##### Keyword Arguments
  - **history**: `integer()`.

    Length of the history.

  - **nmixtures**: `integer()`.

    Number of Gaussian mixtures.

  - **backgroundRatio**: `double`.

    Background ratio.

  - **noiseSigma**: `double`.

    Noise strength (standard deviation of the brightness or each color channel). 0
    means some automatic value.

  ##### Return
  - **retval**: `Evision.CUDA.BackgroundSubtractorMOG.t()`

  Python prototype (for reference only):
  ```python3
  createBackgroundSubtractorMOG([, history[, nmixtures[, backgroundRatio[, noiseSigma]]]]) -> retval
  ```
  """
  @spec createBackgroundSubtractorMOG() :: Evision.CUDA.BackgroundSubtractorMOG.t() | {:error, String.t()}
  def createBackgroundSubtractorMOG() do
    positional = [
    ]
    :evision_nif.cuda_createBackgroundSubtractorMOG(positional)
    |> to_struct()
  end

  @doc """
  Creates MOG2 Background Subtractor
  ##### Keyword Arguments
  - **history**: `integer()`.

    Length of the history.

  - **varThreshold**: `double`.

    Threshold on the squared Mahalanobis distance between the pixel and the model
    to decide whether a pixel is well described by the background model. This parameter does not
    affect the background update.

  - **detectShadows**: `bool`.

    If true, the algorithm will detect shadows and mark them. It decreases the
    speed a bit, so if you do not need this feature, set the parameter to false.

  ##### Return
  - **retval**: `Evision.CUDA.BackgroundSubtractorMOG2.t()`

  Python prototype (for reference only):
  ```python3
  createBackgroundSubtractorMOG2([, history[, varThreshold[, detectShadows]]]) -> retval
  ```
  """
  @spec createBackgroundSubtractorMOG2([{:detectShadows, term()} | {:history, term()} | {:varThreshold, term()}] | nil) :: Evision.CUDA.BackgroundSubtractorMOG2.t() | {:error, String.t()}
  def createBackgroundSubtractorMOG2(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:detectShadows, :history, :varThreshold])
    positional = [
    ]
    :evision_nif.cuda_createBackgroundSubtractorMOG2(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Creates MOG2 Background Subtractor
  ##### Keyword Arguments
  - **history**: `integer()`.

    Length of the history.

  - **varThreshold**: `double`.

    Threshold on the squared Mahalanobis distance between the pixel and the model
    to decide whether a pixel is well described by the background model. This parameter does not
    affect the background update.

  - **detectShadows**: `bool`.

    If true, the algorithm will detect shadows and mark them. It decreases the
    speed a bit, so if you do not need this feature, set the parameter to false.

  ##### Return
  - **retval**: `Evision.CUDA.BackgroundSubtractorMOG2.t()`

  Python prototype (for reference only):
  ```python3
  createBackgroundSubtractorMOG2([, history[, varThreshold[, detectShadows]]]) -> retval
  ```
  """
  @spec createBackgroundSubtractorMOG2() :: Evision.CUDA.BackgroundSubtractorMOG2.t() | {:error, String.t()}
  def createBackgroundSubtractorMOG2() do
    positional = [
    ]
    :evision_nif.cuda_createBackgroundSubtractorMOG2(positional)
    |> to_struct()
  end

  @doc """
  Creates a normalized 2D box filter.

  ##### Positional Arguments
  - **srcType**: `integer()`.

    Input image type. Only CV_8UC1, CV_8UC4 and CV_32FC1 are supported for now.

  - **dstType**: `integer()`.

    Output image type. Only the same type as src is supported for now.

  - **ksize**: `Size`.

    Kernel size.

  ##### Keyword Arguments
  - **anchor**: `Point`.

    Anchor point. The default value Point(-1, -1) means that the anchor is at the kernel
    center.

  - **borderMode**: `integer()`.

    Pixel extrapolation method. For details, see borderInterpolate .

  - **borderVal**: `Evision.scalar()`.

    Default border value.

  ##### Return
  - **retval**: `Filter`

  @sa boxFilter

  Python prototype (for reference only):
  ```python3
  createBoxFilter(srcType, dstType, ksize[, anchor[, borderMode[, borderVal]]]) -> retval
  ```
  """
  @spec createBoxFilter(integer(), integer(), {number(), number()}, [{:anchor, term()} | {:borderMode, term()} | {:borderVal, term()}] | nil) :: Evision.CUDA.Filter.t() | {:error, String.t()}
  def createBoxFilter(srcType, dstType, ksize, opts) when is_integer(srcType) and is_integer(dstType) and is_tuple(ksize) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:anchor, :borderMode, :borderVal])
    positional = [
      srcType: Evision.Internal.Structurise.from_struct(srcType),
      dstType: Evision.Internal.Structurise.from_struct(dstType),
      ksize: Evision.Internal.Structurise.from_struct(ksize)
    ]
    :evision_nif.cuda_createBoxFilter(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Creates a normalized 2D box filter.

  ##### Positional Arguments
  - **srcType**: `integer()`.

    Input image type. Only CV_8UC1, CV_8UC4 and CV_32FC1 are supported for now.

  - **dstType**: `integer()`.

    Output image type. Only the same type as src is supported for now.

  - **ksize**: `Size`.

    Kernel size.

  ##### Keyword Arguments
  - **anchor**: `Point`.

    Anchor point. The default value Point(-1, -1) means that the anchor is at the kernel
    center.

  - **borderMode**: `integer()`.

    Pixel extrapolation method. For details, see borderInterpolate .

  - **borderVal**: `Evision.scalar()`.

    Default border value.

  ##### Return
  - **retval**: `Filter`

  @sa boxFilter

  Python prototype (for reference only):
  ```python3
  createBoxFilter(srcType, dstType, ksize[, anchor[, borderMode[, borderVal]]]) -> retval
  ```
  """
  @spec createBoxFilter(integer(), integer(), {number(), number()}) :: Evision.CUDA.Filter.t() | {:error, String.t()}
  def createBoxFilter(srcType, dstType, ksize) when is_integer(srcType) and is_integer(dstType) and is_tuple(ksize)
  do
    positional = [
      srcType: Evision.Internal.Structurise.from_struct(srcType),
      dstType: Evision.Internal.Structurise.from_struct(dstType),
      ksize: Evision.Internal.Structurise.from_struct(ksize)
    ]
    :evision_nif.cuda_createBoxFilter(positional)
    |> to_struct()
  end

  @doc """
  Creates the maximum filter.

  ##### Positional Arguments
  - **srcType**: `integer()`.

    Input/output image type. Only CV_8UC1 and CV_8UC4 are supported.

  - **ksize**: `Size`.

    Kernel size.

  ##### Keyword Arguments
  - **anchor**: `Point`.

    Anchor point. The default value (-1) means that the anchor is at the kernel center.

  - **borderMode**: `integer()`.

    Pixel extrapolation method. For details, see borderInterpolate .

  - **borderVal**: `Evision.scalar()`.

    Default border value.

  ##### Return
  - **retval**: `Filter`

  Python prototype (for reference only):
  ```python3
  createBoxMaxFilter(srcType, ksize[, anchor[, borderMode[, borderVal]]]) -> retval
  ```
  """
  @spec createBoxMaxFilter(integer(), {number(), number()}, [{:anchor, term()} | {:borderMode, term()} | {:borderVal, term()}] | nil) :: Evision.CUDA.Filter.t() | {:error, String.t()}
  def createBoxMaxFilter(srcType, ksize, opts) when is_integer(srcType) and is_tuple(ksize) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:anchor, :borderMode, :borderVal])
    positional = [
      srcType: Evision.Internal.Structurise.from_struct(srcType),
      ksize: Evision.Internal.Structurise.from_struct(ksize)
    ]
    :evision_nif.cuda_createBoxMaxFilter(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Creates the maximum filter.

  ##### Positional Arguments
  - **srcType**: `integer()`.

    Input/output image type. Only CV_8UC1 and CV_8UC4 are supported.

  - **ksize**: `Size`.

    Kernel size.

  ##### Keyword Arguments
  - **anchor**: `Point`.

    Anchor point. The default value (-1) means that the anchor is at the kernel center.

  - **borderMode**: `integer()`.

    Pixel extrapolation method. For details, see borderInterpolate .

  - **borderVal**: `Evision.scalar()`.

    Default border value.

  ##### Return
  - **retval**: `Filter`

  Python prototype (for reference only):
  ```python3
  createBoxMaxFilter(srcType, ksize[, anchor[, borderMode[, borderVal]]]) -> retval
  ```
  """
  @spec createBoxMaxFilter(integer(), {number(), number()}) :: Evision.CUDA.Filter.t() | {:error, String.t()}
  def createBoxMaxFilter(srcType, ksize) when is_integer(srcType) and is_tuple(ksize)
  do
    positional = [
      srcType: Evision.Internal.Structurise.from_struct(srcType),
      ksize: Evision.Internal.Structurise.from_struct(ksize)
    ]
    :evision_nif.cuda_createBoxMaxFilter(positional)
    |> to_struct()
  end

  @doc """
  Creates the minimum filter.

  ##### Positional Arguments
  - **srcType**: `integer()`.

    Input/output image type. Only CV_8UC1 and CV_8UC4 are supported.

  - **ksize**: `Size`.

    Kernel size.

  ##### Keyword Arguments
  - **anchor**: `Point`.

    Anchor point. The default value (-1) means that the anchor is at the kernel center.

  - **borderMode**: `integer()`.

    Pixel extrapolation method. For details, see borderInterpolate .

  - **borderVal**: `Evision.scalar()`.

    Default border value.

  ##### Return
  - **retval**: `Filter`

  Python prototype (for reference only):
  ```python3
  createBoxMinFilter(srcType, ksize[, anchor[, borderMode[, borderVal]]]) -> retval
  ```
  """
  @spec createBoxMinFilter(integer(), {number(), number()}, [{:anchor, term()} | {:borderMode, term()} | {:borderVal, term()}] | nil) :: Evision.CUDA.Filter.t() | {:error, String.t()}
  def createBoxMinFilter(srcType, ksize, opts) when is_integer(srcType) and is_tuple(ksize) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:anchor, :borderMode, :borderVal])
    positional = [
      srcType: Evision.Internal.Structurise.from_struct(srcType),
      ksize: Evision.Internal.Structurise.from_struct(ksize)
    ]
    :evision_nif.cuda_createBoxMinFilter(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Creates the minimum filter.

  ##### Positional Arguments
  - **srcType**: `integer()`.

    Input/output image type. Only CV_8UC1 and CV_8UC4 are supported.

  - **ksize**: `Size`.

    Kernel size.

  ##### Keyword Arguments
  - **anchor**: `Point`.

    Anchor point. The default value (-1) means that the anchor is at the kernel center.

  - **borderMode**: `integer()`.

    Pixel extrapolation method. For details, see borderInterpolate .

  - **borderVal**: `Evision.scalar()`.

    Default border value.

  ##### Return
  - **retval**: `Filter`

  Python prototype (for reference only):
  ```python3
  createBoxMinFilter(srcType, ksize[, anchor[, borderMode[, borderVal]]]) -> retval
  ```
  """
  @spec createBoxMinFilter(integer(), {number(), number()}) :: Evision.CUDA.Filter.t() | {:error, String.t()}
  def createBoxMinFilter(srcType, ksize) when is_integer(srcType) and is_tuple(ksize)
  do
    positional = [
      srcType: Evision.Internal.Structurise.from_struct(srcType),
      ksize: Evision.Internal.Structurise.from_struct(ksize)
    ]
    :evision_nif.cuda_createBoxMinFilter(positional)
    |> to_struct()
  end

  @doc """
  Creates implementation for cuda::CLAHE .
  ##### Keyword Arguments
  - **clipLimit**: `double`.

    Threshold for contrast limiting.

  - **tileGridSize**: `Size`.

    Size of grid for histogram equalization. Input image will be divided into
    equally sized rectangular tiles. tileGridSize defines the number of tiles in row and column.

  ##### Return
  - **retval**: `Evision.CUDA.CLAHE.t()`

  Python prototype (for reference only):
  ```python3
  createCLAHE([, clipLimit[, tileGridSize]]) -> retval
  ```
  """
  @spec createCLAHE([{:clipLimit, term()} | {:tileGridSize, term()}] | nil) :: Evision.CUDA.CLAHE.t() | {:error, String.t()}
  def createCLAHE(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:clipLimit, :tileGridSize])
    positional = [
    ]
    :evision_nif.cuda_createCLAHE(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Creates implementation for cuda::CLAHE .
  ##### Keyword Arguments
  - **clipLimit**: `double`.

    Threshold for contrast limiting.

  - **tileGridSize**: `Size`.

    Size of grid for histogram equalization. Input image will be divided into
    equally sized rectangular tiles. tileGridSize defines the number of tiles in row and column.

  ##### Return
  - **retval**: `Evision.CUDA.CLAHE.t()`

  Python prototype (for reference only):
  ```python3
  createCLAHE([, clipLimit[, tileGridSize]]) -> retval
  ```
  """
  @spec createCLAHE() :: Evision.CUDA.CLAHE.t() | {:error, String.t()}
  def createCLAHE() do
    positional = [
    ]
    :evision_nif.cuda_createCLAHE(positional)
    |> to_struct()
  end

  @doc """
  Creates implementation for cuda::CannyEdgeDetector .

  ##### Positional Arguments
  - **low_thresh**: `double`.

    First threshold for the hysteresis procedure.

  - **high_thresh**: `double`.

    Second threshold for the hysteresis procedure.

  ##### Keyword Arguments
  - **apperture_size**: `integer()`.

    Aperture size for the Sobel operator.

  - **l2gradient**: `bool`.

    Flag indicating whether a more accurate \\f$L_2\\f$ norm
    \\f$=\\sqrt{(dI/dx)^2 + (dI/dy)^2}\\f$ should be used to compute the image gradient magnitude (
    L2gradient=true ), or a faster default \\f$L_1\\f$ norm \\f$=|dI/dx|+|dI/dy|\\f$ is enough ( L2gradient=false
    ).

  ##### Return
  - **retval**: `CannyEdgeDetector`

  Python prototype (for reference only):
  ```python3
  createCannyEdgeDetector(low_thresh, high_thresh[, apperture_size[, L2gradient]]) -> retval
  ```
  """
  @spec createCannyEdgeDetector(number(), number(), [{:apperture_size, term()} | {:l2gradient, term()}] | nil) :: Evision.CUDA.CannyEdgeDetector.t() | {:error, String.t()}
  def createCannyEdgeDetector(low_thresh, high_thresh, opts) when is_number(low_thresh) and is_number(high_thresh) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:apperture_size, :l2gradient])
    positional = [
      low_thresh: Evision.Internal.Structurise.from_struct(low_thresh),
      high_thresh: Evision.Internal.Structurise.from_struct(high_thresh)
    ]
    :evision_nif.cuda_createCannyEdgeDetector(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Creates implementation for cuda::CannyEdgeDetector .

  ##### Positional Arguments
  - **low_thresh**: `double`.

    First threshold for the hysteresis procedure.

  - **high_thresh**: `double`.

    Second threshold for the hysteresis procedure.

  ##### Keyword Arguments
  - **apperture_size**: `integer()`.

    Aperture size for the Sobel operator.

  - **l2gradient**: `bool`.

    Flag indicating whether a more accurate \\f$L_2\\f$ norm
    \\f$=\\sqrt{(dI/dx)^2 + (dI/dy)^2}\\f$ should be used to compute the image gradient magnitude (
    L2gradient=true ), or a faster default \\f$L_1\\f$ norm \\f$=|dI/dx|+|dI/dy|\\f$ is enough ( L2gradient=false
    ).

  ##### Return
  - **retval**: `CannyEdgeDetector`

  Python prototype (for reference only):
  ```python3
  createCannyEdgeDetector(low_thresh, high_thresh[, apperture_size[, L2gradient]]) -> retval
  ```
  """
  @spec createCannyEdgeDetector(number(), number()) :: Evision.CUDA.CannyEdgeDetector.t() | {:error, String.t()}
  def createCannyEdgeDetector(low_thresh, high_thresh) when is_number(low_thresh) and is_number(high_thresh)
  do
    positional = [
      low_thresh: Evision.Internal.Structurise.from_struct(low_thresh),
      high_thresh: Evision.Internal.Structurise.from_struct(high_thresh)
    ]
    :evision_nif.cuda_createCannyEdgeDetector(positional)
    |> to_struct()
  end

  @doc """
  Creates a vertical 1D box filter.

  ##### Positional Arguments
  - **srcType**: `integer()`.

    Input image type. Only CV_8UC1 type is supported for now.

  - **dstType**: `integer()`.

    Output image type. Only CV_32FC1 type is supported for now.

  - **ksize**: `integer()`.

    Kernel size.

  ##### Keyword Arguments
  - **anchor**: `integer()`.

    Anchor point. The default value (-1) means that the anchor is at the kernel center.

  - **borderMode**: `integer()`.

    Pixel extrapolation method. For details, see borderInterpolate .

  - **borderVal**: `Evision.scalar()`.

    Default border value.

  ##### Return
  - **retval**: `Filter`

  Python prototype (for reference only):
  ```python3
  createColumnSumFilter(srcType, dstType, ksize[, anchor[, borderMode[, borderVal]]]) -> retval
  ```
  """
  @spec createColumnSumFilter(integer(), integer(), integer(), [{:anchor, term()} | {:borderMode, term()} | {:borderVal, term()}] | nil) :: Evision.CUDA.Filter.t() | {:error, String.t()}
  def createColumnSumFilter(srcType, dstType, ksize, opts) when is_integer(srcType) and is_integer(dstType) and is_integer(ksize) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:anchor, :borderMode, :borderVal])
    positional = [
      srcType: Evision.Internal.Structurise.from_struct(srcType),
      dstType: Evision.Internal.Structurise.from_struct(dstType),
      ksize: Evision.Internal.Structurise.from_struct(ksize)
    ]
    :evision_nif.cuda_createColumnSumFilter(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Creates a vertical 1D box filter.

  ##### Positional Arguments
  - **srcType**: `integer()`.

    Input image type. Only CV_8UC1 type is supported for now.

  - **dstType**: `integer()`.

    Output image type. Only CV_32FC1 type is supported for now.

  - **ksize**: `integer()`.

    Kernel size.

  ##### Keyword Arguments
  - **anchor**: `integer()`.

    Anchor point. The default value (-1) means that the anchor is at the kernel center.

  - **borderMode**: `integer()`.

    Pixel extrapolation method. For details, see borderInterpolate .

  - **borderVal**: `Evision.scalar()`.

    Default border value.

  ##### Return
  - **retval**: `Filter`

  Python prototype (for reference only):
  ```python3
  createColumnSumFilter(srcType, dstType, ksize[, anchor[, borderMode[, borderVal]]]) -> retval
  ```
  """
  @spec createColumnSumFilter(integer(), integer(), integer()) :: Evision.CUDA.Filter.t() | {:error, String.t()}
  def createColumnSumFilter(srcType, dstType, ksize) when is_integer(srcType) and is_integer(dstType) and is_integer(ksize)
  do
    positional = [
      srcType: Evision.Internal.Structurise.from_struct(srcType),
      dstType: Evision.Internal.Structurise.from_struct(dstType),
      ksize: Evision.Internal.Structurise.from_struct(ksize)
    ]
    :evision_nif.cuda_createColumnSumFilter(positional)
    |> to_struct()
  end

  @doc """
  Creates a continuous matrix.

  ##### Positional Arguments
  - **rows**: `integer()`.

    Row count.

  - **cols**: `integer()`.

    Column count.

  - **type**: `integer()`.

    Type of the matrix.

  ##### Return
  - **arr**: `Evision.Mat.t()`.

    Destination matrix. This parameter changes only if it has a proper type and area (
    \\f$\\texttt{rows} \\times \\texttt{cols}\\f$ ).

  Matrix is called continuous if its elements are stored continuously, that is, without gaps at the
  end of each row.

  Python prototype (for reference only):
  ```python3
  createContinuous(rows, cols, type[, arr]) -> arr
  ```
  """
  @spec createContinuous(integer(), integer(), integer(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def createContinuous(rows, cols, type, opts) when is_integer(rows) and is_integer(cols) and is_integer(type) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      rows: Evision.Internal.Structurise.from_struct(rows),
      cols: Evision.Internal.Structurise.from_struct(cols),
      type: Evision.Internal.Structurise.from_struct(type)
    ]
    :evision_nif.cuda_createContinuous(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Creates a continuous matrix.

  ##### Positional Arguments
  - **rows**: `integer()`.

    Row count.

  - **cols**: `integer()`.

    Column count.

  - **type**: `integer()`.

    Type of the matrix.

  ##### Return
  - **arr**: `Evision.Mat.t()`.

    Destination matrix. This parameter changes only if it has a proper type and area (
    \\f$\\texttt{rows} \\times \\texttt{cols}\\f$ ).

  Matrix is called continuous if its elements are stored continuously, that is, without gaps at the
  end of each row.

  Python prototype (for reference only):
  ```python3
  createContinuous(rows, cols, type[, arr]) -> arr
  ```
  """
  @spec createContinuous(integer(), integer(), integer()) :: Evision.Mat.t() | {:error, String.t()}
  def createContinuous(rows, cols, type) when is_integer(rows) and is_integer(cols) and is_integer(type)
  do
    positional = [
      rows: Evision.Internal.Structurise.from_struct(rows),
      cols: Evision.Internal.Structurise.from_struct(cols),
      type: Evision.Internal.Structurise.from_struct(type)
    ]
    :evision_nif.cuda_createContinuous(positional)
    |> to_struct()
  end

  @doc """
  Creates implementation for cuda::Convolution .
  ##### Keyword Arguments
  - **user_block_size**: `Size`.

    Block size. If you leave default value Size(0,0) then automatic
    estimation of block size will be used (which is optimized for speed). By varying user_block_size
    you can reduce memory requirements at the cost of speed.

  ##### Return
  - **retval**: `Convolution`

  Python prototype (for reference only):
  ```python3
  createConvolution([, user_block_size]) -> retval
  ```
  """
  @spec createConvolution([{:user_block_size, term()}] | nil) :: Evision.CUDA.Convolution.t() | {:error, String.t()}
  def createConvolution(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:user_block_size])
    positional = [
    ]
    :evision_nif.cuda_createConvolution(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Creates implementation for cuda::Convolution .
  ##### Keyword Arguments
  - **user_block_size**: `Size`.

    Block size. If you leave default value Size(0,0) then automatic
    estimation of block size will be used (which is optimized for speed). By varying user_block_size
    you can reduce memory requirements at the cost of speed.

  ##### Return
  - **retval**: `Convolution`

  Python prototype (for reference only):
  ```python3
  createConvolution([, user_block_size]) -> retval
  ```
  """
  @spec createConvolution() :: Evision.CUDA.Convolution.t() | {:error, String.t()}
  def createConvolution() do
    positional = [
    ]
    :evision_nif.cuda_createConvolution(positional)
    |> to_struct()
  end

  @doc """
  Creates implementation for cuda::DFT.

  ##### Positional Arguments
  - **dft_size**: `Size`.

    The image size.

  - **flags**: `integer()`.

    Optional flags:
    - **DFT_ROWS** transforms each individual row of the source matrix.
    - **DFT_SCALE** scales the result: divide it by the number of elements in the transform
      (obtained from dft_size ).
    - **DFT_INVERSE** inverts DFT. Use for complex-complex cases (real-complex and complex-real
      cases are always forward and inverse, respectively).
    - **DFT_COMPLEX_INPUT** Specifies that inputs will be complex with 2 channels.
    - **DFT_REAL_OUTPUT** specifies the output as real. The source matrix is the result of
      real-complex transform, so the destination matrix must be real.

  ##### Return
  - **retval**: `DFT`

  Python prototype (for reference only):
  ```python3
  createDFT(dft_size, flags) -> retval
  ```
  """
  @spec createDFT({number(), number()}, integer()) :: Evision.CUDA.DFT.t() | {:error, String.t()}
  def createDFT(dft_size, flags) when is_tuple(dft_size) and is_integer(flags)
  do
    positional = [
      dft_size: Evision.Internal.Structurise.from_struct(dft_size),
      flags: Evision.Internal.Structurise.from_struct(flags)
    ]
    :evision_nif.cuda_createDFT(positional)
    |> to_struct()
  end

  @doc """
  Creates a generalized Deriv operator.

  ##### Positional Arguments
  - **srcType**: `integer()`.

    Source image type.

  - **dstType**: `integer()`.

    Destination array type.

  - **dx**: `integer()`.

    Derivative order in respect of x.

  - **dy**: `integer()`.

    Derivative order in respect of y.

  - **ksize**: `integer()`.

    Aperture size. See getDerivKernels for details.

  ##### Keyword Arguments
  - **normalize**: `bool`.

    Flag indicating whether to normalize (scale down) the filter coefficients or not.
    See getDerivKernels for details.

  - **scale**: `double`.

    Optional scale factor for the computed derivative values. By default, no scaling is
    applied. For details, see getDerivKernels .

  - **rowBorderMode**: `integer()`.

    Pixel extrapolation method in the vertical direction. For details, see
    borderInterpolate.

  - **columnBorderMode**: `integer()`.

    Pixel extrapolation method in the horizontal direction.

  ##### Return
  - **retval**: `Filter`

  Python prototype (for reference only):
  ```python3
  createDerivFilter(srcType, dstType, dx, dy, ksize[, normalize[, scale[, rowBorderMode[, columnBorderMode]]]]) -> retval
  ```
  """
  @spec createDerivFilter(integer(), integer(), integer(), integer(), integer(), [{:columnBorderMode, term()} | {:normalize, term()} | {:rowBorderMode, term()} | {:scale, term()}] | nil) :: Evision.CUDA.Filter.t() | {:error, String.t()}
  def createDerivFilter(srcType, dstType, dx, dy, ksize, opts) when is_integer(srcType) and is_integer(dstType) and is_integer(dx) and is_integer(dy) and is_integer(ksize) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:columnBorderMode, :normalize, :rowBorderMode, :scale])
    positional = [
      srcType: Evision.Internal.Structurise.from_struct(srcType),
      dstType: Evision.Internal.Structurise.from_struct(dstType),
      dx: Evision.Internal.Structurise.from_struct(dx),
      dy: Evision.Internal.Structurise.from_struct(dy),
      ksize: Evision.Internal.Structurise.from_struct(ksize)
    ]
    :evision_nif.cuda_createDerivFilter(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Creates a generalized Deriv operator.

  ##### Positional Arguments
  - **srcType**: `integer()`.

    Source image type.

  - **dstType**: `integer()`.

    Destination array type.

  - **dx**: `integer()`.

    Derivative order in respect of x.

  - **dy**: `integer()`.

    Derivative order in respect of y.

  - **ksize**: `integer()`.

    Aperture size. See getDerivKernels for details.

  ##### Keyword Arguments
  - **normalize**: `bool`.

    Flag indicating whether to normalize (scale down) the filter coefficients or not.
    See getDerivKernels for details.

  - **scale**: `double`.

    Optional scale factor for the computed derivative values. By default, no scaling is
    applied. For details, see getDerivKernels .

  - **rowBorderMode**: `integer()`.

    Pixel extrapolation method in the vertical direction. For details, see
    borderInterpolate.

  - **columnBorderMode**: `integer()`.

    Pixel extrapolation method in the horizontal direction.

  ##### Return
  - **retval**: `Filter`

  Python prototype (for reference only):
  ```python3
  createDerivFilter(srcType, dstType, dx, dy, ksize[, normalize[, scale[, rowBorderMode[, columnBorderMode]]]]) -> retval
  ```
  """
  @spec createDerivFilter(integer(), integer(), integer(), integer(), integer()) :: Evision.CUDA.Filter.t() | {:error, String.t()}
  def createDerivFilter(srcType, dstType, dx, dy, ksize) when is_integer(srcType) and is_integer(dstType) and is_integer(dx) and is_integer(dy) and is_integer(ksize)
  do
    positional = [
      srcType: Evision.Internal.Structurise.from_struct(srcType),
      dstType: Evision.Internal.Structurise.from_struct(dstType),
      dx: Evision.Internal.Structurise.from_struct(dx),
      dy: Evision.Internal.Structurise.from_struct(dy),
      ksize: Evision.Internal.Structurise.from_struct(ksize)
    ]
    :evision_nif.cuda_createDerivFilter(positional)
    |> to_struct()
  end

  @doc """
  Creates DisparityBilateralFilter object.
  ##### Keyword Arguments
  - **ndisp**: `integer()`.

    Number of disparities.

  - **radius**: `integer()`.

    Filter radius.

  - **iters**: `integer()`.

    Number of iterations.

  ##### Return
  - **retval**: `Evision.CUDA.DisparityBilateralFilter.t()`

  Python prototype (for reference only):
  ```python3
  createDisparityBilateralFilter([, ndisp[, radius[, iters]]]) -> retval
  ```
  """
  @spec createDisparityBilateralFilter([{:iters, term()} | {:ndisp, term()} | {:radius, term()}] | nil) :: Evision.CUDA.DisparityBilateralFilter.t() | {:error, String.t()}
  def createDisparityBilateralFilter(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:iters, :ndisp, :radius])
    positional = [
    ]
    :evision_nif.cuda_createDisparityBilateralFilter(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Creates DisparityBilateralFilter object.
  ##### Keyword Arguments
  - **ndisp**: `integer()`.

    Number of disparities.

  - **radius**: `integer()`.

    Filter radius.

  - **iters**: `integer()`.

    Number of iterations.

  ##### Return
  - **retval**: `Evision.CUDA.DisparityBilateralFilter.t()`

  Python prototype (for reference only):
  ```python3
  createDisparityBilateralFilter([, ndisp[, radius[, iters]]]) -> retval
  ```
  """
  @spec createDisparityBilateralFilter() :: Evision.CUDA.DisparityBilateralFilter.t() | {:error, String.t()}
  def createDisparityBilateralFilter() do
    positional = [
    ]
    :evision_nif.cuda_createDisparityBilateralFilter(positional)
    |> to_struct()
  end

  @doc """
  Creates a Gaussian filter.

  ##### Positional Arguments
  - **srcType**: `integer()`.

    Source image type.

  - **dstType**: `integer()`.

    Destination array type.

  - **ksize**: `Size`.

    Aperture size. See getGaussianKernel for details.

  - **sigma1**: `double`.

    Gaussian sigma in the horizontal direction. See getGaussianKernel for details.

  ##### Keyword Arguments
  - **sigma2**: `double`.

    Gaussian sigma in the vertical direction. If 0, then
    \\f$\\texttt{sigma2}\\leftarrow\\texttt{sigma1}\\f$ .

  - **rowBorderMode**: `integer()`.

    Pixel extrapolation method in the vertical direction. For details, see
    borderInterpolate.

  - **columnBorderMode**: `integer()`.

    Pixel extrapolation method in the horizontal direction.

  ##### Return
  - **retval**: `Filter`

  @sa GaussianBlur

  Python prototype (for reference only):
  ```python3
  createGaussianFilter(srcType, dstType, ksize, sigma1[, sigma2[, rowBorderMode[, columnBorderMode]]]) -> retval
  ```
  """
  @spec createGaussianFilter(integer(), integer(), {number(), number()}, number(), [{:columnBorderMode, term()} | {:rowBorderMode, term()} | {:sigma2, term()}] | nil) :: Evision.CUDA.Filter.t() | {:error, String.t()}
  def createGaussianFilter(srcType, dstType, ksize, sigma1, opts) when is_integer(srcType) and is_integer(dstType) and is_tuple(ksize) and is_number(sigma1) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:columnBorderMode, :rowBorderMode, :sigma2])
    positional = [
      srcType: Evision.Internal.Structurise.from_struct(srcType),
      dstType: Evision.Internal.Structurise.from_struct(dstType),
      ksize: Evision.Internal.Structurise.from_struct(ksize),
      sigma1: Evision.Internal.Structurise.from_struct(sigma1)
    ]
    :evision_nif.cuda_createGaussianFilter(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Creates a Gaussian filter.

  ##### Positional Arguments
  - **srcType**: `integer()`.

    Source image type.

  - **dstType**: `integer()`.

    Destination array type.

  - **ksize**: `Size`.

    Aperture size. See getGaussianKernel for details.

  - **sigma1**: `double`.

    Gaussian sigma in the horizontal direction. See getGaussianKernel for details.

  ##### Keyword Arguments
  - **sigma2**: `double`.

    Gaussian sigma in the vertical direction. If 0, then
    \\f$\\texttt{sigma2}\\leftarrow\\texttt{sigma1}\\f$ .

  - **rowBorderMode**: `integer()`.

    Pixel extrapolation method in the vertical direction. For details, see
    borderInterpolate.

  - **columnBorderMode**: `integer()`.

    Pixel extrapolation method in the horizontal direction.

  ##### Return
  - **retval**: `Filter`

  @sa GaussianBlur

  Python prototype (for reference only):
  ```python3
  createGaussianFilter(srcType, dstType, ksize, sigma1[, sigma2[, rowBorderMode[, columnBorderMode]]]) -> retval
  ```
  """
  @spec createGaussianFilter(integer(), integer(), {number(), number()}, number()) :: Evision.CUDA.Filter.t() | {:error, String.t()}
  def createGaussianFilter(srcType, dstType, ksize, sigma1) when is_integer(srcType) and is_integer(dstType) and is_tuple(ksize) and is_number(sigma1)
  do
    positional = [
      srcType: Evision.Internal.Structurise.from_struct(srcType),
      dstType: Evision.Internal.Structurise.from_struct(dstType),
      ksize: Evision.Internal.Structurise.from_struct(ksize),
      sigma1: Evision.Internal.Structurise.from_struct(sigma1)
    ]
    :evision_nif.cuda_createGaussianFilter(positional)
    |> to_struct()
  end

  @doc """
  Creates implementation for generalized hough transform from @cite Ballard1981 .
  ##### Return
  - **retval**: `Evision.GeneralizedHoughBallard.t()`

  Python prototype (for reference only):
  ```python3
  createGeneralizedHoughBallard() -> retval
  ```
  """
  @spec createGeneralizedHoughBallard() :: Evision.GeneralizedHoughBallard.t() | {:error, String.t()}
  def createGeneralizedHoughBallard() do
    positional = [
    ]
    :evision_nif.cuda_createGeneralizedHoughBallard(positional)
    |> to_struct()
  end

  @doc """
  Creates implementation for generalized hough transform from @cite Guil1999 .
  ##### Return
  - **retval**: `Evision.GeneralizedHoughGuil.t()`

  Python prototype (for reference only):
  ```python3
  createGeneralizedHoughGuil() -> retval
  ```
  """
  @spec createGeneralizedHoughGuil() :: Evision.GeneralizedHoughGuil.t() | {:error, String.t()}
  def createGeneralizedHoughGuil() do
    positional = [
    ]
    :evision_nif.cuda_createGeneralizedHoughGuil(positional)
    |> to_struct()
  end

  @doc """
  Creates implementation for cuda::CornersDetector .

  ##### Positional Arguments
  - **srcType**: `integer()`.

    Input source type. Only CV_8UC1 and CV_32FC1 are supported for now.

  ##### Keyword Arguments
  - **maxCorners**: `integer()`.

    Maximum number of corners to return. If there are more corners than are found,
    the strongest of them is returned.

  - **qualityLevel**: `double`.

    Parameter characterizing the minimal accepted quality of image corners. The
    parameter value is multiplied by the best corner quality measure, which is the minimal eigenvalue
    (see cornerMinEigenVal ) or the Harris function response (see cornerHarris ). The corners with the
    quality measure less than the product are rejected. For example, if the best corner has the
    quality measure = 1500, and the qualityLevel=0.01 , then all the corners with the quality measure
    less than 15 are rejected.

  - **minDistance**: `double`.

    Minimum possible Euclidean distance between the returned corners.

  - **blockSize**: `integer()`.

    Size of an average block for computing a derivative covariation matrix over each
    pixel neighborhood. See cornerEigenValsAndVecs .

  - **useHarrisDetector**: `bool`.

    Parameter indicating whether to use a Harris detector (see cornerHarris)
    or cornerMinEigenVal.

  - **harrisK**: `double`.

    Free parameter of the Harris detector.

  ##### Return
  - **retval**: `CornersDetector`

  Python prototype (for reference only):
  ```python3
  createGoodFeaturesToTrackDetector(srcType[, maxCorners[, qualityLevel[, minDistance[, blockSize[, useHarrisDetector[, harrisK]]]]]]) -> retval
  ```
  """
  @spec createGoodFeaturesToTrackDetector(integer(), [{:blockSize, term()} | {:harrisK, term()} | {:maxCorners, term()} | {:minDistance, term()} | {:qualityLevel, term()} | {:useHarrisDetector, term()}] | nil) :: Evision.CUDA.CornersDetector.t() | {:error, String.t()}
  def createGoodFeaturesToTrackDetector(srcType, opts) when is_integer(srcType) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:blockSize, :harrisK, :maxCorners, :minDistance, :qualityLevel, :useHarrisDetector])
    positional = [
      srcType: Evision.Internal.Structurise.from_struct(srcType)
    ]
    :evision_nif.cuda_createGoodFeaturesToTrackDetector(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Creates implementation for cuda::CornersDetector .

  ##### Positional Arguments
  - **srcType**: `integer()`.

    Input source type. Only CV_8UC1 and CV_32FC1 are supported for now.

  ##### Keyword Arguments
  - **maxCorners**: `integer()`.

    Maximum number of corners to return. If there are more corners than are found,
    the strongest of them is returned.

  - **qualityLevel**: `double`.

    Parameter characterizing the minimal accepted quality of image corners. The
    parameter value is multiplied by the best corner quality measure, which is the minimal eigenvalue
    (see cornerMinEigenVal ) or the Harris function response (see cornerHarris ). The corners with the
    quality measure less than the product are rejected. For example, if the best corner has the
    quality measure = 1500, and the qualityLevel=0.01 , then all the corners with the quality measure
    less than 15 are rejected.

  - **minDistance**: `double`.

    Minimum possible Euclidean distance between the returned corners.

  - **blockSize**: `integer()`.

    Size of an average block for computing a derivative covariation matrix over each
    pixel neighborhood. See cornerEigenValsAndVecs .

  - **useHarrisDetector**: `bool`.

    Parameter indicating whether to use a Harris detector (see cornerHarris)
    or cornerMinEigenVal.

  - **harrisK**: `double`.

    Free parameter of the Harris detector.

  ##### Return
  - **retval**: `CornersDetector`

  Python prototype (for reference only):
  ```python3
  createGoodFeaturesToTrackDetector(srcType[, maxCorners[, qualityLevel[, minDistance[, blockSize[, useHarrisDetector[, harrisK]]]]]]) -> retval
  ```
  """
  @spec createGoodFeaturesToTrackDetector(integer()) :: Evision.CUDA.CornersDetector.t() | {:error, String.t()}
  def createGoodFeaturesToTrackDetector(srcType) when is_integer(srcType)
  do
    positional = [
      srcType: Evision.Internal.Structurise.from_struct(srcType)
    ]
    :evision_nif.cuda_createGoodFeaturesToTrackDetector(positional)
    |> to_struct()
  end

  @doc """
  Bindings overload to create a GpuMat from existing GPU memory.

  ##### Positional Arguments
  - **rows**: `integer()`.

    Row count.

  - **cols**: `integer()`.

    Column count.

  - **type**: `integer()`.

    Type of the matrix.

  - **cudaMemoryAddress**: `size_t`.

    Address of the allocated GPU memory on the device. This does not allocate matrix data. Instead, it just initializes the matrix header that points to the specified \\a cudaMemoryAddress, which means that no data is copied. This operation is very efficient and can be used to process external data using OpenCV functions. The external data is not automatically deallocated, so you should take care of it.

  ##### Keyword Arguments
  - **step**: `size_t`.

    Number of bytes each matrix row occupies. The value should include the padding bytes at the end of each row, if any. If the parameter is missing (set to Mat::AUTO_STEP ), no padding is assumed and the actual step is calculated as cols*elemSize(). See GpuMat::elemSize.

  ##### Return
  - **retval**: `Evision.CUDA.GpuMat.t()`

  **Note**: Overload for generation of bindings only, not exported or intended for use internally from C++.

  Python prototype (for reference only):
  ```python3
  createGpuMatFromCudaMemory(rows, cols, type, cudaMemoryAddress[, step]) -> retval
  ```
  """
  @spec createGpuMatFromCudaMemory(integer(), integer(), integer(), integer(), [{:step, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def createGpuMatFromCudaMemory(rows, cols, type, cudaMemoryAddress, opts) when is_integer(rows) and is_integer(cols) and is_integer(type) and is_integer(cudaMemoryAddress) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:step])
    positional = [
      rows: Evision.Internal.Structurise.from_struct(rows),
      cols: Evision.Internal.Structurise.from_struct(cols),
      type: Evision.Internal.Structurise.from_struct(type),
      cudaMemoryAddress: Evision.Internal.Structurise.from_struct(cudaMemoryAddress)
    ]
    :evision_nif.cuda_createGpuMatFromCudaMemory(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Bindings overload to create a GpuMat from existing GPU memory.

  ##### Positional Arguments
  - **rows**: `integer()`.

    Row count.

  - **cols**: `integer()`.

    Column count.

  - **type**: `integer()`.

    Type of the matrix.

  - **cudaMemoryAddress**: `size_t`.

    Address of the allocated GPU memory on the device. This does not allocate matrix data. Instead, it just initializes the matrix header that points to the specified \\a cudaMemoryAddress, which means that no data is copied. This operation is very efficient and can be used to process external data using OpenCV functions. The external data is not automatically deallocated, so you should take care of it.

  ##### Keyword Arguments
  - **step**: `size_t`.

    Number of bytes each matrix row occupies. The value should include the padding bytes at the end of each row, if any. If the parameter is missing (set to Mat::AUTO_STEP ), no padding is assumed and the actual step is calculated as cols*elemSize(). See GpuMat::elemSize.

  ##### Return
  - **retval**: `Evision.CUDA.GpuMat.t()`

  **Note**: Overload for generation of bindings only, not exported or intended for use internally from C++.

  Python prototype (for reference only):
  ```python3
  createGpuMatFromCudaMemory(rows, cols, type, cudaMemoryAddress[, step]) -> retval
  ```
  #### Variant 2:
  createGpuMatFromCudaMemory

  ##### Positional Arguments
  - **size**: `Size`.

    2D array size: Size(cols, rows). In the Size() constructor, the number of rows and the number of columns go in the reverse order.

  - **type**: `integer()`.

    Type of the matrix.

  - **cudaMemoryAddress**: `size_t`.

    Address of the allocated GPU memory on the device. This does not allocate matrix data. Instead, it just initializes the matrix header that points to the specified \\a cudaMemoryAddress, which means that no data is copied. This operation is very efficient and can be used to process external data using OpenCV functions. The external data is not automatically deallocated, so you should take care of it.

  ##### Keyword Arguments
  - **step**: `size_t`.

    Number of bytes each matrix row occupies. The value should include the padding bytes at the end of each row, if any. If the parameter is missing (set to Mat::AUTO_STEP ), no padding is assumed and the actual step is calculated as cols*elemSize(). See GpuMat::elemSize.

  ##### Return
  - **retval**: `Evision.CUDA.GpuMat.t()`

  Has overloading in C++

  **Note**: Overload for generation of bindings only, not exported or intended for use internally from C++.

  Python prototype (for reference only):
  ```python3
  createGpuMatFromCudaMemory(size, type, cudaMemoryAddress[, step]) -> retval
  ```

  """
  @spec createGpuMatFromCudaMemory({number(), number()}, integer(), integer(), [{:step, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def createGpuMatFromCudaMemory(size, type, cudaMemoryAddress, opts) when is_tuple(size) and is_integer(type) and is_integer(cudaMemoryAddress) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:step])
    positional = [
      size: Evision.Internal.Structurise.from_struct(size),
      type: Evision.Internal.Structurise.from_struct(type),
      cudaMemoryAddress: Evision.Internal.Structurise.from_struct(cudaMemoryAddress)
    ]
    :evision_nif.cuda_createGpuMatFromCudaMemory(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec createGpuMatFromCudaMemory(integer(), integer(), integer(), integer()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def createGpuMatFromCudaMemory(rows, cols, type, cudaMemoryAddress) when is_integer(rows) and is_integer(cols) and is_integer(type) and is_integer(cudaMemoryAddress)
  do
    positional = [
      rows: Evision.Internal.Structurise.from_struct(rows),
      cols: Evision.Internal.Structurise.from_struct(cols),
      type: Evision.Internal.Structurise.from_struct(type),
      cudaMemoryAddress: Evision.Internal.Structurise.from_struct(cudaMemoryAddress)
    ]
    :evision_nif.cuda_createGpuMatFromCudaMemory(positional)
    |> to_struct()
  end

  @doc """
  createGpuMatFromCudaMemory

  ##### Positional Arguments
  - **size**: `Size`.

    2D array size: Size(cols, rows). In the Size() constructor, the number of rows and the number of columns go in the reverse order.

  - **type**: `integer()`.

    Type of the matrix.

  - **cudaMemoryAddress**: `size_t`.

    Address of the allocated GPU memory on the device. This does not allocate matrix data. Instead, it just initializes the matrix header that points to the specified \\a cudaMemoryAddress, which means that no data is copied. This operation is very efficient and can be used to process external data using OpenCV functions. The external data is not automatically deallocated, so you should take care of it.

  ##### Keyword Arguments
  - **step**: `size_t`.

    Number of bytes each matrix row occupies. The value should include the padding bytes at the end of each row, if any. If the parameter is missing (set to Mat::AUTO_STEP ), no padding is assumed and the actual step is calculated as cols*elemSize(). See GpuMat::elemSize.

  ##### Return
  - **retval**: `Evision.CUDA.GpuMat.t()`

  Has overloading in C++

  **Note**: Overload for generation of bindings only, not exported or intended for use internally from C++.

  Python prototype (for reference only):
  ```python3
  createGpuMatFromCudaMemory(size, type, cudaMemoryAddress[, step]) -> retval
  ```
  """
  @spec createGpuMatFromCudaMemory({number(), number()}, integer(), integer()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def createGpuMatFromCudaMemory(size, type, cudaMemoryAddress) when is_tuple(size) and is_integer(type) and is_integer(cudaMemoryAddress)
  do
    positional = [
      size: Evision.Internal.Structurise.from_struct(size),
      type: Evision.Internal.Structurise.from_struct(type),
      cudaMemoryAddress: Evision.Internal.Structurise.from_struct(cudaMemoryAddress)
    ]
    :evision_nif.cuda_createGpuMatFromCudaMemory(positional)
    |> to_struct()
  end

  @doc """
  Creates implementation for Harris cornerness criteria.

  ##### Positional Arguments
  - **srcType**: `integer()`.

    Input source type. Only CV_8UC1 and CV_32FC1 are supported for now.

  - **blockSize**: `integer()`.

    Neighborhood size.

  - **ksize**: `integer()`.

    Aperture parameter for the Sobel operator.

  - **k**: `double`.

    Harris detector free parameter.

  ##### Keyword Arguments
  - **borderType**: `integer()`.

    Pixel extrapolation method. Only BORDER_REFLECT101 and BORDER_REPLICATE are
    supported for now.

  ##### Return
  - **retval**: `CornernessCriteria`

  @sa cornerHarris

  Python prototype (for reference only):
  ```python3
  createHarrisCorner(srcType, blockSize, ksize, k[, borderType]) -> retval
  ```
  """
  @spec createHarrisCorner(integer(), integer(), integer(), number(), [{:borderType, term()}] | nil) :: Evision.CUDA.CornernessCriteria.t() | {:error, String.t()}
  def createHarrisCorner(srcType, blockSize, ksize, k, opts) when is_integer(srcType) and is_integer(blockSize) and is_integer(ksize) and is_number(k) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:borderType])
    positional = [
      srcType: Evision.Internal.Structurise.from_struct(srcType),
      blockSize: Evision.Internal.Structurise.from_struct(blockSize),
      ksize: Evision.Internal.Structurise.from_struct(ksize),
      k: Evision.Internal.Structurise.from_struct(k)
    ]
    :evision_nif.cuda_createHarrisCorner(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Creates implementation for Harris cornerness criteria.

  ##### Positional Arguments
  - **srcType**: `integer()`.

    Input source type. Only CV_8UC1 and CV_32FC1 are supported for now.

  - **blockSize**: `integer()`.

    Neighborhood size.

  - **ksize**: `integer()`.

    Aperture parameter for the Sobel operator.

  - **k**: `double`.

    Harris detector free parameter.

  ##### Keyword Arguments
  - **borderType**: `integer()`.

    Pixel extrapolation method. Only BORDER_REFLECT101 and BORDER_REPLICATE are
    supported for now.

  ##### Return
  - **retval**: `CornernessCriteria`

  @sa cornerHarris

  Python prototype (for reference only):
  ```python3
  createHarrisCorner(srcType, blockSize, ksize, k[, borderType]) -> retval
  ```
  """
  @spec createHarrisCorner(integer(), integer(), integer(), number()) :: Evision.CUDA.CornernessCriteria.t() | {:error, String.t()}
  def createHarrisCorner(srcType, blockSize, ksize, k) when is_integer(srcType) and is_integer(blockSize) and is_integer(ksize) and is_number(k)
  do
    positional = [
      srcType: Evision.Internal.Structurise.from_struct(srcType),
      blockSize: Evision.Internal.Structurise.from_struct(blockSize),
      ksize: Evision.Internal.Structurise.from_struct(ksize),
      k: Evision.Internal.Structurise.from_struct(k)
    ]
    :evision_nif.cuda_createHarrisCorner(positional)
    |> to_struct()
  end

  @doc """
  Creates implementation for cuda::HoughCirclesDetector .

  ##### Positional Arguments
  - **dp**: `float`.

    Inverse ratio of the accumulator resolution to the image resolution. For example, if
    dp=1 , the accumulator has the same resolution as the input image. If dp=2 , the accumulator has
    half as big width and height.

  - **minDist**: `float`.

    Minimum distance between the centers of the detected circles. If the parameter is
    too small, multiple neighbor circles may be falsely detected in addition to a true one. If it is
    too large, some circles may be missed.

  - **cannyThreshold**: `integer()`.

    The higher threshold of the two passed to Canny edge detector (the lower one
    is twice smaller).

  - **votesThreshold**: `integer()`.

    The accumulator threshold for the circle centers at the detection stage. The
    smaller it is, the more false circles may be detected.

  - **minRadius**: `integer()`.

    Minimum circle radius.

  - **maxRadius**: `integer()`.

    Maximum circle radius.

  ##### Keyword Arguments
  - **maxCircles**: `integer()`.

    Maximum number of output circles.

  ##### Return
  - **retval**: `HoughCirclesDetector`

  Python prototype (for reference only):
  ```python3
  createHoughCirclesDetector(dp, minDist, cannyThreshold, votesThreshold, minRadius, maxRadius[, maxCircles]) -> retval
  ```
  """
  @spec createHoughCirclesDetector(number(), number(), integer(), integer(), integer(), integer(), [{:maxCircles, term()}] | nil) :: Evision.CUDA.HoughCirclesDetector.t() | {:error, String.t()}
  def createHoughCirclesDetector(dp, minDist, cannyThreshold, votesThreshold, minRadius, maxRadius, opts) when is_float(dp) and is_float(minDist) and is_integer(cannyThreshold) and is_integer(votesThreshold) and is_integer(minRadius) and is_integer(maxRadius) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:maxCircles])
    positional = [
      dp: Evision.Internal.Structurise.from_struct(dp),
      minDist: Evision.Internal.Structurise.from_struct(minDist),
      cannyThreshold: Evision.Internal.Structurise.from_struct(cannyThreshold),
      votesThreshold: Evision.Internal.Structurise.from_struct(votesThreshold),
      minRadius: Evision.Internal.Structurise.from_struct(minRadius),
      maxRadius: Evision.Internal.Structurise.from_struct(maxRadius)
    ]
    :evision_nif.cuda_createHoughCirclesDetector(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Creates implementation for cuda::HoughCirclesDetector .

  ##### Positional Arguments
  - **dp**: `float`.

    Inverse ratio of the accumulator resolution to the image resolution. For example, if
    dp=1 , the accumulator has the same resolution as the input image. If dp=2 , the accumulator has
    half as big width and height.

  - **minDist**: `float`.

    Minimum distance between the centers of the detected circles. If the parameter is
    too small, multiple neighbor circles may be falsely detected in addition to a true one. If it is
    too large, some circles may be missed.

  - **cannyThreshold**: `integer()`.

    The higher threshold of the two passed to Canny edge detector (the lower one
    is twice smaller).

  - **votesThreshold**: `integer()`.

    The accumulator threshold for the circle centers at the detection stage. The
    smaller it is, the more false circles may be detected.

  - **minRadius**: `integer()`.

    Minimum circle radius.

  - **maxRadius**: `integer()`.

    Maximum circle radius.

  ##### Keyword Arguments
  - **maxCircles**: `integer()`.

    Maximum number of output circles.

  ##### Return
  - **retval**: `HoughCirclesDetector`

  Python prototype (for reference only):
  ```python3
  createHoughCirclesDetector(dp, minDist, cannyThreshold, votesThreshold, minRadius, maxRadius[, maxCircles]) -> retval
  ```
  """
  @spec createHoughCirclesDetector(number(), number(), integer(), integer(), integer(), integer()) :: Evision.CUDA.HoughCirclesDetector.t() | {:error, String.t()}
  def createHoughCirclesDetector(dp, minDist, cannyThreshold, votesThreshold, minRadius, maxRadius) when is_float(dp) and is_float(minDist) and is_integer(cannyThreshold) and is_integer(votesThreshold) and is_integer(minRadius) and is_integer(maxRadius)
  do
    positional = [
      dp: Evision.Internal.Structurise.from_struct(dp),
      minDist: Evision.Internal.Structurise.from_struct(minDist),
      cannyThreshold: Evision.Internal.Structurise.from_struct(cannyThreshold),
      votesThreshold: Evision.Internal.Structurise.from_struct(votesThreshold),
      minRadius: Evision.Internal.Structurise.from_struct(minRadius),
      maxRadius: Evision.Internal.Structurise.from_struct(maxRadius)
    ]
    :evision_nif.cuda_createHoughCirclesDetector(positional)
    |> to_struct()
  end

  @doc """
  Creates implementation for cuda::HoughLinesDetector .

  ##### Positional Arguments
  - **rho**: `float`.

    Distance resolution of the accumulator in pixels.

  - **theta**: `float`.

    Angle resolution of the accumulator in radians.

  - **threshold**: `integer()`.

    Accumulator threshold parameter. Only those lines are returned that get enough
    votes ( \\f$>\\texttt{threshold}\\f$ ).

  ##### Keyword Arguments
  - **doSort**: `bool`.

    Performs lines sort by votes.

  - **maxLines**: `integer()`.

    Maximum number of output lines.

  ##### Return
  - **retval**: `HoughLinesDetector`

  Python prototype (for reference only):
  ```python3
  createHoughLinesDetector(rho, theta, threshold[, doSort[, maxLines]]) -> retval
  ```
  """
  @spec createHoughLinesDetector(number(), number(), integer(), [{:doSort, term()} | {:maxLines, term()}] | nil) :: Evision.CUDA.HoughLinesDetector.t() | {:error, String.t()}
  def createHoughLinesDetector(rho, theta, threshold, opts) when is_float(rho) and is_float(theta) and is_integer(threshold) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:doSort, :maxLines])
    positional = [
      rho: Evision.Internal.Structurise.from_struct(rho),
      theta: Evision.Internal.Structurise.from_struct(theta),
      threshold: Evision.Internal.Structurise.from_struct(threshold)
    ]
    :evision_nif.cuda_createHoughLinesDetector(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Creates implementation for cuda::HoughLinesDetector .

  ##### Positional Arguments
  - **rho**: `float`.

    Distance resolution of the accumulator in pixels.

  - **theta**: `float`.

    Angle resolution of the accumulator in radians.

  - **threshold**: `integer()`.

    Accumulator threshold parameter. Only those lines are returned that get enough
    votes ( \\f$>\\texttt{threshold}\\f$ ).

  ##### Keyword Arguments
  - **doSort**: `bool`.

    Performs lines sort by votes.

  - **maxLines**: `integer()`.

    Maximum number of output lines.

  ##### Return
  - **retval**: `HoughLinesDetector`

  Python prototype (for reference only):
  ```python3
  createHoughLinesDetector(rho, theta, threshold[, doSort[, maxLines]]) -> retval
  ```
  """
  @spec createHoughLinesDetector(number(), number(), integer()) :: Evision.CUDA.HoughLinesDetector.t() | {:error, String.t()}
  def createHoughLinesDetector(rho, theta, threshold) when is_float(rho) and is_float(theta) and is_integer(threshold)
  do
    positional = [
      rho: Evision.Internal.Structurise.from_struct(rho),
      theta: Evision.Internal.Structurise.from_struct(theta),
      threshold: Evision.Internal.Structurise.from_struct(threshold)
    ]
    :evision_nif.cuda_createHoughLinesDetector(positional)
    |> to_struct()
  end

  @doc """
  Creates implementation for cuda::HoughSegmentDetector .

  ##### Positional Arguments
  - **rho**: `float`.

    Distance resolution of the accumulator in pixels.

  - **theta**: `float`.

    Angle resolution of the accumulator in radians.

  - **minLineLength**: `integer()`.

    Minimum line length. Line segments shorter than that are rejected.

  - **maxLineGap**: `integer()`.

    Maximum allowed gap between points on the same line to link them.

  ##### Keyword Arguments
  - **maxLines**: `integer()`.

    Maximum number of output lines.

  - **threshold**: `integer()`.

    %Accumulator threshold parameter. Only those lines are returned that get enough
    votes ( \\f$>\\texttt{threshold}\\f$ ).

  ##### Return
  - **retval**: `HoughSegmentDetector`

  Python prototype (for reference only):
  ```python3
  createHoughSegmentDetector(rho, theta, minLineLength, maxLineGap[, maxLines[, threshold]]) -> retval
  ```
  """
  @spec createHoughSegmentDetector(number(), number(), integer(), integer(), [{:maxLines, term()} | {:threshold, term()}] | nil) :: Evision.CUDA.HoughSegmentDetector.t() | {:error, String.t()}
  def createHoughSegmentDetector(rho, theta, minLineLength, maxLineGap, opts) when is_float(rho) and is_float(theta) and is_integer(minLineLength) and is_integer(maxLineGap) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:maxLines, :threshold])
    positional = [
      rho: Evision.Internal.Structurise.from_struct(rho),
      theta: Evision.Internal.Structurise.from_struct(theta),
      minLineLength: Evision.Internal.Structurise.from_struct(minLineLength),
      maxLineGap: Evision.Internal.Structurise.from_struct(maxLineGap)
    ]
    :evision_nif.cuda_createHoughSegmentDetector(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Creates implementation for cuda::HoughSegmentDetector .

  ##### Positional Arguments
  - **rho**: `float`.

    Distance resolution of the accumulator in pixels.

  - **theta**: `float`.

    Angle resolution of the accumulator in radians.

  - **minLineLength**: `integer()`.

    Minimum line length. Line segments shorter than that are rejected.

  - **maxLineGap**: `integer()`.

    Maximum allowed gap between points on the same line to link them.

  ##### Keyword Arguments
  - **maxLines**: `integer()`.

    Maximum number of output lines.

  - **threshold**: `integer()`.

    %Accumulator threshold parameter. Only those lines are returned that get enough
    votes ( \\f$>\\texttt{threshold}\\f$ ).

  ##### Return
  - **retval**: `HoughSegmentDetector`

  Python prototype (for reference only):
  ```python3
  createHoughSegmentDetector(rho, theta, minLineLength, maxLineGap[, maxLines[, threshold]]) -> retval
  ```
  """
  @spec createHoughSegmentDetector(number(), number(), integer(), integer()) :: Evision.CUDA.HoughSegmentDetector.t() | {:error, String.t()}
  def createHoughSegmentDetector(rho, theta, minLineLength, maxLineGap) when is_float(rho) and is_float(theta) and is_integer(minLineLength) and is_integer(maxLineGap)
  do
    positional = [
      rho: Evision.Internal.Structurise.from_struct(rho),
      theta: Evision.Internal.Structurise.from_struct(theta),
      minLineLength: Evision.Internal.Structurise.from_struct(minLineLength),
      maxLineGap: Evision.Internal.Structurise.from_struct(maxLineGap)
    ]
    :evision_nif.cuda_createHoughSegmentDetector(positional)
    |> to_struct()
  end

  @doc """
  Creates a Laplacian operator.

  ##### Positional Arguments
  - **srcType**: `integer()`.

    Input image type. Supports CV_8U , CV_16U and CV_32F one and four channel image.

  - **dstType**: `integer()`.

    Output image type. Only the same type as src is supported for now.

  ##### Keyword Arguments
  - **ksize**: `integer()`.

    Aperture size used to compute the second-derivative filters (see getDerivKernels). It
    must be positive and odd. Only ksize = 1 and ksize = 3 are supported.

  - **scale**: `double`.

    Optional scale factor for the computed Laplacian values. By default, no scaling is
    applied (see getDerivKernels ).

  - **borderMode**: `integer()`.

    Pixel extrapolation method. For details, see borderInterpolate .

  - **borderVal**: `Evision.scalar()`.

    Default border value.

  ##### Return
  - **retval**: `Filter`

  @sa Laplacian

  Python prototype (for reference only):
  ```python3
  createLaplacianFilter(srcType, dstType[, ksize[, scale[, borderMode[, borderVal]]]]) -> retval
  ```
  """
  @spec createLaplacianFilter(integer(), integer(), [{:borderMode, term()} | {:borderVal, term()} | {:ksize, term()} | {:scale, term()}] | nil) :: Evision.CUDA.Filter.t() | {:error, String.t()}
  def createLaplacianFilter(srcType, dstType, opts) when is_integer(srcType) and is_integer(dstType) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:borderMode, :borderVal, :ksize, :scale])
    positional = [
      srcType: Evision.Internal.Structurise.from_struct(srcType),
      dstType: Evision.Internal.Structurise.from_struct(dstType)
    ]
    :evision_nif.cuda_createLaplacianFilter(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Creates a Laplacian operator.

  ##### Positional Arguments
  - **srcType**: `integer()`.

    Input image type. Supports CV_8U , CV_16U and CV_32F one and four channel image.

  - **dstType**: `integer()`.

    Output image type. Only the same type as src is supported for now.

  ##### Keyword Arguments
  - **ksize**: `integer()`.

    Aperture size used to compute the second-derivative filters (see getDerivKernels). It
    must be positive and odd. Only ksize = 1 and ksize = 3 are supported.

  - **scale**: `double`.

    Optional scale factor for the computed Laplacian values. By default, no scaling is
    applied (see getDerivKernels ).

  - **borderMode**: `integer()`.

    Pixel extrapolation method. For details, see borderInterpolate .

  - **borderVal**: `Evision.scalar()`.

    Default border value.

  ##### Return
  - **retval**: `Filter`

  @sa Laplacian

  Python prototype (for reference only):
  ```python3
  createLaplacianFilter(srcType, dstType[, ksize[, scale[, borderMode[, borderVal]]]]) -> retval
  ```
  """
  @spec createLaplacianFilter(integer(), integer()) :: Evision.CUDA.Filter.t() | {:error, String.t()}
  def createLaplacianFilter(srcType, dstType) when is_integer(srcType) and is_integer(dstType)
  do
    positional = [
      srcType: Evision.Internal.Structurise.from_struct(srcType),
      dstType: Evision.Internal.Structurise.from_struct(dstType)
    ]
    :evision_nif.cuda_createLaplacianFilter(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Creates a non-separable linear 2D filter.

  ##### Positional Arguments
  - **srcType**: `integer()`.

    Input image type. Supports CV_8U , CV_16U and CV_32F one and four channel image.

  - **dstType**: `integer()`.

    Output image type. Only the same type as src is supported for now.

  - **kernel**: `Evision.Mat`.

    2D array of filter coefficients.

  ##### Keyword Arguments
  - **anchor**: `Point`.

    Anchor point. The default value Point(-1, -1) means that the anchor is at the kernel
    center.

  - **borderMode**: `integer()`.

    Pixel extrapolation method. For details, see borderInterpolate .

  - **borderVal**: `Evision.scalar()`.

    Default border value.

  ##### Return
  - **retval**: `Filter`

  @sa filter2D

  Python prototype (for reference only):
  ```python3
  createLinearFilter(srcType, dstType, kernel[, anchor[, borderMode[, borderVal]]]) -> retval
  ```
  #### Variant 2:
  Creates a non-separable linear 2D filter.

  ##### Positional Arguments
  - **srcType**: `integer()`.

    Input image type. Supports CV_8U , CV_16U and CV_32F one and four channel image.

  - **dstType**: `integer()`.

    Output image type. Only the same type as src is supported for now.

  - **kernel**: `Evision.CUDA.GpuMat.t()`.

    2D array of filter coefficients.

  ##### Keyword Arguments
  - **anchor**: `Point`.

    Anchor point. The default value Point(-1, -1) means that the anchor is at the kernel
    center.

  - **borderMode**: `integer()`.

    Pixel extrapolation method. For details, see borderInterpolate .

  - **borderVal**: `Evision.scalar()`.

    Default border value.

  ##### Return
  - **retval**: `Filter`

  @sa filter2D

  Python prototype (for reference only):
  ```python3
  createLinearFilter(srcType, dstType, kernel[, anchor[, borderMode[, borderVal]]]) -> retval
  ```

  """
  @spec createLinearFilter(integer(), integer(), Evision.Mat.maybe_mat_in(), [{:anchor, term()} | {:borderMode, term()} | {:borderVal, term()}] | nil) :: Evision.CUDA.Filter.t() | {:error, String.t()}
  def createLinearFilter(srcType, dstType, kernel, opts) when is_integer(srcType) and is_integer(dstType) and (is_struct(kernel, Evision.Mat) or is_struct(kernel, Nx.Tensor) or is_number(kernel) or is_tuple(kernel)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:anchor, :borderMode, :borderVal])
    positional = [
      srcType: Evision.Internal.Structurise.from_struct(srcType),
      dstType: Evision.Internal.Structurise.from_struct(dstType),
      kernel: Evision.Internal.Structurise.from_struct(kernel)
    ]
    :evision_nif.cuda_createLinearFilter(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec createLinearFilter(integer(), integer(), Evision.CUDA.GpuMat.t(), [{:anchor, term()} | {:borderMode, term()} | {:borderVal, term()}] | nil) :: Evision.CUDA.Filter.t() | {:error, String.t()}
  def createLinearFilter(srcType, dstType, kernel, opts) when is_integer(srcType) and is_integer(dstType) and is_struct(kernel, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:anchor, :borderMode, :borderVal])
    positional = [
      srcType: Evision.Internal.Structurise.from_struct(srcType),
      dstType: Evision.Internal.Structurise.from_struct(dstType),
      kernel: Evision.Internal.Structurise.from_struct(kernel)
    ]
    :evision_nif.cuda_createLinearFilter(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Creates a non-separable linear 2D filter.

  ##### Positional Arguments
  - **srcType**: `integer()`.

    Input image type. Supports CV_8U , CV_16U and CV_32F one and four channel image.

  - **dstType**: `integer()`.

    Output image type. Only the same type as src is supported for now.

  - **kernel**: `Evision.Mat`.

    2D array of filter coefficients.

  ##### Keyword Arguments
  - **anchor**: `Point`.

    Anchor point. The default value Point(-1, -1) means that the anchor is at the kernel
    center.

  - **borderMode**: `integer()`.

    Pixel extrapolation method. For details, see borderInterpolate .

  - **borderVal**: `Evision.scalar()`.

    Default border value.

  ##### Return
  - **retval**: `Filter`

  @sa filter2D

  Python prototype (for reference only):
  ```python3
  createLinearFilter(srcType, dstType, kernel[, anchor[, borderMode[, borderVal]]]) -> retval
  ```
  #### Variant 2:
  Creates a non-separable linear 2D filter.

  ##### Positional Arguments
  - **srcType**: `integer()`.

    Input image type. Supports CV_8U , CV_16U and CV_32F one and four channel image.

  - **dstType**: `integer()`.

    Output image type. Only the same type as src is supported for now.

  - **kernel**: `Evision.CUDA.GpuMat.t()`.

    2D array of filter coefficients.

  ##### Keyword Arguments
  - **anchor**: `Point`.

    Anchor point. The default value Point(-1, -1) means that the anchor is at the kernel
    center.

  - **borderMode**: `integer()`.

    Pixel extrapolation method. For details, see borderInterpolate .

  - **borderVal**: `Evision.scalar()`.

    Default border value.

  ##### Return
  - **retval**: `Filter`

  @sa filter2D

  Python prototype (for reference only):
  ```python3
  createLinearFilter(srcType, dstType, kernel[, anchor[, borderMode[, borderVal]]]) -> retval
  ```

  """
  @spec createLinearFilter(integer(), integer(), Evision.Mat.maybe_mat_in()) :: Evision.CUDA.Filter.t() | {:error, String.t()}
  def createLinearFilter(srcType, dstType, kernel) when is_integer(srcType) and is_integer(dstType) and (is_struct(kernel, Evision.Mat) or is_struct(kernel, Nx.Tensor) or is_number(kernel) or is_tuple(kernel))
  do
    positional = [
      srcType: Evision.Internal.Structurise.from_struct(srcType),
      dstType: Evision.Internal.Structurise.from_struct(dstType),
      kernel: Evision.Internal.Structurise.from_struct(kernel)
    ]
    :evision_nif.cuda_createLinearFilter(positional)
    |> to_struct()
  end
  @spec createLinearFilter(integer(), integer(), Evision.CUDA.GpuMat.t()) :: Evision.CUDA.Filter.t() | {:error, String.t()}
  def createLinearFilter(srcType, dstType, kernel) when is_integer(srcType) and is_integer(dstType) and is_struct(kernel, Evision.CUDA.GpuMat)
  do
    positional = [
      srcType: Evision.Internal.Structurise.from_struct(srcType),
      dstType: Evision.Internal.Structurise.from_struct(dstType),
      kernel: Evision.Internal.Structurise.from_struct(kernel)
    ]
    :evision_nif.cuda_createLinearFilter(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Creates implementation for cuda::LookUpTable .

  ##### Positional Arguments
  - **lut**: `Evision.Mat`.

    Look-up table of 256 elements. It is a continuous CV_8U matrix.

  ##### Return
  - **retval**: `LookUpTable`

  Python prototype (for reference only):
  ```python3
  createLookUpTable(lut) -> retval
  ```
  #### Variant 2:
  Creates implementation for cuda::LookUpTable .

  ##### Positional Arguments
  - **lut**: `Evision.CUDA.GpuMat.t()`.

    Look-up table of 256 elements. It is a continuous CV_8U matrix.

  ##### Return
  - **retval**: `LookUpTable`

  Python prototype (for reference only):
  ```python3
  createLookUpTable(lut) -> retval
  ```

  """
  @spec createLookUpTable(Evision.Mat.maybe_mat_in()) :: Evision.CUDA.LookUpTable.t() | {:error, String.t()}
  def createLookUpTable(lut) when (is_struct(lut, Evision.Mat) or is_struct(lut, Nx.Tensor) or is_number(lut) or is_tuple(lut))
  do
    positional = [
      lut: Evision.Internal.Structurise.from_struct(lut)
    ]
    :evision_nif.cuda_createLookUpTable(positional)
    |> to_struct()
  end
  @spec createLookUpTable(Evision.CUDA.GpuMat.t()) :: Evision.CUDA.LookUpTable.t() | {:error, String.t()}
  def createLookUpTable(lut) when is_struct(lut, Evision.CUDA.GpuMat)
  do
    positional = [
      lut: Evision.Internal.Structurise.from_struct(lut)
    ]
    :evision_nif.cuda_createLookUpTable(positional)
    |> to_struct()
  end

  @doc """
  Performs median filtering for each point of the source image.

  ##### Positional Arguments
  - **srcType**: `integer()`.

    type of of source image. Only CV_8UC1 images are supported for now.

  - **windowSize**: `integer()`.

    Size of the kernerl used for the filtering. Uses a (windowSize x windowSize) filter.

  ##### Keyword Arguments
  - **partition**: `integer()`.

    Specifies the parallel granularity of the workload. This parameter should be used GPU experts when optimizing performance.

  ##### Return
  - **retval**: `Filter`

  Outputs an image that has been filtered using a median-filtering formulation.
  Details on this algorithm can be found in:
  Green, O., 2017. "Efficient scalable median filtering using histogram-based operations",
  IEEE Transactions on Image Processing, 27(5), pp.2217-2228.

  Python prototype (for reference only):
  ```python3
  createMedianFilter(srcType, windowSize[, partition]) -> retval
  ```
  """
  @spec createMedianFilter(integer(), integer(), [{:partition, term()}] | nil) :: Evision.CUDA.Filter.t() | {:error, String.t()}
  def createMedianFilter(srcType, windowSize, opts) when is_integer(srcType) and is_integer(windowSize) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:partition])
    positional = [
      srcType: Evision.Internal.Structurise.from_struct(srcType),
      windowSize: Evision.Internal.Structurise.from_struct(windowSize)
    ]
    :evision_nif.cuda_createMedianFilter(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Performs median filtering for each point of the source image.

  ##### Positional Arguments
  - **srcType**: `integer()`.

    type of of source image. Only CV_8UC1 images are supported for now.

  - **windowSize**: `integer()`.

    Size of the kernerl used for the filtering. Uses a (windowSize x windowSize) filter.

  ##### Keyword Arguments
  - **partition**: `integer()`.

    Specifies the parallel granularity of the workload. This parameter should be used GPU experts when optimizing performance.

  ##### Return
  - **retval**: `Filter`

  Outputs an image that has been filtered using a median-filtering formulation.
  Details on this algorithm can be found in:
  Green, O., 2017. "Efficient scalable median filtering using histogram-based operations",
  IEEE Transactions on Image Processing, 27(5), pp.2217-2228.

  Python prototype (for reference only):
  ```python3
  createMedianFilter(srcType, windowSize[, partition]) -> retval
  ```
  """
  @spec createMedianFilter(integer(), integer()) :: Evision.CUDA.Filter.t() | {:error, String.t()}
  def createMedianFilter(srcType, windowSize) when is_integer(srcType) and is_integer(windowSize)
  do
    positional = [
      srcType: Evision.Internal.Structurise.from_struct(srcType),
      windowSize: Evision.Internal.Structurise.from_struct(windowSize)
    ]
    :evision_nif.cuda_createMedianFilter(positional)
    |> to_struct()
  end

  @doc """
  Creates implementation for the minimum eigen value of a 2x2 derivative covariation matrix (the
  cornerness criteria).

  ##### Positional Arguments
  - **srcType**: `integer()`.

    Input source type. Only CV_8UC1 and CV_32FC1 are supported for now.

  - **blockSize**: `integer()`.

    Neighborhood size.

  - **ksize**: `integer()`.

    Aperture parameter for the Sobel operator.

  ##### Keyword Arguments
  - **borderType**: `integer()`.

    Pixel extrapolation method. Only BORDER_REFLECT101 and BORDER_REPLICATE are
    supported for now.

  ##### Return
  - **retval**: `CornernessCriteria`

  @sa cornerMinEigenVal

  Python prototype (for reference only):
  ```python3
  createMinEigenValCorner(srcType, blockSize, ksize[, borderType]) -> retval
  ```
  """
  @spec createMinEigenValCorner(integer(), integer(), integer(), [{:borderType, term()}] | nil) :: Evision.CUDA.CornernessCriteria.t() | {:error, String.t()}
  def createMinEigenValCorner(srcType, blockSize, ksize, opts) when is_integer(srcType) and is_integer(blockSize) and is_integer(ksize) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:borderType])
    positional = [
      srcType: Evision.Internal.Structurise.from_struct(srcType),
      blockSize: Evision.Internal.Structurise.from_struct(blockSize),
      ksize: Evision.Internal.Structurise.from_struct(ksize)
    ]
    :evision_nif.cuda_createMinEigenValCorner(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Creates implementation for the minimum eigen value of a 2x2 derivative covariation matrix (the
  cornerness criteria).

  ##### Positional Arguments
  - **srcType**: `integer()`.

    Input source type. Only CV_8UC1 and CV_32FC1 are supported for now.

  - **blockSize**: `integer()`.

    Neighborhood size.

  - **ksize**: `integer()`.

    Aperture parameter for the Sobel operator.

  ##### Keyword Arguments
  - **borderType**: `integer()`.

    Pixel extrapolation method. Only BORDER_REFLECT101 and BORDER_REPLICATE are
    supported for now.

  ##### Return
  - **retval**: `CornernessCriteria`

  @sa cornerMinEigenVal

  Python prototype (for reference only):
  ```python3
  createMinEigenValCorner(srcType, blockSize, ksize[, borderType]) -> retval
  ```
  """
  @spec createMinEigenValCorner(integer(), integer(), integer()) :: Evision.CUDA.CornernessCriteria.t() | {:error, String.t()}
  def createMinEigenValCorner(srcType, blockSize, ksize) when is_integer(srcType) and is_integer(blockSize) and is_integer(ksize)
  do
    positional = [
      srcType: Evision.Internal.Structurise.from_struct(srcType),
      blockSize: Evision.Internal.Structurise.from_struct(blockSize),
      ksize: Evision.Internal.Structurise.from_struct(ksize)
    ]
    :evision_nif.cuda_createMinEigenValCorner(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Creates a 2D morphological filter.

  ##### Positional Arguments
  - **op**: `integer()`.

    Type of morphological operation. The following types are possible:
    - **MORPH_ERODE** erode
    - **MORPH_DILATE** dilate
    - **MORPH_OPEN** opening
    - **MORPH_CLOSE** closing
    - **MORPH_GRADIENT** morphological gradient
    - **MORPH_TOPHAT** "top hat"
    - **MORPH_BLACKHAT** "black hat"

  - **srcType**: `integer()`.

    Input/output image type. Only CV_8UC1, CV_8UC4, CV_32FC1 and CV_32FC4 are supported.

  - **kernel**: `Evision.Mat`.

    2D 8-bit structuring element for the morphological operation.

  ##### Keyword Arguments
  - **anchor**: `Point`.

    Anchor position within the structuring element. Negative values mean that the anchor
    is at the center.

  - **iterations**: `integer()`.

    Number of times erosion and dilation to be applied.

  ##### Return
  - **retval**: `Filter`

  @sa morphologyEx

  Python prototype (for reference only):
  ```python3
  createMorphologyFilter(op, srcType, kernel[, anchor[, iterations]]) -> retval
  ```
  #### Variant 2:
  Creates a 2D morphological filter.

  ##### Positional Arguments
  - **op**: `integer()`.

    Type of morphological operation. The following types are possible:
    - **MORPH_ERODE** erode
    - **MORPH_DILATE** dilate
    - **MORPH_OPEN** opening
    - **MORPH_CLOSE** closing
    - **MORPH_GRADIENT** morphological gradient
    - **MORPH_TOPHAT** "top hat"
    - **MORPH_BLACKHAT** "black hat"

  - **srcType**: `integer()`.

    Input/output image type. Only CV_8UC1, CV_8UC4, CV_32FC1 and CV_32FC4 are supported.

  - **kernel**: `Evision.CUDA.GpuMat.t()`.

    2D 8-bit structuring element for the morphological operation.

  ##### Keyword Arguments
  - **anchor**: `Point`.

    Anchor position within the structuring element. Negative values mean that the anchor
    is at the center.

  - **iterations**: `integer()`.

    Number of times erosion and dilation to be applied.

  ##### Return
  - **retval**: `Filter`

  @sa morphologyEx

  Python prototype (for reference only):
  ```python3
  createMorphologyFilter(op, srcType, kernel[, anchor[, iterations]]) -> retval
  ```

  """
  @spec createMorphologyFilter(integer(), integer(), Evision.Mat.maybe_mat_in(), [{:anchor, term()} | {:iterations, term()}] | nil) :: Evision.CUDA.Filter.t() | {:error, String.t()}
  def createMorphologyFilter(op, srcType, kernel, opts) when is_integer(op) and is_integer(srcType) and (is_struct(kernel, Evision.Mat) or is_struct(kernel, Nx.Tensor) or is_number(kernel) or is_tuple(kernel)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:anchor, :iterations])
    positional = [
      op: Evision.Internal.Structurise.from_struct(op),
      srcType: Evision.Internal.Structurise.from_struct(srcType),
      kernel: Evision.Internal.Structurise.from_struct(kernel)
    ]
    :evision_nif.cuda_createMorphologyFilter(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec createMorphologyFilter(integer(), integer(), Evision.CUDA.GpuMat.t(), [{:anchor, term()} | {:iterations, term()}] | nil) :: Evision.CUDA.Filter.t() | {:error, String.t()}
  def createMorphologyFilter(op, srcType, kernel, opts) when is_integer(op) and is_integer(srcType) and is_struct(kernel, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:anchor, :iterations])
    positional = [
      op: Evision.Internal.Structurise.from_struct(op),
      srcType: Evision.Internal.Structurise.from_struct(srcType),
      kernel: Evision.Internal.Structurise.from_struct(kernel)
    ]
    :evision_nif.cuda_createMorphologyFilter(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Creates a 2D morphological filter.

  ##### Positional Arguments
  - **op**: `integer()`.

    Type of morphological operation. The following types are possible:
    - **MORPH_ERODE** erode
    - **MORPH_DILATE** dilate
    - **MORPH_OPEN** opening
    - **MORPH_CLOSE** closing
    - **MORPH_GRADIENT** morphological gradient
    - **MORPH_TOPHAT** "top hat"
    - **MORPH_BLACKHAT** "black hat"

  - **srcType**: `integer()`.

    Input/output image type. Only CV_8UC1, CV_8UC4, CV_32FC1 and CV_32FC4 are supported.

  - **kernel**: `Evision.Mat`.

    2D 8-bit structuring element for the morphological operation.

  ##### Keyword Arguments
  - **anchor**: `Point`.

    Anchor position within the structuring element. Negative values mean that the anchor
    is at the center.

  - **iterations**: `integer()`.

    Number of times erosion and dilation to be applied.

  ##### Return
  - **retval**: `Filter`

  @sa morphologyEx

  Python prototype (for reference only):
  ```python3
  createMorphologyFilter(op, srcType, kernel[, anchor[, iterations]]) -> retval
  ```
  #### Variant 2:
  Creates a 2D morphological filter.

  ##### Positional Arguments
  - **op**: `integer()`.

    Type of morphological operation. The following types are possible:
    - **MORPH_ERODE** erode
    - **MORPH_DILATE** dilate
    - **MORPH_OPEN** opening
    - **MORPH_CLOSE** closing
    - **MORPH_GRADIENT** morphological gradient
    - **MORPH_TOPHAT** "top hat"
    - **MORPH_BLACKHAT** "black hat"

  - **srcType**: `integer()`.

    Input/output image type. Only CV_8UC1, CV_8UC4, CV_32FC1 and CV_32FC4 are supported.

  - **kernel**: `Evision.CUDA.GpuMat.t()`.

    2D 8-bit structuring element for the morphological operation.

  ##### Keyword Arguments
  - **anchor**: `Point`.

    Anchor position within the structuring element. Negative values mean that the anchor
    is at the center.

  - **iterations**: `integer()`.

    Number of times erosion and dilation to be applied.

  ##### Return
  - **retval**: `Filter`

  @sa morphologyEx

  Python prototype (for reference only):
  ```python3
  createMorphologyFilter(op, srcType, kernel[, anchor[, iterations]]) -> retval
  ```

  """
  @spec createMorphologyFilter(integer(), integer(), Evision.Mat.maybe_mat_in()) :: Evision.CUDA.Filter.t() | {:error, String.t()}
  def createMorphologyFilter(op, srcType, kernel) when is_integer(op) and is_integer(srcType) and (is_struct(kernel, Evision.Mat) or is_struct(kernel, Nx.Tensor) or is_number(kernel) or is_tuple(kernel))
  do
    positional = [
      op: Evision.Internal.Structurise.from_struct(op),
      srcType: Evision.Internal.Structurise.from_struct(srcType),
      kernel: Evision.Internal.Structurise.from_struct(kernel)
    ]
    :evision_nif.cuda_createMorphologyFilter(positional)
    |> to_struct()
  end
  @spec createMorphologyFilter(integer(), integer(), Evision.CUDA.GpuMat.t()) :: Evision.CUDA.Filter.t() | {:error, String.t()}
  def createMorphologyFilter(op, srcType, kernel) when is_integer(op) and is_integer(srcType) and is_struct(kernel, Evision.CUDA.GpuMat)
  do
    positional = [
      op: Evision.Internal.Structurise.from_struct(op),
      srcType: Evision.Internal.Structurise.from_struct(srcType),
      kernel: Evision.Internal.Structurise.from_struct(kernel)
    ]
    :evision_nif.cuda_createMorphologyFilter(positional)
    |> to_struct()
  end

  @doc """
  Creates a horizontal 1D box filter.

  ##### Positional Arguments
  - **srcType**: `integer()`.

    Input image type. Only CV_8UC1 type is supported for now.

  - **dstType**: `integer()`.

    Output image type. Only CV_32FC1 type is supported for now.

  - **ksize**: `integer()`.

    Kernel size.

  ##### Keyword Arguments
  - **anchor**: `integer()`.

    Anchor point. The default value (-1) means that the anchor is at the kernel center.

  - **borderMode**: `integer()`.

    Pixel extrapolation method. For details, see borderInterpolate .

  - **borderVal**: `Evision.scalar()`.

    Default border value.

  ##### Return
  - **retval**: `Filter`

  Python prototype (for reference only):
  ```python3
  createRowSumFilter(srcType, dstType, ksize[, anchor[, borderMode[, borderVal]]]) -> retval
  ```
  """
  @spec createRowSumFilter(integer(), integer(), integer(), [{:anchor, term()} | {:borderMode, term()} | {:borderVal, term()}] | nil) :: Evision.CUDA.Filter.t() | {:error, String.t()}
  def createRowSumFilter(srcType, dstType, ksize, opts) when is_integer(srcType) and is_integer(dstType) and is_integer(ksize) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:anchor, :borderMode, :borderVal])
    positional = [
      srcType: Evision.Internal.Structurise.from_struct(srcType),
      dstType: Evision.Internal.Structurise.from_struct(dstType),
      ksize: Evision.Internal.Structurise.from_struct(ksize)
    ]
    :evision_nif.cuda_createRowSumFilter(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Creates a horizontal 1D box filter.

  ##### Positional Arguments
  - **srcType**: `integer()`.

    Input image type. Only CV_8UC1 type is supported for now.

  - **dstType**: `integer()`.

    Output image type. Only CV_32FC1 type is supported for now.

  - **ksize**: `integer()`.

    Kernel size.

  ##### Keyword Arguments
  - **anchor**: `integer()`.

    Anchor point. The default value (-1) means that the anchor is at the kernel center.

  - **borderMode**: `integer()`.

    Pixel extrapolation method. For details, see borderInterpolate .

  - **borderVal**: `Evision.scalar()`.

    Default border value.

  ##### Return
  - **retval**: `Filter`

  Python prototype (for reference only):
  ```python3
  createRowSumFilter(srcType, dstType, ksize[, anchor[, borderMode[, borderVal]]]) -> retval
  ```
  """
  @spec createRowSumFilter(integer(), integer(), integer()) :: Evision.CUDA.Filter.t() | {:error, String.t()}
  def createRowSumFilter(srcType, dstType, ksize) when is_integer(srcType) and is_integer(dstType) and is_integer(ksize)
  do
    positional = [
      srcType: Evision.Internal.Structurise.from_struct(srcType),
      dstType: Evision.Internal.Structurise.from_struct(dstType),
      ksize: Evision.Internal.Structurise.from_struct(ksize)
    ]
    :evision_nif.cuda_createRowSumFilter(positional)
    |> to_struct()
  end

  @doc """
  Creates a vertical or horizontal Scharr operator.

  ##### Positional Arguments
  - **srcType**: `integer()`.

    Source image type.

  - **dstType**: `integer()`.

    Destination array type.

  - **dx**: `integer()`.

    Order of the derivative x.

  - **dy**: `integer()`.

    Order of the derivative y.

  ##### Keyword Arguments
  - **scale**: `double`.

    Optional scale factor for the computed derivative values. By default, no scaling is
    applied. See getDerivKernels for details.

  - **rowBorderMode**: `integer()`.

    Pixel extrapolation method in the vertical direction. For details, see
    borderInterpolate.

  - **columnBorderMode**: `integer()`.

    Pixel extrapolation method in the horizontal direction.

  ##### Return
  - **retval**: `Filter`

  @sa Scharr

  Python prototype (for reference only):
  ```python3
  createScharrFilter(srcType, dstType, dx, dy[, scale[, rowBorderMode[, columnBorderMode]]]) -> retval
  ```
  """
  @spec createScharrFilter(integer(), integer(), integer(), integer(), [{:columnBorderMode, term()} | {:rowBorderMode, term()} | {:scale, term()}] | nil) :: Evision.CUDA.Filter.t() | {:error, String.t()}
  def createScharrFilter(srcType, dstType, dx, dy, opts) when is_integer(srcType) and is_integer(dstType) and is_integer(dx) and is_integer(dy) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:columnBorderMode, :rowBorderMode, :scale])
    positional = [
      srcType: Evision.Internal.Structurise.from_struct(srcType),
      dstType: Evision.Internal.Structurise.from_struct(dstType),
      dx: Evision.Internal.Structurise.from_struct(dx),
      dy: Evision.Internal.Structurise.from_struct(dy)
    ]
    :evision_nif.cuda_createScharrFilter(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Creates a vertical or horizontal Scharr operator.

  ##### Positional Arguments
  - **srcType**: `integer()`.

    Source image type.

  - **dstType**: `integer()`.

    Destination array type.

  - **dx**: `integer()`.

    Order of the derivative x.

  - **dy**: `integer()`.

    Order of the derivative y.

  ##### Keyword Arguments
  - **scale**: `double`.

    Optional scale factor for the computed derivative values. By default, no scaling is
    applied. See getDerivKernels for details.

  - **rowBorderMode**: `integer()`.

    Pixel extrapolation method in the vertical direction. For details, see
    borderInterpolate.

  - **columnBorderMode**: `integer()`.

    Pixel extrapolation method in the horizontal direction.

  ##### Return
  - **retval**: `Filter`

  @sa Scharr

  Python prototype (for reference only):
  ```python3
  createScharrFilter(srcType, dstType, dx, dy[, scale[, rowBorderMode[, columnBorderMode]]]) -> retval
  ```
  """
  @spec createScharrFilter(integer(), integer(), integer(), integer()) :: Evision.CUDA.Filter.t() | {:error, String.t()}
  def createScharrFilter(srcType, dstType, dx, dy) when is_integer(srcType) and is_integer(dstType) and is_integer(dx) and is_integer(dy)
  do
    positional = [
      srcType: Evision.Internal.Structurise.from_struct(srcType),
      dstType: Evision.Internal.Structurise.from_struct(dstType),
      dx: Evision.Internal.Structurise.from_struct(dx),
      dy: Evision.Internal.Structurise.from_struct(dy)
    ]
    :evision_nif.cuda_createScharrFilter(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Creates a separable linear filter.

  ##### Positional Arguments
  - **srcType**: `integer()`.

    Source array type.

  - **dstType**: `integer()`.

    Destination array type.

  - **rowKernel**: `Evision.Mat`.

    Horizontal filter coefficients. Support kernels with size \\<= 32 .

  - **columnKernel**: `Evision.Mat`.

    Vertical filter coefficients. Support kernels with size \\<= 32 .

  ##### Keyword Arguments
  - **anchor**: `Point`.

    Anchor position within the kernel. Negative values mean that anchor is positioned at
    the aperture center.

  - **rowBorderMode**: `integer()`.

    Pixel extrapolation method in the vertical direction For details, see
    borderInterpolate.

  - **columnBorderMode**: `integer()`.

    Pixel extrapolation method in the horizontal direction.

  ##### Return
  - **retval**: `Filter`

  @sa sepFilter2D

  Python prototype (for reference only):
  ```python3
  createSeparableLinearFilter(srcType, dstType, rowKernel, columnKernel[, anchor[, rowBorderMode[, columnBorderMode]]]) -> retval
  ```
  #### Variant 2:
  Creates a separable linear filter.

  ##### Positional Arguments
  - **srcType**: `integer()`.

    Source array type.

  - **dstType**: `integer()`.

    Destination array type.

  - **rowKernel**: `Evision.CUDA.GpuMat.t()`.

    Horizontal filter coefficients. Support kernels with size \\<= 32 .

  - **columnKernel**: `Evision.CUDA.GpuMat.t()`.

    Vertical filter coefficients. Support kernels with size \\<= 32 .

  ##### Keyword Arguments
  - **anchor**: `Point`.

    Anchor position within the kernel. Negative values mean that anchor is positioned at
    the aperture center.

  - **rowBorderMode**: `integer()`.

    Pixel extrapolation method in the vertical direction For details, see
    borderInterpolate.

  - **columnBorderMode**: `integer()`.

    Pixel extrapolation method in the horizontal direction.

  ##### Return
  - **retval**: `Filter`

  @sa sepFilter2D

  Python prototype (for reference only):
  ```python3
  createSeparableLinearFilter(srcType, dstType, rowKernel, columnKernel[, anchor[, rowBorderMode[, columnBorderMode]]]) -> retval
  ```

  """
  @spec createSeparableLinearFilter(integer(), integer(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:anchor, term()} | {:columnBorderMode, term()} | {:rowBorderMode, term()}] | nil) :: Evision.CUDA.Filter.t() | {:error, String.t()}
  def createSeparableLinearFilter(srcType, dstType, rowKernel, columnKernel, opts) when is_integer(srcType) and is_integer(dstType) and (is_struct(rowKernel, Evision.Mat) or is_struct(rowKernel, Nx.Tensor) or is_number(rowKernel) or is_tuple(rowKernel)) and (is_struct(columnKernel, Evision.Mat) or is_struct(columnKernel, Nx.Tensor) or is_number(columnKernel) or is_tuple(columnKernel)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:anchor, :columnBorderMode, :rowBorderMode])
    positional = [
      srcType: Evision.Internal.Structurise.from_struct(srcType),
      dstType: Evision.Internal.Structurise.from_struct(dstType),
      rowKernel: Evision.Internal.Structurise.from_struct(rowKernel),
      columnKernel: Evision.Internal.Structurise.from_struct(columnKernel)
    ]
    :evision_nif.cuda_createSeparableLinearFilter(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec createSeparableLinearFilter(integer(), integer(), Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t(), [{:anchor, term()} | {:columnBorderMode, term()} | {:rowBorderMode, term()}] | nil) :: Evision.CUDA.Filter.t() | {:error, String.t()}
  def createSeparableLinearFilter(srcType, dstType, rowKernel, columnKernel, opts) when is_integer(srcType) and is_integer(dstType) and is_struct(rowKernel, Evision.CUDA.GpuMat) and is_struct(columnKernel, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:anchor, :columnBorderMode, :rowBorderMode])
    positional = [
      srcType: Evision.Internal.Structurise.from_struct(srcType),
      dstType: Evision.Internal.Structurise.from_struct(dstType),
      rowKernel: Evision.Internal.Structurise.from_struct(rowKernel),
      columnKernel: Evision.Internal.Structurise.from_struct(columnKernel)
    ]
    :evision_nif.cuda_createSeparableLinearFilter(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Creates a separable linear filter.

  ##### Positional Arguments
  - **srcType**: `integer()`.

    Source array type.

  - **dstType**: `integer()`.

    Destination array type.

  - **rowKernel**: `Evision.Mat`.

    Horizontal filter coefficients. Support kernels with size \\<= 32 .

  - **columnKernel**: `Evision.Mat`.

    Vertical filter coefficients. Support kernels with size \\<= 32 .

  ##### Keyword Arguments
  - **anchor**: `Point`.

    Anchor position within the kernel. Negative values mean that anchor is positioned at
    the aperture center.

  - **rowBorderMode**: `integer()`.

    Pixel extrapolation method in the vertical direction For details, see
    borderInterpolate.

  - **columnBorderMode**: `integer()`.

    Pixel extrapolation method in the horizontal direction.

  ##### Return
  - **retval**: `Filter`

  @sa sepFilter2D

  Python prototype (for reference only):
  ```python3
  createSeparableLinearFilter(srcType, dstType, rowKernel, columnKernel[, anchor[, rowBorderMode[, columnBorderMode]]]) -> retval
  ```
  #### Variant 2:
  Creates a separable linear filter.

  ##### Positional Arguments
  - **srcType**: `integer()`.

    Source array type.

  - **dstType**: `integer()`.

    Destination array type.

  - **rowKernel**: `Evision.CUDA.GpuMat.t()`.

    Horizontal filter coefficients. Support kernels with size \\<= 32 .

  - **columnKernel**: `Evision.CUDA.GpuMat.t()`.

    Vertical filter coefficients. Support kernels with size \\<= 32 .

  ##### Keyword Arguments
  - **anchor**: `Point`.

    Anchor position within the kernel. Negative values mean that anchor is positioned at
    the aperture center.

  - **rowBorderMode**: `integer()`.

    Pixel extrapolation method in the vertical direction For details, see
    borderInterpolate.

  - **columnBorderMode**: `integer()`.

    Pixel extrapolation method in the horizontal direction.

  ##### Return
  - **retval**: `Filter`

  @sa sepFilter2D

  Python prototype (for reference only):
  ```python3
  createSeparableLinearFilter(srcType, dstType, rowKernel, columnKernel[, anchor[, rowBorderMode[, columnBorderMode]]]) -> retval
  ```

  """
  @spec createSeparableLinearFilter(integer(), integer(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.CUDA.Filter.t() | {:error, String.t()}
  def createSeparableLinearFilter(srcType, dstType, rowKernel, columnKernel) when is_integer(srcType) and is_integer(dstType) and (is_struct(rowKernel, Evision.Mat) or is_struct(rowKernel, Nx.Tensor) or is_number(rowKernel) or is_tuple(rowKernel)) and (is_struct(columnKernel, Evision.Mat) or is_struct(columnKernel, Nx.Tensor) or is_number(columnKernel) or is_tuple(columnKernel))
  do
    positional = [
      srcType: Evision.Internal.Structurise.from_struct(srcType),
      dstType: Evision.Internal.Structurise.from_struct(dstType),
      rowKernel: Evision.Internal.Structurise.from_struct(rowKernel),
      columnKernel: Evision.Internal.Structurise.from_struct(columnKernel)
    ]
    :evision_nif.cuda_createSeparableLinearFilter(positional)
    |> to_struct()
  end
  @spec createSeparableLinearFilter(integer(), integer(), Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t()) :: Evision.CUDA.Filter.t() | {:error, String.t()}
  def createSeparableLinearFilter(srcType, dstType, rowKernel, columnKernel) when is_integer(srcType) and is_integer(dstType) and is_struct(rowKernel, Evision.CUDA.GpuMat) and is_struct(columnKernel, Evision.CUDA.GpuMat)
  do
    positional = [
      srcType: Evision.Internal.Structurise.from_struct(srcType),
      dstType: Evision.Internal.Structurise.from_struct(dstType),
      rowKernel: Evision.Internal.Structurise.from_struct(rowKernel),
      columnKernel: Evision.Internal.Structurise.from_struct(columnKernel)
    ]
    :evision_nif.cuda_createSeparableLinearFilter(positional)
    |> to_struct()
  end

  @doc """
  Creates a Sobel operator.

  ##### Positional Arguments
  - **srcType**: `integer()`.

    Source image type.

  - **dstType**: `integer()`.

    Destination array type.

  - **dx**: `integer()`.

    Derivative order in respect of x.

  - **dy**: `integer()`.

    Derivative order in respect of y.

  ##### Keyword Arguments
  - **ksize**: `integer()`.

    Size of the extended Sobel kernel. Possible values are 1, 3, 5 or 7.

  - **scale**: `double`.

    Optional scale factor for the computed derivative values. By default, no scaling is
    applied. For details, see getDerivKernels .

  - **rowBorderMode**: `integer()`.

    Pixel extrapolation method in the vertical direction. For details, see
    borderInterpolate.

  - **columnBorderMode**: `integer()`.

    Pixel extrapolation method in the horizontal direction.

  ##### Return
  - **retval**: `Filter`

  @sa Sobel

  Python prototype (for reference only):
  ```python3
  createSobelFilter(srcType, dstType, dx, dy[, ksize[, scale[, rowBorderMode[, columnBorderMode]]]]) -> retval
  ```
  """
  @spec createSobelFilter(integer(), integer(), integer(), integer(), [{:columnBorderMode, term()} | {:ksize, term()} | {:rowBorderMode, term()} | {:scale, term()}] | nil) :: Evision.CUDA.Filter.t() | {:error, String.t()}
  def createSobelFilter(srcType, dstType, dx, dy, opts) when is_integer(srcType) and is_integer(dstType) and is_integer(dx) and is_integer(dy) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:columnBorderMode, :ksize, :rowBorderMode, :scale])
    positional = [
      srcType: Evision.Internal.Structurise.from_struct(srcType),
      dstType: Evision.Internal.Structurise.from_struct(dstType),
      dx: Evision.Internal.Structurise.from_struct(dx),
      dy: Evision.Internal.Structurise.from_struct(dy)
    ]
    :evision_nif.cuda_createSobelFilter(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Creates a Sobel operator.

  ##### Positional Arguments
  - **srcType**: `integer()`.

    Source image type.

  - **dstType**: `integer()`.

    Destination array type.

  - **dx**: `integer()`.

    Derivative order in respect of x.

  - **dy**: `integer()`.

    Derivative order in respect of y.

  ##### Keyword Arguments
  - **ksize**: `integer()`.

    Size of the extended Sobel kernel. Possible values are 1, 3, 5 or 7.

  - **scale**: `double`.

    Optional scale factor for the computed derivative values. By default, no scaling is
    applied. For details, see getDerivKernels .

  - **rowBorderMode**: `integer()`.

    Pixel extrapolation method in the vertical direction. For details, see
    borderInterpolate.

  - **columnBorderMode**: `integer()`.

    Pixel extrapolation method in the horizontal direction.

  ##### Return
  - **retval**: `Filter`

  @sa Sobel

  Python prototype (for reference only):
  ```python3
  createSobelFilter(srcType, dstType, dx, dy[, ksize[, scale[, rowBorderMode[, columnBorderMode]]]]) -> retval
  ```
  """
  @spec createSobelFilter(integer(), integer(), integer(), integer()) :: Evision.CUDA.Filter.t() | {:error, String.t()}
  def createSobelFilter(srcType, dstType, dx, dy) when is_integer(srcType) and is_integer(dstType) and is_integer(dx) and is_integer(dy)
  do
    positional = [
      srcType: Evision.Internal.Structurise.from_struct(srcType),
      dstType: Evision.Internal.Structurise.from_struct(dstType),
      dx: Evision.Internal.Structurise.from_struct(dx),
      dy: Evision.Internal.Structurise.from_struct(dy)
    ]
    :evision_nif.cuda_createSobelFilter(positional)
    |> to_struct()
  end

  @doc """
  Creates StereoBM object.
  ##### Keyword Arguments
  - **numDisparities**: `integer()`.

    the disparity search range. For each pixel algorithm will find the best
    disparity from 0 (default minimum disparity) to numDisparities. The search range can then be
    shifted by changing the minimum disparity.

  - **blockSize**: `integer()`.

    the linear size of the blocks compared by the algorithm. The size should be odd
    (as the block is centered at the current pixel). Larger block size implies smoother, though less
    accurate disparity map. Smaller block size gives more detailed disparity map, but there is higher
    chance for algorithm to find a wrong correspondence.

  ##### Return
  - **retval**: `Evision.CUDA.StereoBM.t()`

  Python prototype (for reference only):
  ```python3
  createStereoBM([, numDisparities[, blockSize]]) -> retval
  ```
  """
  @spec createStereoBM([{:blockSize, term()} | {:numDisparities, term()}] | nil) :: Evision.CUDA.StereoBM.t() | {:error, String.t()}
  def createStereoBM(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:blockSize, :numDisparities])
    positional = [
    ]
    :evision_nif.cuda_createStereoBM(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Creates StereoBM object.
  ##### Keyword Arguments
  - **numDisparities**: `integer()`.

    the disparity search range. For each pixel algorithm will find the best
    disparity from 0 (default minimum disparity) to numDisparities. The search range can then be
    shifted by changing the minimum disparity.

  - **blockSize**: `integer()`.

    the linear size of the blocks compared by the algorithm. The size should be odd
    (as the block is centered at the current pixel). Larger block size implies smoother, though less
    accurate disparity map. Smaller block size gives more detailed disparity map, but there is higher
    chance for algorithm to find a wrong correspondence.

  ##### Return
  - **retval**: `Evision.CUDA.StereoBM.t()`

  Python prototype (for reference only):
  ```python3
  createStereoBM([, numDisparities[, blockSize]]) -> retval
  ```
  """
  @spec createStereoBM() :: Evision.CUDA.StereoBM.t() | {:error, String.t()}
  def createStereoBM() do
    positional = [
    ]
    :evision_nif.cuda_createStereoBM(positional)
    |> to_struct()
  end

  @doc """
  Creates StereoBeliefPropagation object.
  ##### Keyword Arguments
  - **ndisp**: `integer()`.

    Number of disparities.

  - **iters**: `integer()`.

    Number of BP iterations on each level.

  - **levels**: `integer()`.

    Number of levels.

  - **msg_type**: `integer()`.

    Type for messages. CV_16SC1 and CV_32FC1 types are supported.

  ##### Return
  - **retval**: `Evision.CUDA.StereoBeliefPropagation.t()`

  Python prototype (for reference only):
  ```python3
  createStereoBeliefPropagation([, ndisp[, iters[, levels[, msg_type]]]]) -> retval
  ```
  """
  @spec createStereoBeliefPropagation([{:iters, term()} | {:levels, term()} | {:msg_type, term()} | {:ndisp, term()}] | nil) :: Evision.CUDA.StereoBeliefPropagation.t() | {:error, String.t()}
  def createStereoBeliefPropagation(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:iters, :levels, :msg_type, :ndisp])
    positional = [
    ]
    :evision_nif.cuda_createStereoBeliefPropagation(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Creates StereoBeliefPropagation object.
  ##### Keyword Arguments
  - **ndisp**: `integer()`.

    Number of disparities.

  - **iters**: `integer()`.

    Number of BP iterations on each level.

  - **levels**: `integer()`.

    Number of levels.

  - **msg_type**: `integer()`.

    Type for messages. CV_16SC1 and CV_32FC1 types are supported.

  ##### Return
  - **retval**: `Evision.CUDA.StereoBeliefPropagation.t()`

  Python prototype (for reference only):
  ```python3
  createStereoBeliefPropagation([, ndisp[, iters[, levels[, msg_type]]]]) -> retval
  ```
  """
  @spec createStereoBeliefPropagation() :: Evision.CUDA.StereoBeliefPropagation.t() | {:error, String.t()}
  def createStereoBeliefPropagation() do
    positional = [
    ]
    :evision_nif.cuda_createStereoBeliefPropagation(positional)
    |> to_struct()
  end

  @doc """
  Creates StereoConstantSpaceBP object.
  ##### Keyword Arguments
  - **ndisp**: `integer()`.

    Number of disparities.

  - **iters**: `integer()`.

    Number of BP iterations on each level.

  - **levels**: `integer()`.

    Number of levels.

  - **nr_plane**: `integer()`.

    Number of disparity levels on the first level.

  - **msg_type**: `integer()`.

    Type for messages. CV_16SC1 and CV_32FC1 types are supported.

  ##### Return
  - **retval**: `Evision.CUDA.StereoConstantSpaceBP.t()`

  Python prototype (for reference only):
  ```python3
  createStereoConstantSpaceBP([, ndisp[, iters[, levels[, nr_plane[, msg_type]]]]]) -> retval
  ```
  """
  @spec createStereoConstantSpaceBP([{:iters, term()} | {:levels, term()} | {:msg_type, term()} | {:ndisp, term()} | {:nr_plane, term()}] | nil) :: Evision.CUDA.StereoConstantSpaceBP.t() | {:error, String.t()}
  def createStereoConstantSpaceBP(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:iters, :levels, :msg_type, :ndisp, :nr_plane])
    positional = [
    ]
    :evision_nif.cuda_createStereoConstantSpaceBP(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Creates StereoConstantSpaceBP object.
  ##### Keyword Arguments
  - **ndisp**: `integer()`.

    Number of disparities.

  - **iters**: `integer()`.

    Number of BP iterations on each level.

  - **levels**: `integer()`.

    Number of levels.

  - **nr_plane**: `integer()`.

    Number of disparity levels on the first level.

  - **msg_type**: `integer()`.

    Type for messages. CV_16SC1 and CV_32FC1 types are supported.

  ##### Return
  - **retval**: `Evision.CUDA.StereoConstantSpaceBP.t()`

  Python prototype (for reference only):
  ```python3
  createStereoConstantSpaceBP([, ndisp[, iters[, levels[, nr_plane[, msg_type]]]]]) -> retval
  ```
  """
  @spec createStereoConstantSpaceBP() :: Evision.CUDA.StereoConstantSpaceBP.t() | {:error, String.t()}
  def createStereoConstantSpaceBP() do
    positional = [
    ]
    :evision_nif.cuda_createStereoConstantSpaceBP(positional)
    |> to_struct()
  end

  @doc """
  Creates StereoSGM object.
  ##### Keyword Arguments
  - **minDisparity**: `integer()`.

    Minimum possible disparity value. Normally, it is zero but sometimes rectification algorithms can shift images, so this parameter needs to be adjusted accordingly.

  - **numDisparities**: `integer()`.

    Maximum disparity minus minimum disparity. The value must be 64, 128 or 256.

  - **p1**: `integer()`.

    The first parameter controlling the disparity smoothness.This parameter is used for the case of slanted surfaces (not fronto parallel).

  - **p2**: `integer()`.

    The second parameter controlling the disparity smoothness.This parameter is used for "solving" the depth discontinuities problem.

  - **uniquenessRatio**: `integer()`.

    Margin in percentage by which the best (minimum) computed cost function
    value should "win" the second best value to consider the found match correct. Normally, a value
    within the 5-15 range is good enough.

  - **mode**: `integer()`.

    Set it to StereoSGM::MODE_HH to run the full-scale two-pass dynamic programming algorithm.
    It will consume O(W\\*H\\*numDisparities) bytes. By default, it is set to StereoSGM::MODE_HH4.

  ##### Return
  - **retval**: `Evision.CUDA.StereoSGM.t()`

  Python prototype (for reference only):
  ```python3
  createStereoSGM([, minDisparity[, numDisparities[, P1[, P2[, uniquenessRatio[, mode]]]]]]) -> retval
  ```
  """
  @spec createStereoSGM([{:minDisparity, term()} | {:mode, term()} | {:numDisparities, term()} | {:p1, term()} | {:p2, term()} | {:uniquenessRatio, term()}] | nil) :: Evision.CUDA.StereoSGM.t() | {:error, String.t()}
  def createStereoSGM(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:minDisparity, :mode, :numDisparities, :p1, :p2, :uniquenessRatio])
    positional = [
    ]
    :evision_nif.cuda_createStereoSGM(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Creates StereoSGM object.
  ##### Keyword Arguments
  - **minDisparity**: `integer()`.

    Minimum possible disparity value. Normally, it is zero but sometimes rectification algorithms can shift images, so this parameter needs to be adjusted accordingly.

  - **numDisparities**: `integer()`.

    Maximum disparity minus minimum disparity. The value must be 64, 128 or 256.

  - **p1**: `integer()`.

    The first parameter controlling the disparity smoothness.This parameter is used for the case of slanted surfaces (not fronto parallel).

  - **p2**: `integer()`.

    The second parameter controlling the disparity smoothness.This parameter is used for "solving" the depth discontinuities problem.

  - **uniquenessRatio**: `integer()`.

    Margin in percentage by which the best (minimum) computed cost function
    value should "win" the second best value to consider the found match correct. Normally, a value
    within the 5-15 range is good enough.

  - **mode**: `integer()`.

    Set it to StereoSGM::MODE_HH to run the full-scale two-pass dynamic programming algorithm.
    It will consume O(W\\*H\\*numDisparities) bytes. By default, it is set to StereoSGM::MODE_HH4.

  ##### Return
  - **retval**: `Evision.CUDA.StereoSGM.t()`

  Python prototype (for reference only):
  ```python3
  createStereoSGM([, minDisparity[, numDisparities[, P1[, P2[, uniquenessRatio[, mode]]]]]]) -> retval
  ```
  """
  @spec createStereoSGM() :: Evision.CUDA.StereoSGM.t() | {:error, String.t()}
  def createStereoSGM() do
    positional = [
    ]
    :evision_nif.cuda_createStereoSGM(positional)
    |> to_struct()
  end

  @doc """
  Creates implementation for cuda::TemplateMatching .

  ##### Positional Arguments
  - **srcType**: `integer()`.

    Input source type. CV_32F and CV_8U depth images (1..4 channels) are supported
    for now.

  - **method**: `integer()`.

    Specifies the way to compare the template with the image.

  ##### Keyword Arguments
  - **user_block_size**: `Size`.

    You can use field user_block_size to set specific block size. If you
    leave its default value Size(0,0) then automatic estimation of block size will be used (which is
    optimized for speed). By varying user_block_size you can reduce memory requirements at the cost
    of speed.

  ##### Return
  - **retval**: `TemplateMatching`

  The following methods are supported for the CV_8U depth images for now:
  - CV_TM_SQDIFF
  - CV_TM_SQDIFF_NORMED
  - CV_TM_CCORR
  - CV_TM_CCORR_NORMED
  - CV_TM_CCOEFF
  - CV_TM_CCOEFF_NORMED

  The following methods are supported for the CV_32F images for now:
  - CV_TM_SQDIFF
  - CV_TM_CCORR

  @sa matchTemplate

  Python prototype (for reference only):
  ```python3
  createTemplateMatching(srcType, method[, user_block_size]) -> retval
  ```
  """
  @spec createTemplateMatching(integer(), integer(), [{:user_block_size, term()}] | nil) :: Evision.CUDA.TemplateMatching.t() | {:error, String.t()}
  def createTemplateMatching(srcType, method, opts) when is_integer(srcType) and is_integer(method) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:user_block_size])
    positional = [
      srcType: Evision.Internal.Structurise.from_struct(srcType),
      method: Evision.Internal.Structurise.from_struct(method)
    ]
    :evision_nif.cuda_createTemplateMatching(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Creates implementation for cuda::TemplateMatching .

  ##### Positional Arguments
  - **srcType**: `integer()`.

    Input source type. CV_32F and CV_8U depth images (1..4 channels) are supported
    for now.

  - **method**: `integer()`.

    Specifies the way to compare the template with the image.

  ##### Keyword Arguments
  - **user_block_size**: `Size`.

    You can use field user_block_size to set specific block size. If you
    leave its default value Size(0,0) then automatic estimation of block size will be used (which is
    optimized for speed). By varying user_block_size you can reduce memory requirements at the cost
    of speed.

  ##### Return
  - **retval**: `TemplateMatching`

  The following methods are supported for the CV_8U depth images for now:
  - CV_TM_SQDIFF
  - CV_TM_SQDIFF_NORMED
  - CV_TM_CCORR
  - CV_TM_CCORR_NORMED
  - CV_TM_CCOEFF
  - CV_TM_CCOEFF_NORMED

  The following methods are supported for the CV_32F images for now:
  - CV_TM_SQDIFF
  - CV_TM_CCORR

  @sa matchTemplate

  Python prototype (for reference only):
  ```python3
  createTemplateMatching(srcType, method[, user_block_size]) -> retval
  ```
  """
  @spec createTemplateMatching(integer(), integer()) :: Evision.CUDA.TemplateMatching.t() | {:error, String.t()}
  def createTemplateMatching(srcType, method) when is_integer(srcType) and is_integer(method)
  do
    positional = [
      srcType: Evision.Internal.Structurise.from_struct(srcType),
      method: Evision.Internal.Structurise.from_struct(method)
    ]
    :evision_nif.cuda_createTemplateMatching(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Converts an image from one color space to another.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source image with CV_8U , CV_16U , or CV_32F depth and 1, 3, or 4 channels.

  - **code**: `integer()`.

    Color space conversion code. For details, see cvtColor .

  ##### Keyword Arguments
  - **dcn**: `integer()`.

    Number of channels in the destination image. If the parameter is 0, the number of the
    channels is derived automatically from src and the code .

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination image.

  3-channel color spaces (like HSV, XYZ, and so on) can be stored in a 4-channel image for better
  performance.
  @sa cvtColor

  Python prototype (for reference only):
  ```python3
  cvtColor(src, code[, dst[, dcn[, stream]]]) -> dst
  ```
  #### Variant 2:
  Converts an image from one color space to another.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source image with CV_8U , CV_16U , or CV_32F depth and 1, 3, or 4 channels.

  - **code**: `integer()`.

    Color space conversion code. For details, see cvtColor .

  ##### Keyword Arguments
  - **dcn**: `integer()`.

    Number of channels in the destination image. If the parameter is 0, the number of the
    channels is derived automatically from src and the code .

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination image.

  3-channel color spaces (like HSV, XYZ, and so on) can be stored in a 4-channel image for better
  performance.
  @sa cvtColor

  Python prototype (for reference only):
  ```python3
  cvtColor(src, code[, dst[, dcn[, stream]]]) -> dst
  ```

  """
  @spec cvtColor(Evision.Mat.maybe_mat_in(), integer(), [{:dcn, term()} | {:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def cvtColor(src, code, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_integer(code) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:dcn, :stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      code: Evision.Internal.Structurise.from_struct(code)
    ]
    :evision_nif.cuda_cvtColor(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec cvtColor(Evision.CUDA.GpuMat.t(), integer(), [{:dcn, term()} | {:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def cvtColor(src, code, opts) when is_struct(src, Evision.CUDA.GpuMat) and is_integer(code) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:dcn, :stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      code: Evision.Internal.Structurise.from_struct(code)
    ]
    :evision_nif.cuda_cvtColor(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Converts an image from one color space to another.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source image with CV_8U , CV_16U , or CV_32F depth and 1, 3, or 4 channels.

  - **code**: `integer()`.

    Color space conversion code. For details, see cvtColor .

  ##### Keyword Arguments
  - **dcn**: `integer()`.

    Number of channels in the destination image. If the parameter is 0, the number of the
    channels is derived automatically from src and the code .

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination image.

  3-channel color spaces (like HSV, XYZ, and so on) can be stored in a 4-channel image for better
  performance.
  @sa cvtColor

  Python prototype (for reference only):
  ```python3
  cvtColor(src, code[, dst[, dcn[, stream]]]) -> dst
  ```
  #### Variant 2:
  Converts an image from one color space to another.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source image with CV_8U , CV_16U , or CV_32F depth and 1, 3, or 4 channels.

  - **code**: `integer()`.

    Color space conversion code. For details, see cvtColor .

  ##### Keyword Arguments
  - **dcn**: `integer()`.

    Number of channels in the destination image. If the parameter is 0, the number of the
    channels is derived automatically from src and the code .

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination image.

  3-channel color spaces (like HSV, XYZ, and so on) can be stored in a 4-channel image for better
  performance.
  @sa cvtColor

  Python prototype (for reference only):
  ```python3
  cvtColor(src, code[, dst[, dcn[, stream]]]) -> dst
  ```

  """
  @spec cvtColor(Evision.Mat.maybe_mat_in(), integer()) :: Evision.Mat.t() | {:error, String.t()}
  def cvtColor(src, code) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_integer(code)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      code: Evision.Internal.Structurise.from_struct(code)
    ]
    :evision_nif.cuda_cvtColor(positional)
    |> to_struct()
  end
  @spec cvtColor(Evision.CUDA.GpuMat.t(), integer()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def cvtColor(src, code) when is_struct(src, Evision.CUDA.GpuMat) and is_integer(code)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      code: Evision.Internal.Structurise.from_struct(code)
    ]
    :evision_nif.cuda_cvtColor(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Converts an image from Bayer pattern to RGB or grayscale.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source image (8-bit or 16-bit single channel).

  - **code**: `integer()`.

    Color space conversion code (see the description below).

  ##### Keyword Arguments
  - **dcn**: `integer()`.

    Number of channels in the destination image. If the parameter is 0, the number of the
    channels is derived automatically from src and the code .

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination image.

  The function can do the following transformations:
  - Demosaicing using bilinear interpolation

  > -   COLOR_BayerBG2GRAY , COLOR_BayerGB2GRAY , COLOR_BayerRG2GRAY , COLOR_BayerGR2GRAY
  > -   COLOR_BayerBG2BGR , COLOR_BayerGB2BGR , COLOR_BayerRG2BGR , COLOR_BayerGR2BGR
  - Demosaicing using Malvar-He-Cutler algorithm (@cite MHT2011)

  > -   COLOR_BayerBG2GRAY_MHT , COLOR_BayerGB2GRAY_MHT , COLOR_BayerRG2GRAY_MHT ,
  >     COLOR_BayerGR2GRAY_MHT
  > -   COLOR_BayerBG2BGR_MHT , COLOR_BayerGB2BGR_MHT , COLOR_BayerRG2BGR_MHT ,
  >     COLOR_BayerGR2BGR_MHT
  @sa cvtColor

  Python prototype (for reference only):
  ```python3
  demosaicing(src, code[, dst[, dcn[, stream]]]) -> dst
  ```
  #### Variant 2:
  Converts an image from Bayer pattern to RGB or grayscale.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source image (8-bit or 16-bit single channel).

  - **code**: `integer()`.

    Color space conversion code (see the description below).

  ##### Keyword Arguments
  - **dcn**: `integer()`.

    Number of channels in the destination image. If the parameter is 0, the number of the
    channels is derived automatically from src and the code .

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination image.

  The function can do the following transformations:
  - Demosaicing using bilinear interpolation

  > -   COLOR_BayerBG2GRAY , COLOR_BayerGB2GRAY , COLOR_BayerRG2GRAY , COLOR_BayerGR2GRAY
  > -   COLOR_BayerBG2BGR , COLOR_BayerGB2BGR , COLOR_BayerRG2BGR , COLOR_BayerGR2BGR
  - Demosaicing using Malvar-He-Cutler algorithm (@cite MHT2011)

  > -   COLOR_BayerBG2GRAY_MHT , COLOR_BayerGB2GRAY_MHT , COLOR_BayerRG2GRAY_MHT ,
  >     COLOR_BayerGR2GRAY_MHT
  > -   COLOR_BayerBG2BGR_MHT , COLOR_BayerGB2BGR_MHT , COLOR_BayerRG2BGR_MHT ,
  >     COLOR_BayerGR2BGR_MHT
  @sa cvtColor

  Python prototype (for reference only):
  ```python3
  demosaicing(src, code[, dst[, dcn[, stream]]]) -> dst
  ```

  """
  @spec demosaicing(Evision.Mat.maybe_mat_in(), integer(), [{:dcn, term()} | {:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def demosaicing(src, code, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_integer(code) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:dcn, :stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      code: Evision.Internal.Structurise.from_struct(code)
    ]
    :evision_nif.cuda_demosaicing(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec demosaicing(Evision.CUDA.GpuMat.t(), integer(), [{:dcn, term()} | {:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def demosaicing(src, code, opts) when is_struct(src, Evision.CUDA.GpuMat) and is_integer(code) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:dcn, :stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      code: Evision.Internal.Structurise.from_struct(code)
    ]
    :evision_nif.cuda_demosaicing(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Converts an image from Bayer pattern to RGB or grayscale.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source image (8-bit or 16-bit single channel).

  - **code**: `integer()`.

    Color space conversion code (see the description below).

  ##### Keyword Arguments
  - **dcn**: `integer()`.

    Number of channels in the destination image. If the parameter is 0, the number of the
    channels is derived automatically from src and the code .

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination image.

  The function can do the following transformations:
  - Demosaicing using bilinear interpolation

  > -   COLOR_BayerBG2GRAY , COLOR_BayerGB2GRAY , COLOR_BayerRG2GRAY , COLOR_BayerGR2GRAY
  > -   COLOR_BayerBG2BGR , COLOR_BayerGB2BGR , COLOR_BayerRG2BGR , COLOR_BayerGR2BGR
  - Demosaicing using Malvar-He-Cutler algorithm (@cite MHT2011)

  > -   COLOR_BayerBG2GRAY_MHT , COLOR_BayerGB2GRAY_MHT , COLOR_BayerRG2GRAY_MHT ,
  >     COLOR_BayerGR2GRAY_MHT
  > -   COLOR_BayerBG2BGR_MHT , COLOR_BayerGB2BGR_MHT , COLOR_BayerRG2BGR_MHT ,
  >     COLOR_BayerGR2BGR_MHT
  @sa cvtColor

  Python prototype (for reference only):
  ```python3
  demosaicing(src, code[, dst[, dcn[, stream]]]) -> dst
  ```
  #### Variant 2:
  Converts an image from Bayer pattern to RGB or grayscale.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source image (8-bit or 16-bit single channel).

  - **code**: `integer()`.

    Color space conversion code (see the description below).

  ##### Keyword Arguments
  - **dcn**: `integer()`.

    Number of channels in the destination image. If the parameter is 0, the number of the
    channels is derived automatically from src and the code .

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination image.

  The function can do the following transformations:
  - Demosaicing using bilinear interpolation

  > -   COLOR_BayerBG2GRAY , COLOR_BayerGB2GRAY , COLOR_BayerRG2GRAY , COLOR_BayerGR2GRAY
  > -   COLOR_BayerBG2BGR , COLOR_BayerGB2BGR , COLOR_BayerRG2BGR , COLOR_BayerGR2BGR
  - Demosaicing using Malvar-He-Cutler algorithm (@cite MHT2011)

  > -   COLOR_BayerBG2GRAY_MHT , COLOR_BayerGB2GRAY_MHT , COLOR_BayerRG2GRAY_MHT ,
  >     COLOR_BayerGR2GRAY_MHT
  > -   COLOR_BayerBG2BGR_MHT , COLOR_BayerGB2BGR_MHT , COLOR_BayerRG2BGR_MHT ,
  >     COLOR_BayerGR2BGR_MHT
  @sa cvtColor

  Python prototype (for reference only):
  ```python3
  demosaicing(src, code[, dst[, dcn[, stream]]]) -> dst
  ```

  """
  @spec demosaicing(Evision.Mat.maybe_mat_in(), integer()) :: Evision.Mat.t() | {:error, String.t()}
  def demosaicing(src, code) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_integer(code)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      code: Evision.Internal.Structurise.from_struct(code)
    ]
    :evision_nif.cuda_demosaicing(positional)
    |> to_struct()
  end
  @spec demosaicing(Evision.CUDA.GpuMat.t(), integer()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def demosaicing(src, code) when is_struct(src, Evision.CUDA.GpuMat) and is_integer(code)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      code: Evision.Internal.Structurise.from_struct(code)
    ]
    :evision_nif.cuda_demosaicing(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Performs a forward or inverse discrete Fourier transform (1D or 2D) of the floating point matrix.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source matrix (real or complex).

  - **dft_size**: `Size`.

    Size of a discrete Fourier transform.

  ##### Keyword Arguments
  - **flags**: `integer()`.

    Optional flags:
    - **DFT_ROWS** transforms each individual row of the source matrix.
    - **DFT_SCALE** scales the result: divide it by the number of elements in the transform
      (obtained from dft_size ).
    - **DFT_INVERSE** inverts DFT. Use for complex-complex cases (real-complex and complex-real
      cases are always forward and inverse, respectively).
    - **DFT_COMPLEX_INPUT** Specifies that input is complex input with 2 channels.
    - **DFT_REAL_OUTPUT** specifies the output as real. The source matrix is the result of
      real-complex transform, so the destination matrix must be real.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination matrix (real or complex).

  Use to handle real matrices ( CV32FC1 ) and complex matrices in the interleaved format ( CV32FC2 ).
  The source matrix should be continuous, otherwise reallocation and data copying is performed. The
  function chooses an operation mode depending on the flags, size, and channel count of the source
  matrix:
  - If the source matrix is complex and the output is not specified as real, the destination
    matrix is complex and has the dft_size size and CV_32FC2 type. The destination matrix
    contains a full result of the DFT (forward or inverse).

  - If the source matrix is complex and the output is specified as real, the function assumes that
    its input is the result of the forward transform (see the next item). The destination matrix
    has the dft_size size and CV_32FC1 type. It contains the result of the inverse DFT.

  - If the source matrix is real (its type is CV_32FC1 ), forward DFT is performed. The result of
    the DFT is packed into complex ( CV_32FC2 ) matrix. So, the width of the destination matrix
    is dft_size.width / 2 + 1 . But if the source is a single column, the height is reduced
    instead of the width.

  @sa dft

  Python prototype (for reference only):
  ```python3
  dft(src, dft_size[, dst[, flags[, stream]]]) -> dst
  ```
  #### Variant 2:
  Performs a forward or inverse discrete Fourier transform (1D or 2D) of the floating point matrix.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source matrix (real or complex).

  - **dft_size**: `Size`.

    Size of a discrete Fourier transform.

  ##### Keyword Arguments
  - **flags**: `integer()`.

    Optional flags:
    - **DFT_ROWS** transforms each individual row of the source matrix.
    - **DFT_SCALE** scales the result: divide it by the number of elements in the transform
      (obtained from dft_size ).
    - **DFT_INVERSE** inverts DFT. Use for complex-complex cases (real-complex and complex-real
      cases are always forward and inverse, respectively).
    - **DFT_COMPLEX_INPUT** Specifies that input is complex input with 2 channels.
    - **DFT_REAL_OUTPUT** specifies the output as real. The source matrix is the result of
      real-complex transform, so the destination matrix must be real.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination matrix (real or complex).

  Use to handle real matrices ( CV32FC1 ) and complex matrices in the interleaved format ( CV32FC2 ).
  The source matrix should be continuous, otherwise reallocation and data copying is performed. The
  function chooses an operation mode depending on the flags, size, and channel count of the source
  matrix:
  - If the source matrix is complex and the output is not specified as real, the destination
    matrix is complex and has the dft_size size and CV_32FC2 type. The destination matrix
    contains a full result of the DFT (forward or inverse).

  - If the source matrix is complex and the output is specified as real, the function assumes that
    its input is the result of the forward transform (see the next item). The destination matrix
    has the dft_size size and CV_32FC1 type. It contains the result of the inverse DFT.

  - If the source matrix is real (its type is CV_32FC1 ), forward DFT is performed. The result of
    the DFT is packed into complex ( CV_32FC2 ) matrix. So, the width of the destination matrix
    is dft_size.width / 2 + 1 . But if the source is a single column, the height is reduced
    instead of the width.

  @sa dft

  Python prototype (for reference only):
  ```python3
  dft(src, dft_size[, dst[, flags[, stream]]]) -> dst
  ```

  """
  @spec dft(Evision.Mat.maybe_mat_in(), {number(), number()}, [{:flags, term()} | {:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def dft(src, dft_size, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_tuple(dft_size) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:flags, :stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      dft_size: Evision.Internal.Structurise.from_struct(dft_size)
    ]
    :evision_nif.cuda_dft(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec dft(Evision.CUDA.GpuMat.t(), {number(), number()}, [{:flags, term()} | {:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def dft(src, dft_size, opts) when is_struct(src, Evision.CUDA.GpuMat) and is_tuple(dft_size) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:flags, :stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      dft_size: Evision.Internal.Structurise.from_struct(dft_size)
    ]
    :evision_nif.cuda_dft(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Performs a forward or inverse discrete Fourier transform (1D or 2D) of the floating point matrix.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source matrix (real or complex).

  - **dft_size**: `Size`.

    Size of a discrete Fourier transform.

  ##### Keyword Arguments
  - **flags**: `integer()`.

    Optional flags:
    - **DFT_ROWS** transforms each individual row of the source matrix.
    - **DFT_SCALE** scales the result: divide it by the number of elements in the transform
      (obtained from dft_size ).
    - **DFT_INVERSE** inverts DFT. Use for complex-complex cases (real-complex and complex-real
      cases are always forward and inverse, respectively).
    - **DFT_COMPLEX_INPUT** Specifies that input is complex input with 2 channels.
    - **DFT_REAL_OUTPUT** specifies the output as real. The source matrix is the result of
      real-complex transform, so the destination matrix must be real.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination matrix (real or complex).

  Use to handle real matrices ( CV32FC1 ) and complex matrices in the interleaved format ( CV32FC2 ).
  The source matrix should be continuous, otherwise reallocation and data copying is performed. The
  function chooses an operation mode depending on the flags, size, and channel count of the source
  matrix:
  - If the source matrix is complex and the output is not specified as real, the destination
    matrix is complex and has the dft_size size and CV_32FC2 type. The destination matrix
    contains a full result of the DFT (forward or inverse).

  - If the source matrix is complex and the output is specified as real, the function assumes that
    its input is the result of the forward transform (see the next item). The destination matrix
    has the dft_size size and CV_32FC1 type. It contains the result of the inverse DFT.

  - If the source matrix is real (its type is CV_32FC1 ), forward DFT is performed. The result of
    the DFT is packed into complex ( CV_32FC2 ) matrix. So, the width of the destination matrix
    is dft_size.width / 2 + 1 . But if the source is a single column, the height is reduced
    instead of the width.

  @sa dft

  Python prototype (for reference only):
  ```python3
  dft(src, dft_size[, dst[, flags[, stream]]]) -> dst
  ```
  #### Variant 2:
  Performs a forward or inverse discrete Fourier transform (1D or 2D) of the floating point matrix.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source matrix (real or complex).

  - **dft_size**: `Size`.

    Size of a discrete Fourier transform.

  ##### Keyword Arguments
  - **flags**: `integer()`.

    Optional flags:
    - **DFT_ROWS** transforms each individual row of the source matrix.
    - **DFT_SCALE** scales the result: divide it by the number of elements in the transform
      (obtained from dft_size ).
    - **DFT_INVERSE** inverts DFT. Use for complex-complex cases (real-complex and complex-real
      cases are always forward and inverse, respectively).
    - **DFT_COMPLEX_INPUT** Specifies that input is complex input with 2 channels.
    - **DFT_REAL_OUTPUT** specifies the output as real. The source matrix is the result of
      real-complex transform, so the destination matrix must be real.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination matrix (real or complex).

  Use to handle real matrices ( CV32FC1 ) and complex matrices in the interleaved format ( CV32FC2 ).
  The source matrix should be continuous, otherwise reallocation and data copying is performed. The
  function chooses an operation mode depending on the flags, size, and channel count of the source
  matrix:
  - If the source matrix is complex and the output is not specified as real, the destination
    matrix is complex and has the dft_size size and CV_32FC2 type. The destination matrix
    contains a full result of the DFT (forward or inverse).

  - If the source matrix is complex and the output is specified as real, the function assumes that
    its input is the result of the forward transform (see the next item). The destination matrix
    has the dft_size size and CV_32FC1 type. It contains the result of the inverse DFT.

  - If the source matrix is real (its type is CV_32FC1 ), forward DFT is performed. The result of
    the DFT is packed into complex ( CV_32FC2 ) matrix. So, the width of the destination matrix
    is dft_size.width / 2 + 1 . But if the source is a single column, the height is reduced
    instead of the width.

  @sa dft

  Python prototype (for reference only):
  ```python3
  dft(src, dft_size[, dst[, flags[, stream]]]) -> dst
  ```

  """
  @spec dft(Evision.Mat.maybe_mat_in(), {number(), number()}) :: Evision.Mat.t() | {:error, String.t()}
  def dft(src, dft_size) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_tuple(dft_size)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      dft_size: Evision.Internal.Structurise.from_struct(dft_size)
    ]
    :evision_nif.cuda_dft(positional)
    |> to_struct()
  end
  @spec dft(Evision.CUDA.GpuMat.t(), {number(), number()}) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def dft(src, dft_size) when is_struct(src, Evision.CUDA.GpuMat) and is_tuple(dft_size)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      dft_size: Evision.Internal.Structurise.from_struct(dft_size)
    ]
    :evision_nif.cuda_dft(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Computes a matrix-matrix or matrix-scalar division.

  ##### Positional Arguments
  - **src1**: `Evision.Mat`.

    First source matrix or a scalar.

  - **src2**: `Evision.Mat`.

    Second source matrix or scalar.

  ##### Keyword Arguments
  - **scale**: `double`.

    Optional scale factor.

  - **dtype**: `integer()`.

    Optional depth of the output array.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination matrix that has the same size and number of channels as the input array(s).
    The depth is defined by dtype or src1 depth.

  This function, in contrast to divide, uses a round-down rounding mode.
  @sa divide

  Python prototype (for reference only):
  ```python3
  divide(src1, src2[, dst[, scale[, dtype[, stream]]]]) -> dst
  ```
  #### Variant 2:
  Computes a matrix-matrix or matrix-scalar division.

  ##### Positional Arguments
  - **src1**: `Evision.CUDA.GpuMat.t()`.

    First source matrix or a scalar.

  - **src2**: `Evision.CUDA.GpuMat.t()`.

    Second source matrix or scalar.

  ##### Keyword Arguments
  - **scale**: `double`.

    Optional scale factor.

  - **dtype**: `integer()`.

    Optional depth of the output array.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination matrix that has the same size and number of channels as the input array(s).
    The depth is defined by dtype or src1 depth.

  This function, in contrast to divide, uses a round-down rounding mode.
  @sa divide

  Python prototype (for reference only):
  ```python3
  divide(src1, src2[, dst[, scale[, dtype[, stream]]]]) -> dst
  ```

  """
  @spec divide(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:dtype, term()} | {:scale, term()} | {:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def divide(src1, src2, opts) when (is_struct(src1, Evision.Mat) or is_struct(src1, Nx.Tensor) or is_number(src1) or is_tuple(src1)) and (is_struct(src2, Evision.Mat) or is_struct(src2, Nx.Tensor) or is_number(src2) or is_tuple(src2)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:dtype, :scale, :stream])
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1),
      src2: Evision.Internal.Structurise.from_struct(src2)
    ]
    :evision_nif.cuda_divide(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec divide(Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t(), [{:dtype, term()} | {:scale, term()} | {:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def divide(src1, src2, opts) when is_struct(src1, Evision.CUDA.GpuMat) and is_struct(src2, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:dtype, :scale, :stream])
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1),
      src2: Evision.Internal.Structurise.from_struct(src2)
    ]
    :evision_nif.cuda_divide(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Computes a matrix-matrix or matrix-scalar division.

  ##### Positional Arguments
  - **src1**: `Evision.Mat`.

    First source matrix or a scalar.

  - **src2**: `Evision.Mat`.

    Second source matrix or scalar.

  ##### Keyword Arguments
  - **scale**: `double`.

    Optional scale factor.

  - **dtype**: `integer()`.

    Optional depth of the output array.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination matrix that has the same size and number of channels as the input array(s).
    The depth is defined by dtype or src1 depth.

  This function, in contrast to divide, uses a round-down rounding mode.
  @sa divide

  Python prototype (for reference only):
  ```python3
  divide(src1, src2[, dst[, scale[, dtype[, stream]]]]) -> dst
  ```
  #### Variant 2:
  Computes a matrix-matrix or matrix-scalar division.

  ##### Positional Arguments
  - **src1**: `Evision.CUDA.GpuMat.t()`.

    First source matrix or a scalar.

  - **src2**: `Evision.CUDA.GpuMat.t()`.

    Second source matrix or scalar.

  ##### Keyword Arguments
  - **scale**: `double`.

    Optional scale factor.

  - **dtype**: `integer()`.

    Optional depth of the output array.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination matrix that has the same size and number of channels as the input array(s).
    The depth is defined by dtype or src1 depth.

  This function, in contrast to divide, uses a round-down rounding mode.
  @sa divide

  Python prototype (for reference only):
  ```python3
  divide(src1, src2[, dst[, scale[, dtype[, stream]]]]) -> dst
  ```

  """
  @spec divide(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def divide(src1, src2) when (is_struct(src1, Evision.Mat) or is_struct(src1, Nx.Tensor) or is_number(src1) or is_tuple(src1)) and (is_struct(src2, Evision.Mat) or is_struct(src2, Nx.Tensor) or is_number(src2) or is_tuple(src2))
  do
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1),
      src2: Evision.Internal.Structurise.from_struct(src2)
    ]
    :evision_nif.cuda_divide(positional)
    |> to_struct()
  end
  @spec divide(Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def divide(src1, src2) when is_struct(src1, Evision.CUDA.GpuMat) and is_struct(src2, Evision.CUDA.GpuMat)
  do
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1),
      src2: Evision.Internal.Structurise.from_struct(src2)
    ]
    :evision_nif.cuda_divide(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Colors a disparity image.

  ##### Positional Arguments
  - **src_disp**: `Evision.Mat`.

    Input single-channel 8-bit unsigned, 16-bit signed, 32-bit signed or 32-bit
    floating-point disparity image. If 16-bit signed format is used, the values are assumed to have no
    fractional bits.

  - **ndisp**: `integer()`.

    Number of disparities.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst_disp**: `Evision.Mat.t()`.

    Output disparity image. It has the same size as src_disp. The type is CV_8UC4
    in BGRA format (alpha = 255).

  This function draws a colored disparity map by converting disparity values from [0..ndisp) interval
  first to HSV color space (where different disparity values correspond to different hues) and then
  converting the pixels to RGB for visualization.

  Python prototype (for reference only):
  ```python3
  drawColorDisp(src_disp, ndisp[, dst_disp[, stream]]) -> dst_disp
  ```
  #### Variant 2:
  Colors a disparity image.

  ##### Positional Arguments
  - **src_disp**: `Evision.CUDA.GpuMat.t()`.

    Input single-channel 8-bit unsigned, 16-bit signed, 32-bit signed or 32-bit
    floating-point disparity image. If 16-bit signed format is used, the values are assumed to have no
    fractional bits.

  - **ndisp**: `integer()`.

    Number of disparities.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst_disp**: `Evision.CUDA.GpuMat.t()`.

    Output disparity image. It has the same size as src_disp. The type is CV_8UC4
    in BGRA format (alpha = 255).

  This function draws a colored disparity map by converting disparity values from [0..ndisp) interval
  first to HSV color space (where different disparity values correspond to different hues) and then
  converting the pixels to RGB for visualization.

  Python prototype (for reference only):
  ```python3
  drawColorDisp(src_disp, ndisp[, dst_disp[, stream]]) -> dst_disp
  ```

  """
  @spec drawColorDisp(Evision.Mat.maybe_mat_in(), integer(), [{:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def drawColorDisp(src_disp, ndisp, opts) when (is_struct(src_disp, Evision.Mat) or is_struct(src_disp, Nx.Tensor) or is_number(src_disp) or is_tuple(src_disp)) and is_integer(ndisp) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src_disp: Evision.Internal.Structurise.from_struct(src_disp),
      ndisp: Evision.Internal.Structurise.from_struct(ndisp)
    ]
    :evision_nif.cuda_drawColorDisp(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec drawColorDisp(Evision.CUDA.GpuMat.t(), integer(), [{:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def drawColorDisp(src_disp, ndisp, opts) when is_struct(src_disp, Evision.CUDA.GpuMat) and is_integer(ndisp) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src_disp: Evision.Internal.Structurise.from_struct(src_disp),
      ndisp: Evision.Internal.Structurise.from_struct(ndisp)
    ]
    :evision_nif.cuda_drawColorDisp(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Colors a disparity image.

  ##### Positional Arguments
  - **src_disp**: `Evision.Mat`.

    Input single-channel 8-bit unsigned, 16-bit signed, 32-bit signed or 32-bit
    floating-point disparity image. If 16-bit signed format is used, the values are assumed to have no
    fractional bits.

  - **ndisp**: `integer()`.

    Number of disparities.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst_disp**: `Evision.Mat.t()`.

    Output disparity image. It has the same size as src_disp. The type is CV_8UC4
    in BGRA format (alpha = 255).

  This function draws a colored disparity map by converting disparity values from [0..ndisp) interval
  first to HSV color space (where different disparity values correspond to different hues) and then
  converting the pixels to RGB for visualization.

  Python prototype (for reference only):
  ```python3
  drawColorDisp(src_disp, ndisp[, dst_disp[, stream]]) -> dst_disp
  ```
  #### Variant 2:
  Colors a disparity image.

  ##### Positional Arguments
  - **src_disp**: `Evision.CUDA.GpuMat.t()`.

    Input single-channel 8-bit unsigned, 16-bit signed, 32-bit signed or 32-bit
    floating-point disparity image. If 16-bit signed format is used, the values are assumed to have no
    fractional bits.

  - **ndisp**: `integer()`.

    Number of disparities.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst_disp**: `Evision.CUDA.GpuMat.t()`.

    Output disparity image. It has the same size as src_disp. The type is CV_8UC4
    in BGRA format (alpha = 255).

  This function draws a colored disparity map by converting disparity values from [0..ndisp) interval
  first to HSV color space (where different disparity values correspond to different hues) and then
  converting the pixels to RGB for visualization.

  Python prototype (for reference only):
  ```python3
  drawColorDisp(src_disp, ndisp[, dst_disp[, stream]]) -> dst_disp
  ```

  """
  @spec drawColorDisp(Evision.Mat.maybe_mat_in(), integer()) :: Evision.Mat.t() | {:error, String.t()}
  def drawColorDisp(src_disp, ndisp) when (is_struct(src_disp, Evision.Mat) or is_struct(src_disp, Nx.Tensor) or is_number(src_disp) or is_tuple(src_disp)) and is_integer(ndisp)
  do
    positional = [
      src_disp: Evision.Internal.Structurise.from_struct(src_disp),
      ndisp: Evision.Internal.Structurise.from_struct(ndisp)
    ]
    :evision_nif.cuda_drawColorDisp(positional)
    |> to_struct()
  end
  @spec drawColorDisp(Evision.CUDA.GpuMat.t(), integer()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def drawColorDisp(src_disp, ndisp) when is_struct(src_disp, Evision.CUDA.GpuMat) and is_integer(ndisp)
  do
    positional = [
      src_disp: Evision.Internal.Structurise.from_struct(src_disp),
      ndisp: Evision.Internal.Structurise.from_struct(ndisp)
    ]
    :evision_nif.cuda_drawColorDisp(positional)
    |> to_struct()
  end

  @doc """
  Ensures that the size of a matrix is big enough and the matrix has a proper type.

  ##### Positional Arguments
  - **rows**: `integer()`.

    Minimum desired number of rows.

  - **cols**: `integer()`.

    Minimum desired number of columns.

  - **type**: `integer()`.

    Desired matrix type.

  ##### Return
  - **arr**: `Evision.Mat.t()`.

    Destination matrix.

  The function does not reallocate memory if the matrix has proper attributes already.

  Python prototype (for reference only):
  ```python3
  ensureSizeIsEnough(rows, cols, type[, arr]) -> arr
  ```
  """
  @spec ensureSizeIsEnough(integer(), integer(), integer(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def ensureSizeIsEnough(rows, cols, type, opts) when is_integer(rows) and is_integer(cols) and is_integer(type) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      rows: Evision.Internal.Structurise.from_struct(rows),
      cols: Evision.Internal.Structurise.from_struct(cols),
      type: Evision.Internal.Structurise.from_struct(type)
    ]
    :evision_nif.cuda_ensureSizeIsEnough(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Ensures that the size of a matrix is big enough and the matrix has a proper type.

  ##### Positional Arguments
  - **rows**: `integer()`.

    Minimum desired number of rows.

  - **cols**: `integer()`.

    Minimum desired number of columns.

  - **type**: `integer()`.

    Desired matrix type.

  ##### Return
  - **arr**: `Evision.Mat.t()`.

    Destination matrix.

  The function does not reallocate memory if the matrix has proper attributes already.

  Python prototype (for reference only):
  ```python3
  ensureSizeIsEnough(rows, cols, type[, arr]) -> arr
  ```
  """
  @spec ensureSizeIsEnough(integer(), integer(), integer()) :: Evision.Mat.t() | {:error, String.t()}
  def ensureSizeIsEnough(rows, cols, type) when is_integer(rows) and is_integer(cols) and is_integer(type)
  do
    positional = [
      rows: Evision.Internal.Structurise.from_struct(rows),
      cols: Evision.Internal.Structurise.from_struct(cols),
      type: Evision.Internal.Structurise.from_struct(type)
    ]
    :evision_nif.cuda_ensureSizeIsEnough(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Equalizes the histogram of a grayscale image.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source image with CV_8UC1 type.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination image.

  @sa equalizeHist

  Python prototype (for reference only):
  ```python3
  equalizeHist(src[, dst[, stream]]) -> dst
  ```
  #### Variant 2:
  Equalizes the histogram of a grayscale image.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source image with CV_8UC1 type.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination image.

  @sa equalizeHist

  Python prototype (for reference only):
  ```python3
  equalizeHist(src[, dst[, stream]]) -> dst
  ```

  """
  @spec equalizeHist(Evision.Mat.maybe_mat_in(), [{:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def equalizeHist(src, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_equalizeHist(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec equalizeHist(Evision.CUDA.GpuMat.t(), [{:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def equalizeHist(src, opts) when is_struct(src, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_equalizeHist(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Equalizes the histogram of a grayscale image.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source image with CV_8UC1 type.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination image.

  @sa equalizeHist

  Python prototype (for reference only):
  ```python3
  equalizeHist(src[, dst[, stream]]) -> dst
  ```
  #### Variant 2:
  Equalizes the histogram of a grayscale image.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source image with CV_8UC1 type.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination image.

  @sa equalizeHist

  Python prototype (for reference only):
  ```python3
  equalizeHist(src[, dst[, stream]]) -> dst
  ```

  """
  @spec equalizeHist(Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def equalizeHist(src) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_equalizeHist(positional)
    |> to_struct()
  end
  @spec equalizeHist(Evision.CUDA.GpuMat.t()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def equalizeHist(src) when is_struct(src, Evision.CUDA.GpuMat)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_equalizeHist(positional)
    |> to_struct()
  end

  @doc """
  Computes levels with even distribution.

  ##### Positional Arguments
  - **nLevels**: `integer()`.

    Number of computed levels. nLevels must be at least 2.

  - **lowerLevel**: `integer()`.

    Lower boundary value of the lowest level.

  - **upperLevel**: `integer()`.

    Upper boundary value of the greatest level.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **levels**: `Evision.Mat.t()`.

    Destination array. levels has 1 row, nLevels columns, and the CV_32SC1 type.

  Python prototype (for reference only):
  ```python3
  evenLevels(nLevels, lowerLevel, upperLevel[, levels[, stream]]) -> levels
  ```
  """
  @spec evenLevels(integer(), integer(), integer(), [{:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def evenLevels(nLevels, lowerLevel, upperLevel, opts) when is_integer(nLevels) and is_integer(lowerLevel) and is_integer(upperLevel) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      nLevels: Evision.Internal.Structurise.from_struct(nLevels),
      lowerLevel: Evision.Internal.Structurise.from_struct(lowerLevel),
      upperLevel: Evision.Internal.Structurise.from_struct(upperLevel)
    ]
    :evision_nif.cuda_evenLevels(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Computes levels with even distribution.

  ##### Positional Arguments
  - **nLevels**: `integer()`.

    Number of computed levels. nLevels must be at least 2.

  - **lowerLevel**: `integer()`.

    Lower boundary value of the lowest level.

  - **upperLevel**: `integer()`.

    Upper boundary value of the greatest level.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **levels**: `Evision.Mat.t()`.

    Destination array. levels has 1 row, nLevels columns, and the CV_32SC1 type.

  Python prototype (for reference only):
  ```python3
  evenLevels(nLevels, lowerLevel, upperLevel[, levels[, stream]]) -> levels
  ```
  """
  @spec evenLevels(integer(), integer(), integer()) :: Evision.Mat.t() | {:error, String.t()}
  def evenLevels(nLevels, lowerLevel, upperLevel) when is_integer(nLevels) and is_integer(lowerLevel) and is_integer(upperLevel)
  do
    positional = [
      nLevels: Evision.Internal.Structurise.from_struct(nLevels),
      lowerLevel: Evision.Internal.Structurise.from_struct(lowerLevel),
      upperLevel: Evision.Internal.Structurise.from_struct(upperLevel)
    ]
    :evision_nif.cuda_evenLevels(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Computes an exponent of each matrix element.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source matrix.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination matrix with the same size and type as src .

  @sa exp

  Python prototype (for reference only):
  ```python3
  exp(src[, dst[, stream]]) -> dst
  ```
  #### Variant 2:
  Computes an exponent of each matrix element.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source matrix.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination matrix with the same size and type as src .

  @sa exp

  Python prototype (for reference only):
  ```python3
  exp(src[, dst[, stream]]) -> dst
  ```

  """
  @spec exp(Evision.Mat.maybe_mat_in(), [{:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def exp(src, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_exp(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec exp(Evision.CUDA.GpuMat.t(), [{:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def exp(src, opts) when is_struct(src, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_exp(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Computes an exponent of each matrix element.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source matrix.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination matrix with the same size and type as src .

  @sa exp

  Python prototype (for reference only):
  ```python3
  exp(src[, dst[, stream]]) -> dst
  ```
  #### Variant 2:
  Computes an exponent of each matrix element.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source matrix.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination matrix with the same size and type as src .

  @sa exp

  Python prototype (for reference only):
  ```python3
  exp(src[, dst[, stream]]) -> dst
  ```

  """
  @spec exp(Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def exp(src) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_exp(positional)
    |> to_struct()
  end
  @spec exp(Evision.CUDA.GpuMat.t()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def exp(src) when is_struct(src, Evision.CUDA.GpuMat)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_exp(positional)
    |> to_struct()
  end

  @doc """
  Perform image denoising using Non-local Means Denoising algorithm
  <http://www.ipol.im/pub/algo/bcm_non_local_means_denoising> with several computational
  optimizations. Noise expected to be a gaussian white noise

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Input 8-bit 1-channel, 2-channel or 3-channel image.

  - **h**: `float`.

    Parameter regulating filter strength. Big h value perfectly removes noise but also
    removes image details, smaller h value preserves details but also preserves some noise

  ##### Keyword Arguments
  - **search_window**: `integer()`.

    Size in pixels of the window that is used to compute weighted average for
    given pixel. Should be odd. Affect performance linearly: greater search_window - greater
    denoising time. Recommended value 21 pixels

  - **block_size**: `integer()`.

    Size in pixels of the template patch that is used to compute weights. Should be
    odd. Recommended value 7 pixels

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous invocations.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Output image with the same size and type as src .

  This function expected to be applied to grayscale images. For colored images look at
  FastNonLocalMeansDenoising::labMethod.
  @sa
  fastNlMeansDenoising

  Python prototype (for reference only):
  ```python3
  fastNlMeansDenoising(src, h[, dst[, search_window[, block_size[, stream]]]]) -> dst
  ```
  """
  @spec fastNlMeansDenoising(Evision.CUDA.GpuMat.t(), number(), [{:block_size, term()} | {:search_window, term()} | {:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def fastNlMeansDenoising(src, h, opts) when is_struct(src, Evision.CUDA.GpuMat) and is_float(h) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:block_size, :search_window, :stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      h: Evision.Internal.Structurise.from_struct(h)
    ]
    :evision_nif.cuda_fastNlMeansDenoising(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Perform image denoising using Non-local Means Denoising algorithm
  <http://www.ipol.im/pub/algo/bcm_non_local_means_denoising> with several computational
  optimizations. Noise expected to be a gaussian white noise

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Input 8-bit 1-channel, 2-channel or 3-channel image.

  - **h**: `float`.

    Parameter regulating filter strength. Big h value perfectly removes noise but also
    removes image details, smaller h value preserves details but also preserves some noise

  ##### Keyword Arguments
  - **search_window**: `integer()`.

    Size in pixels of the window that is used to compute weighted average for
    given pixel. Should be odd. Affect performance linearly: greater search_window - greater
    denoising time. Recommended value 21 pixels

  - **block_size**: `integer()`.

    Size in pixels of the template patch that is used to compute weights. Should be
    odd. Recommended value 7 pixels

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous invocations.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Output image with the same size and type as src .

  This function expected to be applied to grayscale images. For colored images look at
  FastNonLocalMeansDenoising::labMethod.
  @sa
  fastNlMeansDenoising

  Python prototype (for reference only):
  ```python3
  fastNlMeansDenoising(src, h[, dst[, search_window[, block_size[, stream]]]]) -> dst
  ```
  """
  @spec fastNlMeansDenoising(Evision.CUDA.GpuMat.t(), number()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def fastNlMeansDenoising(src, h) when is_struct(src, Evision.CUDA.GpuMat) and is_float(h)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      h: Evision.Internal.Structurise.from_struct(h)
    ]
    :evision_nif.cuda_fastNlMeansDenoising(positional)
    |> to_struct()
  end

  @doc """
  Modification of fastNlMeansDenoising function for colored images

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Input 8-bit 3-channel image.

  - **h_luminance**: `float`.

    Parameter regulating filter strength. Big h value perfectly removes noise but
    also removes image details, smaller h value preserves details but also preserves some noise

  - **photo_render**: `float`.

    float The same as h but for color components. For most images value equals 10 will be
    enough to remove colored noise and do not distort colors

  ##### Keyword Arguments
  - **search_window**: `integer()`.

    Size in pixels of the window that is used to compute weighted average for
    given pixel. Should be odd. Affect performance linearly: greater search_window - greater
    denoising time. Recommended value 21 pixels

  - **block_size**: `integer()`.

    Size in pixels of the template patch that is used to compute weights. Should be
    odd. Recommended value 7 pixels

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous invocations.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Output image with the same size and type as src .

  The function converts image to CIELAB colorspace and then separately denoise L and AB components
  with given h parameters using FastNonLocalMeansDenoising::simpleMethod function.
  @sa
  fastNlMeansDenoisingColored

  Python prototype (for reference only):
  ```python3
  fastNlMeansDenoisingColored(src, h_luminance, photo_render[, dst[, search_window[, block_size[, stream]]]]) -> dst
  ```
  """
  @spec fastNlMeansDenoisingColored(Evision.CUDA.GpuMat.t(), number(), number(), [{:block_size, term()} | {:search_window, term()} | {:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def fastNlMeansDenoisingColored(src, h_luminance, photo_render, opts) when is_struct(src, Evision.CUDA.GpuMat) and is_float(h_luminance) and is_float(photo_render) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:block_size, :search_window, :stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      h_luminance: Evision.Internal.Structurise.from_struct(h_luminance),
      photo_render: Evision.Internal.Structurise.from_struct(photo_render)
    ]
    :evision_nif.cuda_fastNlMeansDenoisingColored(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Modification of fastNlMeansDenoising function for colored images

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Input 8-bit 3-channel image.

  - **h_luminance**: `float`.

    Parameter regulating filter strength. Big h value perfectly removes noise but
    also removes image details, smaller h value preserves details but also preserves some noise

  - **photo_render**: `float`.

    float The same as h but for color components. For most images value equals 10 will be
    enough to remove colored noise and do not distort colors

  ##### Keyword Arguments
  - **search_window**: `integer()`.

    Size in pixels of the window that is used to compute weighted average for
    given pixel. Should be odd. Affect performance linearly: greater search_window - greater
    denoising time. Recommended value 21 pixels

  - **block_size**: `integer()`.

    Size in pixels of the template patch that is used to compute weights. Should be
    odd. Recommended value 7 pixels

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous invocations.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Output image with the same size and type as src .

  The function converts image to CIELAB colorspace and then separately denoise L and AB components
  with given h parameters using FastNonLocalMeansDenoising::simpleMethod function.
  @sa
  fastNlMeansDenoisingColored

  Python prototype (for reference only):
  ```python3
  fastNlMeansDenoisingColored(src, h_luminance, photo_render[, dst[, search_window[, block_size[, stream]]]]) -> dst
  ```
  """
  @spec fastNlMeansDenoisingColored(Evision.CUDA.GpuMat.t(), number(), number()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def fastNlMeansDenoisingColored(src, h_luminance, photo_render) when is_struct(src, Evision.CUDA.GpuMat) and is_float(h_luminance) and is_float(photo_render)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      h_luminance: Evision.Internal.Structurise.from_struct(h_luminance),
      photo_render: Evision.Internal.Structurise.from_struct(photo_render)
    ]
    :evision_nif.cuda_fastNlMeansDenoisingColored(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  findMinMax

  ##### Positional Arguments
  - **src**: `Evision.Mat`

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  findMinMax(src[, dst[, mask[, stream]]]) -> dst
  ```
  #### Variant 2:
  findMinMax

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`

  ##### Keyword Arguments
  - **mask**: `Evision.CUDA.GpuMat.t()`.
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  findMinMax(src[, dst[, mask[, stream]]]) -> dst
  ```

  """
  @spec findMinMax(Evision.Mat.maybe_mat_in(), [{:mask, term()} | {:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def findMinMax(src, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:mask, :stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_findMinMax(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec findMinMax(Evision.CUDA.GpuMat.t(), [{:mask, term()} | {:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def findMinMax(src, opts) when is_struct(src, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:mask, :stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_findMinMax(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  findMinMax

  ##### Positional Arguments
  - **src**: `Evision.Mat`

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  findMinMax(src[, dst[, mask[, stream]]]) -> dst
  ```
  #### Variant 2:
  findMinMax

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`

  ##### Keyword Arguments
  - **mask**: `Evision.CUDA.GpuMat.t()`.
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  findMinMax(src[, dst[, mask[, stream]]]) -> dst
  ```

  """
  @spec findMinMax(Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def findMinMax(src) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_findMinMax(positional)
    |> to_struct()
  end
  @spec findMinMax(Evision.CUDA.GpuMat.t()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def findMinMax(src) when is_struct(src, Evision.CUDA.GpuMat)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_findMinMax(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  findMinMaxLoc

  ##### Positional Arguments
  - **src**: `Evision.Mat`

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **minMaxVals**: `Evision.Mat.t()`.
  - **loc**: `Evision.Mat.t()`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  findMinMaxLoc(src[, minMaxVals[, loc[, mask[, stream]]]]) -> minMaxVals, loc
  ```
  #### Variant 2:
  findMinMaxLoc

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`

  ##### Keyword Arguments
  - **mask**: `Evision.CUDA.GpuMat.t()`.
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **minMaxVals**: `Evision.CUDA.GpuMat.t()`.
  - **loc**: `Evision.CUDA.GpuMat.t()`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  findMinMaxLoc(src[, minMaxVals[, loc[, mask[, stream]]]]) -> minMaxVals, loc
  ```

  """
  @spec findMinMaxLoc(Evision.Mat.maybe_mat_in(), [{:mask, term()} | {:stream, term()}] | nil) :: {Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def findMinMaxLoc(src, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:mask, :stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_findMinMaxLoc(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec findMinMaxLoc(Evision.CUDA.GpuMat.t(), [{:mask, term()} | {:stream, term()}] | nil) :: {Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t()} | {:error, String.t()}
  def findMinMaxLoc(src, opts) when is_struct(src, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:mask, :stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_findMinMaxLoc(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  findMinMaxLoc

  ##### Positional Arguments
  - **src**: `Evision.Mat`

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **minMaxVals**: `Evision.Mat.t()`.
  - **loc**: `Evision.Mat.t()`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  findMinMaxLoc(src[, minMaxVals[, loc[, mask[, stream]]]]) -> minMaxVals, loc
  ```
  #### Variant 2:
  findMinMaxLoc

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`

  ##### Keyword Arguments
  - **mask**: `Evision.CUDA.GpuMat.t()`.
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **minMaxVals**: `Evision.CUDA.GpuMat.t()`.
  - **loc**: `Evision.CUDA.GpuMat.t()`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  findMinMaxLoc(src[, minMaxVals[, loc[, mask[, stream]]]]) -> minMaxVals, loc
  ```

  """
  @spec findMinMaxLoc(Evision.Mat.maybe_mat_in()) :: {Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def findMinMaxLoc(src) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_findMinMaxLoc(positional)
    |> to_struct()
  end
  @spec findMinMaxLoc(Evision.CUDA.GpuMat.t()) :: {Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t()} | {:error, String.t()}
  def findMinMaxLoc(src) when is_struct(src, Evision.CUDA.GpuMat)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_findMinMaxLoc(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Flips a 2D matrix around vertical, horizontal, or both axes.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source matrix. Supports 1, 3 and 4 channels images with CV_8U, CV_16U, CV_32S or
    CV_32F depth.

  - **flipCode**: `integer()`.

    Flip mode for the source:
    - 0 Flips around x-axis.
    - \\> 0 Flips around y-axis.
    - \\< 0 Flips around both axes.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination matrix.

  @sa flip

  Python prototype (for reference only):
  ```python3
  flip(src, flipCode[, dst[, stream]]) -> dst
  ```
  #### Variant 2:
  Flips a 2D matrix around vertical, horizontal, or both axes.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source matrix. Supports 1, 3 and 4 channels images with CV_8U, CV_16U, CV_32S or
    CV_32F depth.

  - **flipCode**: `integer()`.

    Flip mode for the source:
    - 0 Flips around x-axis.
    - \\> 0 Flips around y-axis.
    - \\< 0 Flips around both axes.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination matrix.

  @sa flip

  Python prototype (for reference only):
  ```python3
  flip(src, flipCode[, dst[, stream]]) -> dst
  ```

  """
  @spec flip(Evision.Mat.maybe_mat_in(), integer(), [{:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def flip(src, flipCode, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_integer(flipCode) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      flipCode: Evision.Internal.Structurise.from_struct(flipCode)
    ]
    :evision_nif.cuda_flip(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec flip(Evision.CUDA.GpuMat.t(), integer(), [{:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def flip(src, flipCode, opts) when is_struct(src, Evision.CUDA.GpuMat) and is_integer(flipCode) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      flipCode: Evision.Internal.Structurise.from_struct(flipCode)
    ]
    :evision_nif.cuda_flip(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Flips a 2D matrix around vertical, horizontal, or both axes.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source matrix. Supports 1, 3 and 4 channels images with CV_8U, CV_16U, CV_32S or
    CV_32F depth.

  - **flipCode**: `integer()`.

    Flip mode for the source:
    - 0 Flips around x-axis.
    - \\> 0 Flips around y-axis.
    - \\< 0 Flips around both axes.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination matrix.

  @sa flip

  Python prototype (for reference only):
  ```python3
  flip(src, flipCode[, dst[, stream]]) -> dst
  ```
  #### Variant 2:
  Flips a 2D matrix around vertical, horizontal, or both axes.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source matrix. Supports 1, 3 and 4 channels images with CV_8U, CV_16U, CV_32S or
    CV_32F depth.

  - **flipCode**: `integer()`.

    Flip mode for the source:
    - 0 Flips around x-axis.
    - \\> 0 Flips around y-axis.
    - \\< 0 Flips around both axes.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination matrix.

  @sa flip

  Python prototype (for reference only):
  ```python3
  flip(src, flipCode[, dst[, stream]]) -> dst
  ```

  """
  @spec flip(Evision.Mat.maybe_mat_in(), integer()) :: Evision.Mat.t() | {:error, String.t()}
  def flip(src, flipCode) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_integer(flipCode)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      flipCode: Evision.Internal.Structurise.from_struct(flipCode)
    ]
    :evision_nif.cuda_flip(positional)
    |> to_struct()
  end
  @spec flip(Evision.CUDA.GpuMat.t(), integer()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def flip(src, flipCode) when is_struct(src, Evision.CUDA.GpuMat) and is_integer(flipCode)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      flipCode: Evision.Internal.Structurise.from_struct(flipCode)
    ]
    :evision_nif.cuda_flip(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Routines for correcting image color gamma.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source image (3- or 4-channel 8 bit).

  ##### Keyword Arguments
  - **forward**: `bool`.

    true for forward gamma correction or false for inverse gamma correction.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination image.

  Python prototype (for reference only):
  ```python3
  gammaCorrection(src[, dst[, forward[, stream]]]) -> dst
  ```
  #### Variant 2:
  Routines for correcting image color gamma.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source image (3- or 4-channel 8 bit).

  ##### Keyword Arguments
  - **forward**: `bool`.

    true for forward gamma correction or false for inverse gamma correction.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination image.

  Python prototype (for reference only):
  ```python3
  gammaCorrection(src[, dst[, forward[, stream]]]) -> dst
  ```

  """
  @spec gammaCorrection(Evision.Mat.maybe_mat_in(), [{:forward, term()} | {:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def gammaCorrection(src, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:forward, :stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_gammaCorrection(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec gammaCorrection(Evision.CUDA.GpuMat.t(), [{:forward, term()} | {:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def gammaCorrection(src, opts) when is_struct(src, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:forward, :stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_gammaCorrection(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Routines for correcting image color gamma.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source image (3- or 4-channel 8 bit).

  ##### Keyword Arguments
  - **forward**: `bool`.

    true for forward gamma correction or false for inverse gamma correction.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination image.

  Python prototype (for reference only):
  ```python3
  gammaCorrection(src[, dst[, forward[, stream]]]) -> dst
  ```
  #### Variant 2:
  Routines for correcting image color gamma.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source image (3- or 4-channel 8 bit).

  ##### Keyword Arguments
  - **forward**: `bool`.

    true for forward gamma correction or false for inverse gamma correction.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination image.

  Python prototype (for reference only):
  ```python3
  gammaCorrection(src[, dst[, forward[, stream]]]) -> dst
  ```

  """
  @spec gammaCorrection(Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def gammaCorrection(src) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_gammaCorrection(positional)
    |> to_struct()
  end
  @spec gammaCorrection(Evision.CUDA.GpuMat.t()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def gammaCorrection(src) when is_struct(src, Evision.CUDA.GpuMat)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_gammaCorrection(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Performs generalized matrix multiplication.

  ##### Positional Arguments
  - **src1**: `Evision.Mat`.

    First multiplied input matrix that should have CV_32FC1 , CV_64FC1 , CV_32FC2 , or
    CV_64FC2 type.

  - **src2**: `Evision.Mat`.

    Second multiplied input matrix of the same type as src1 .

  - **alpha**: `double`.

    Weight of the matrix product.

  - **src3**: `Evision.Mat`.

    Third optional delta matrix added to the matrix product. It should have the same type
    as src1 and src2 .

  - **beta**: `double`.

    Weight of src3 .

  ##### Keyword Arguments
  - **flags**: `integer()`.

    Operation flags:
    - **GEMM_1_T** transpose src1
    - **GEMM_2_T** transpose src2
    - **GEMM_3_T** transpose src3

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination matrix. It has the proper size and the same type as input matrices.

  The function performs generalized matrix multiplication similar to the gemm functions in BLAS level
  3. For example, gemm(src1, src2, alpha, src3, beta, dst, GEMM_1_T + GEMM_3_T) corresponds to
  \\f[\\texttt{dst} =  \\texttt{alpha} \\cdot \\texttt{src1} ^T  \\cdot \\texttt{src2} +  \\texttt{beta} \\cdot \\texttt{src3} ^T\\f]
  **Note**: Transposition operation doesn't support CV_64FC2 input type.
  @sa gemm

  Python prototype (for reference only):
  ```python3
  gemm(src1, src2, alpha, src3, beta[, dst[, flags[, stream]]]) -> dst
  ```
  #### Variant 2:
  Performs generalized matrix multiplication.

  ##### Positional Arguments
  - **src1**: `Evision.CUDA.GpuMat.t()`.

    First multiplied input matrix that should have CV_32FC1 , CV_64FC1 , CV_32FC2 , or
    CV_64FC2 type.

  - **src2**: `Evision.CUDA.GpuMat.t()`.

    Second multiplied input matrix of the same type as src1 .

  - **alpha**: `double`.

    Weight of the matrix product.

  - **src3**: `Evision.CUDA.GpuMat.t()`.

    Third optional delta matrix added to the matrix product. It should have the same type
    as src1 and src2 .

  - **beta**: `double`.

    Weight of src3 .

  ##### Keyword Arguments
  - **flags**: `integer()`.

    Operation flags:
    - **GEMM_1_T** transpose src1
    - **GEMM_2_T** transpose src2
    - **GEMM_3_T** transpose src3

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination matrix. It has the proper size and the same type as input matrices.

  The function performs generalized matrix multiplication similar to the gemm functions in BLAS level
  3. For example, gemm(src1, src2, alpha, src3, beta, dst, GEMM_1_T + GEMM_3_T) corresponds to
  \\f[\\texttt{dst} =  \\texttt{alpha} \\cdot \\texttt{src1} ^T  \\cdot \\texttt{src2} +  \\texttt{beta} \\cdot \\texttt{src3} ^T\\f]
  **Note**: Transposition operation doesn't support CV_64FC2 input type.
  @sa gemm

  Python prototype (for reference only):
  ```python3
  gemm(src1, src2, alpha, src3, beta[, dst[, flags[, stream]]]) -> dst
  ```

  """
  @spec gemm(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), number(), Evision.Mat.maybe_mat_in(), number(), [{:flags, term()} | {:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def gemm(src1, src2, alpha, src3, beta, opts) when (is_struct(src1, Evision.Mat) or is_struct(src1, Nx.Tensor) or is_number(src1) or is_tuple(src1)) and (is_struct(src2, Evision.Mat) or is_struct(src2, Nx.Tensor) or is_number(src2) or is_tuple(src2)) and is_number(alpha) and (is_struct(src3, Evision.Mat) or is_struct(src3, Nx.Tensor) or is_number(src3) or is_tuple(src3)) and is_number(beta) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:flags, :stream])
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1),
      src2: Evision.Internal.Structurise.from_struct(src2),
      alpha: Evision.Internal.Structurise.from_struct(alpha),
      src3: Evision.Internal.Structurise.from_struct(src3),
      beta: Evision.Internal.Structurise.from_struct(beta)
    ]
    :evision_nif.cuda_gemm(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec gemm(Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t(), number(), Evision.CUDA.GpuMat.t(), number(), [{:flags, term()} | {:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def gemm(src1, src2, alpha, src3, beta, opts) when is_struct(src1, Evision.CUDA.GpuMat) and is_struct(src2, Evision.CUDA.GpuMat) and is_number(alpha) and is_struct(src3, Evision.CUDA.GpuMat) and is_number(beta) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:flags, :stream])
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1),
      src2: Evision.Internal.Structurise.from_struct(src2),
      alpha: Evision.Internal.Structurise.from_struct(alpha),
      src3: Evision.Internal.Structurise.from_struct(src3),
      beta: Evision.Internal.Structurise.from_struct(beta)
    ]
    :evision_nif.cuda_gemm(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Performs generalized matrix multiplication.

  ##### Positional Arguments
  - **src1**: `Evision.Mat`.

    First multiplied input matrix that should have CV_32FC1 , CV_64FC1 , CV_32FC2 , or
    CV_64FC2 type.

  - **src2**: `Evision.Mat`.

    Second multiplied input matrix of the same type as src1 .

  - **alpha**: `double`.

    Weight of the matrix product.

  - **src3**: `Evision.Mat`.

    Third optional delta matrix added to the matrix product. It should have the same type
    as src1 and src2 .

  - **beta**: `double`.

    Weight of src3 .

  ##### Keyword Arguments
  - **flags**: `integer()`.

    Operation flags:
    - **GEMM_1_T** transpose src1
    - **GEMM_2_T** transpose src2
    - **GEMM_3_T** transpose src3

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination matrix. It has the proper size and the same type as input matrices.

  The function performs generalized matrix multiplication similar to the gemm functions in BLAS level
  3. For example, gemm(src1, src2, alpha, src3, beta, dst, GEMM_1_T + GEMM_3_T) corresponds to
  \\f[\\texttt{dst} =  \\texttt{alpha} \\cdot \\texttt{src1} ^T  \\cdot \\texttt{src2} +  \\texttt{beta} \\cdot \\texttt{src3} ^T\\f]
  **Note**: Transposition operation doesn't support CV_64FC2 input type.
  @sa gemm

  Python prototype (for reference only):
  ```python3
  gemm(src1, src2, alpha, src3, beta[, dst[, flags[, stream]]]) -> dst
  ```
  #### Variant 2:
  Performs generalized matrix multiplication.

  ##### Positional Arguments
  - **src1**: `Evision.CUDA.GpuMat.t()`.

    First multiplied input matrix that should have CV_32FC1 , CV_64FC1 , CV_32FC2 , or
    CV_64FC2 type.

  - **src2**: `Evision.CUDA.GpuMat.t()`.

    Second multiplied input matrix of the same type as src1 .

  - **alpha**: `double`.

    Weight of the matrix product.

  - **src3**: `Evision.CUDA.GpuMat.t()`.

    Third optional delta matrix added to the matrix product. It should have the same type
    as src1 and src2 .

  - **beta**: `double`.

    Weight of src3 .

  ##### Keyword Arguments
  - **flags**: `integer()`.

    Operation flags:
    - **GEMM_1_T** transpose src1
    - **GEMM_2_T** transpose src2
    - **GEMM_3_T** transpose src3

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination matrix. It has the proper size and the same type as input matrices.

  The function performs generalized matrix multiplication similar to the gemm functions in BLAS level
  3. For example, gemm(src1, src2, alpha, src3, beta, dst, GEMM_1_T + GEMM_3_T) corresponds to
  \\f[\\texttt{dst} =  \\texttt{alpha} \\cdot \\texttt{src1} ^T  \\cdot \\texttt{src2} +  \\texttt{beta} \\cdot \\texttt{src3} ^T\\f]
  **Note**: Transposition operation doesn't support CV_64FC2 input type.
  @sa gemm

  Python prototype (for reference only):
  ```python3
  gemm(src1, src2, alpha, src3, beta[, dst[, flags[, stream]]]) -> dst
  ```

  """
  @spec gemm(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), number(), Evision.Mat.maybe_mat_in(), number()) :: Evision.Mat.t() | {:error, String.t()}
  def gemm(src1, src2, alpha, src3, beta) when (is_struct(src1, Evision.Mat) or is_struct(src1, Nx.Tensor) or is_number(src1) or is_tuple(src1)) and (is_struct(src2, Evision.Mat) or is_struct(src2, Nx.Tensor) or is_number(src2) or is_tuple(src2)) and is_number(alpha) and (is_struct(src3, Evision.Mat) or is_struct(src3, Nx.Tensor) or is_number(src3) or is_tuple(src3)) and is_number(beta)
  do
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1),
      src2: Evision.Internal.Structurise.from_struct(src2),
      alpha: Evision.Internal.Structurise.from_struct(alpha),
      src3: Evision.Internal.Structurise.from_struct(src3),
      beta: Evision.Internal.Structurise.from_struct(beta)
    ]
    :evision_nif.cuda_gemm(positional)
    |> to_struct()
  end
  @spec gemm(Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t(), number(), Evision.CUDA.GpuMat.t(), number()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def gemm(src1, src2, alpha, src3, beta) when is_struct(src1, Evision.CUDA.GpuMat) and is_struct(src2, Evision.CUDA.GpuMat) and is_number(alpha) and is_struct(src3, Evision.CUDA.GpuMat) and is_number(beta)
  do
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1),
      src2: Evision.Internal.Structurise.from_struct(src2),
      alpha: Evision.Internal.Structurise.from_struct(alpha),
      src3: Evision.Internal.Structurise.from_struct(src3),
      beta: Evision.Internal.Structurise.from_struct(beta)
    ]
    :evision_nif.cuda_gemm(positional)
    |> to_struct()
  end

  @doc """
  Returns the number of installed CUDA-enabled devices.
  ##### Return
  - **retval**: `integer()`

  Use this function before any other CUDA functions calls. If OpenCV is compiled without CUDA support,
  this function returns 0. If the CUDA driver is not installed, or is incompatible, this function
  returns -1.

  Python prototype (for reference only):
  ```python3
  getCudaEnabledDeviceCount() -> retval
  ```
  """
  @spec getCudaEnabledDeviceCount() :: integer() | {:error, String.t()}
  def getCudaEnabledDeviceCount() do
    positional = [
    ]
    :evision_nif.cuda_getCudaEnabledDeviceCount(positional)
    |> to_struct()
  end

  @doc """
  Returns the current device index set by cuda::setDevice or initialized by default.
  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getDevice() -> retval
  ```
  """
  @spec getDevice() :: integer() | {:error, String.t()}
  def getDevice() do
    positional = [
    ]
    :evision_nif.cuda_getDevice(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Calculates a histogram with evenly distributed bins.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source image. CV_8U, CV_16U, or CV_16S depth and 1 or 4 channels are supported. For
    a four-channel image, all channels are processed separately.

  - **histSize**: `integer()`.

    Size of the histogram.

  - **lowerLevel**: `integer()`.

    Lower boundary of lowest-level bin.

  - **upperLevel**: `integer()`.

    Upper boundary of highest-level bin.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **hist**: `Evision.Mat.t()`.

    Destination histogram with one row, histSize columns, and the CV_32S type.

  Python prototype (for reference only):
  ```python3
  histEven(src, histSize, lowerLevel, upperLevel[, hist[, stream]]) -> hist
  ```
  #### Variant 2:
  Calculates a histogram with evenly distributed bins.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source image. CV_8U, CV_16U, or CV_16S depth and 1 or 4 channels are supported. For
    a four-channel image, all channels are processed separately.

  - **histSize**: `integer()`.

    Size of the histogram.

  - **lowerLevel**: `integer()`.

    Lower boundary of lowest-level bin.

  - **upperLevel**: `integer()`.

    Upper boundary of highest-level bin.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **hist**: `Evision.CUDA.GpuMat.t()`.

    Destination histogram with one row, histSize columns, and the CV_32S type.

  Python prototype (for reference only):
  ```python3
  histEven(src, histSize, lowerLevel, upperLevel[, hist[, stream]]) -> hist
  ```
  #### Variant 3:
  histEven

  ##### Positional Arguments
  - **src**: `Evision.Mat`
  - **hist**: `GpuMat*`
  - **histSize**: `int*`
  - **lowerLevel**: `int*`
  - **upperLevel**: `int*`

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  histEven(src, hist, histSize, lowerLevel, upperLevel[, stream]) -> None
  ```
  #### Variant 4:
  histEven

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`
  - **hist**: `GpuMat*`
  - **histSize**: `int*`
  - **lowerLevel**: `int*`
  - **upperLevel**: `int*`

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  histEven(src, hist, histSize, lowerLevel, upperLevel[, stream]) -> None
  ```

  """
  @spec histEven(Evision.Mat.maybe_mat_in(), integer(), integer(), integer(), [{:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def histEven(src, histSize, lowerLevel, upperLevel, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_integer(histSize) and is_integer(lowerLevel) and is_integer(upperLevel) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      histSize: Evision.Internal.Structurise.from_struct(histSize),
      lowerLevel: Evision.Internal.Structurise.from_struct(lowerLevel),
      upperLevel: Evision.Internal.Structurise.from_struct(upperLevel)
    ]
    :evision_nif.cuda_histEven(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec histEven(Evision.CUDA.GpuMat.t(), integer(), integer(), integer(), [{:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def histEven(src, histSize, lowerLevel, upperLevel, opts) when is_struct(src, Evision.CUDA.GpuMat) and is_integer(histSize) and is_integer(lowerLevel) and is_integer(upperLevel) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      histSize: Evision.Internal.Structurise.from_struct(histSize),
      lowerLevel: Evision.Internal.Structurise.from_struct(lowerLevel),
      upperLevel: Evision.Internal.Structurise.from_struct(upperLevel)
    ]
    :evision_nif.cuda_histEven(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec histEven(Evision.Mat.maybe_mat_in(), Evision.CUDA.GpuMat.t(), integer(), integer(), integer()) :: :ok | {:error, String.t()}
  def histEven(src, hist, histSize, lowerLevel, upperLevel) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      hist: Evision.Internal.Structurise.from_struct(hist),
      histSize: Evision.Internal.Structurise.from_struct(histSize),
      lowerLevel: Evision.Internal.Structurise.from_struct(lowerLevel),
      upperLevel: Evision.Internal.Structurise.from_struct(upperLevel)
    ]
    :evision_nif.cuda_histEven(positional)
    |> to_struct()
  end
  @spec histEven(Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t(), integer(), integer(), integer()) :: :ok | {:error, String.t()}
  def histEven(src, hist, histSize, lowerLevel, upperLevel) when is_struct(src, Evision.CUDA.GpuMat)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      hist: Evision.Internal.Structurise.from_struct(hist),
      histSize: Evision.Internal.Structurise.from_struct(histSize),
      lowerLevel: Evision.Internal.Structurise.from_struct(lowerLevel),
      upperLevel: Evision.Internal.Structurise.from_struct(upperLevel)
    ]
    :evision_nif.cuda_histEven(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Calculates a histogram with evenly distributed bins.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source image. CV_8U, CV_16U, or CV_16S depth and 1 or 4 channels are supported. For
    a four-channel image, all channels are processed separately.

  - **histSize**: `integer()`.

    Size of the histogram.

  - **lowerLevel**: `integer()`.

    Lower boundary of lowest-level bin.

  - **upperLevel**: `integer()`.

    Upper boundary of highest-level bin.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **hist**: `Evision.Mat.t()`.

    Destination histogram with one row, histSize columns, and the CV_32S type.

  Python prototype (for reference only):
  ```python3
  histEven(src, histSize, lowerLevel, upperLevel[, hist[, stream]]) -> hist
  ```
  #### Variant 2:
  Calculates a histogram with evenly distributed bins.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source image. CV_8U, CV_16U, or CV_16S depth and 1 or 4 channels are supported. For
    a four-channel image, all channels are processed separately.

  - **histSize**: `integer()`.

    Size of the histogram.

  - **lowerLevel**: `integer()`.

    Lower boundary of lowest-level bin.

  - **upperLevel**: `integer()`.

    Upper boundary of highest-level bin.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **hist**: `Evision.CUDA.GpuMat.t()`.

    Destination histogram with one row, histSize columns, and the CV_32S type.

  Python prototype (for reference only):
  ```python3
  histEven(src, histSize, lowerLevel, upperLevel[, hist[, stream]]) -> hist
  ```

  """
  @spec histEven(Evision.Mat.maybe_mat_in(), integer(), integer(), integer()) :: Evision.Mat.t() | {:error, String.t()}
  def histEven(src, histSize, lowerLevel, upperLevel) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_integer(histSize) and is_integer(lowerLevel) and is_integer(upperLevel)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      histSize: Evision.Internal.Structurise.from_struct(histSize),
      lowerLevel: Evision.Internal.Structurise.from_struct(lowerLevel),
      upperLevel: Evision.Internal.Structurise.from_struct(upperLevel)
    ]
    :evision_nif.cuda_histEven(positional)
    |> to_struct()
  end
  @spec histEven(Evision.CUDA.GpuMat.t(), integer(), integer(), integer()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def histEven(src, histSize, lowerLevel, upperLevel) when is_struct(src, Evision.CUDA.GpuMat) and is_integer(histSize) and is_integer(lowerLevel) and is_integer(upperLevel)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      histSize: Evision.Internal.Structurise.from_struct(histSize),
      lowerLevel: Evision.Internal.Structurise.from_struct(lowerLevel),
      upperLevel: Evision.Internal.Structurise.from_struct(upperLevel)
    ]
    :evision_nif.cuda_histEven(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  histEven

  ##### Positional Arguments
  - **src**: `Evision.Mat`
  - **hist**: `GpuMat*`
  - **histSize**: `int*`
  - **lowerLevel**: `int*`
  - **upperLevel**: `int*`

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  histEven(src, hist, histSize, lowerLevel, upperLevel[, stream]) -> None
  ```
  #### Variant 2:
  histEven

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`
  - **hist**: `GpuMat*`
  - **histSize**: `int*`
  - **lowerLevel**: `int*`
  - **upperLevel**: `int*`

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  histEven(src, hist, histSize, lowerLevel, upperLevel[, stream]) -> None
  ```

  """
  @spec histEven(Evision.Mat.maybe_mat_in(), Evision.CUDA.GpuMat.t(), integer(), integer(), integer(), [{:stream, term()}] | nil) :: :ok | {:error, String.t()}
  def histEven(src, hist, histSize, lowerLevel, upperLevel, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      hist: Evision.Internal.Structurise.from_struct(hist),
      histSize: Evision.Internal.Structurise.from_struct(histSize),
      lowerLevel: Evision.Internal.Structurise.from_struct(lowerLevel),
      upperLevel: Evision.Internal.Structurise.from_struct(upperLevel)
    ]
    :evision_nif.cuda_histEven(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec histEven(Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t(), integer(), integer(), integer(), [{:stream, term()}] | nil) :: :ok | {:error, String.t()}
  def histEven(src, hist, histSize, lowerLevel, upperLevel, opts) when is_struct(src, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      hist: Evision.Internal.Structurise.from_struct(hist),
      histSize: Evision.Internal.Structurise.from_struct(histSize),
      lowerLevel: Evision.Internal.Structurise.from_struct(lowerLevel),
      upperLevel: Evision.Internal.Structurise.from_struct(upperLevel)
    ]
    :evision_nif.cuda_histEven(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Calculates a histogram with bins determined by the levels array.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source image. CV_8U , CV_16U , or CV_16S depth and 1 or 4 channels are supported.
    For a four-channel image, all channels are processed separately.

  - **levels**: `Evision.Mat`.

    Number of levels in the histogram.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **hist**: `Evision.Mat.t()`.

    Destination histogram with one row, (levels.cols-1) columns, and the CV_32SC1 type.

  Python prototype (for reference only):
  ```python3
  histRange(src, levels[, hist[, stream]]) -> hist
  ```
  #### Variant 2:
  Calculates a histogram with bins determined by the levels array.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source image. CV_8U , CV_16U , or CV_16S depth and 1 or 4 channels are supported.
    For a four-channel image, all channels are processed separately.

  - **levels**: `Evision.CUDA.GpuMat.t()`.

    Number of levels in the histogram.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **hist**: `Evision.CUDA.GpuMat.t()`.

    Destination histogram with one row, (levels.cols-1) columns, and the CV_32SC1 type.

  Python prototype (for reference only):
  ```python3
  histRange(src, levels[, hist[, stream]]) -> hist
  ```
  #### Variant 3:
  histRange

  ##### Positional Arguments
  - **src**: `Evision.Mat`
  - **hist**: `GpuMat*`
  - **levels**: `GpuMat*`

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  histRange(src, hist, levels[, stream]) -> None
  ```
  #### Variant 4:
  histRange

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`
  - **hist**: `GpuMat*`
  - **levels**: `GpuMat*`

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  histRange(src, hist, levels[, stream]) -> None
  ```

  """
  @spec histRange(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def histRange(src, levels, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (is_struct(levels, Evision.Mat) or is_struct(levels, Nx.Tensor) or is_number(levels) or is_tuple(levels)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      levels: Evision.Internal.Structurise.from_struct(levels)
    ]
    :evision_nif.cuda_histRange(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec histRange(Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t(), [{:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def histRange(src, levels, opts) when is_struct(src, Evision.CUDA.GpuMat) and is_struct(levels, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      levels: Evision.Internal.Structurise.from_struct(levels)
    ]
    :evision_nif.cuda_histRange(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec histRange(Evision.Mat.maybe_mat_in(), Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t()) :: :ok | {:error, String.t()}
  def histRange(src, hist, levels) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      hist: Evision.Internal.Structurise.from_struct(hist),
      levels: Evision.Internal.Structurise.from_struct(levels)
    ]
    :evision_nif.cuda_histRange(positional)
    |> to_struct()
  end
  @spec histRange(Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t()) :: :ok | {:error, String.t()}
  def histRange(src, hist, levels) when is_struct(src, Evision.CUDA.GpuMat)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      hist: Evision.Internal.Structurise.from_struct(hist),
      levels: Evision.Internal.Structurise.from_struct(levels)
    ]
    :evision_nif.cuda_histRange(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Calculates a histogram with bins determined by the levels array.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source image. CV_8U , CV_16U , or CV_16S depth and 1 or 4 channels are supported.
    For a four-channel image, all channels are processed separately.

  - **levels**: `Evision.Mat`.

    Number of levels in the histogram.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **hist**: `Evision.Mat.t()`.

    Destination histogram with one row, (levels.cols-1) columns, and the CV_32SC1 type.

  Python prototype (for reference only):
  ```python3
  histRange(src, levels[, hist[, stream]]) -> hist
  ```
  #### Variant 2:
  Calculates a histogram with bins determined by the levels array.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source image. CV_8U , CV_16U , or CV_16S depth and 1 or 4 channels are supported.
    For a four-channel image, all channels are processed separately.

  - **levels**: `Evision.CUDA.GpuMat.t()`.

    Number of levels in the histogram.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **hist**: `Evision.CUDA.GpuMat.t()`.

    Destination histogram with one row, (levels.cols-1) columns, and the CV_32SC1 type.

  Python prototype (for reference only):
  ```python3
  histRange(src, levels[, hist[, stream]]) -> hist
  ```

  """
  @spec histRange(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def histRange(src, levels) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (is_struct(levels, Evision.Mat) or is_struct(levels, Nx.Tensor) or is_number(levels) or is_tuple(levels))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      levels: Evision.Internal.Structurise.from_struct(levels)
    ]
    :evision_nif.cuda_histRange(positional)
    |> to_struct()
  end
  @spec histRange(Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def histRange(src, levels) when is_struct(src, Evision.CUDA.GpuMat) and is_struct(levels, Evision.CUDA.GpuMat)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      levels: Evision.Internal.Structurise.from_struct(levels)
    ]
    :evision_nif.cuda_histRange(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  histRange

  ##### Positional Arguments
  - **src**: `Evision.Mat`
  - **hist**: `GpuMat*`
  - **levels**: `GpuMat*`

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  histRange(src, hist, levels[, stream]) -> None
  ```
  #### Variant 2:
  histRange

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`
  - **hist**: `GpuMat*`
  - **levels**: `GpuMat*`

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  histRange(src, hist, levels[, stream]) -> None
  ```

  """
  @spec histRange(Evision.Mat.maybe_mat_in(), Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t(), [{:stream, term()}] | nil) :: :ok | {:error, String.t()}
  def histRange(src, hist, levels, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      hist: Evision.Internal.Structurise.from_struct(hist),
      levels: Evision.Internal.Structurise.from_struct(levels)
    ]
    :evision_nif.cuda_histRange(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec histRange(Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t(), [{:stream, term()}] | nil) :: :ok | {:error, String.t()}
  def histRange(src, hist, levels, opts) when is_struct(src, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      hist: Evision.Internal.Structurise.from_struct(hist),
      levels: Evision.Internal.Structurise.from_struct(levels)
    ]
    :evision_nif.cuda_histRange(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Checks if array elements lie between two scalars.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    first input array.

  - **lowerb**: `Evision.scalar()`.

    inclusive lower boundary cv::Scalar.

  - **upperb**: `Evision.scalar()`.

    inclusive upper boundary cv::Scalar.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    output array of the same size as src and CV_8U type.

  The function checks the range as follows:
  - For every element of a single-channel input array:
    \\f[\\texttt{dst} (I)= \\texttt{lowerb}\\_0  \\leq \\texttt{src} (I)\\_0 \\leq  \\texttt{upperb}\\_0\\f]

  - For two-channel arrays:
    \\f[\\texttt{dst} (I)= \\texttt{lowerb}\\_0  \\leq \\texttt{src} (I)\\_0 \\leq  \\texttt{upperb}\\_0  \\land \\texttt{lowerb}\\_1  \\leq \\texttt{src} (I)\\_1 \\leq  \\texttt{upperb}\\_1\\f]

  - and so forth.

  That is, dst (I) is set to 255 (all 1 -bits) if src (I) is within the
  specified 1D, 2D, 3D, ... box and 0 otherwise.
  Note that unlike the CPU inRange, this does NOT accept an array for lowerb or
  upperb, only a cv::Scalar.

  @sa cv::inRange

  Python prototype (for reference only):
  ```python3
  inRange(src, lowerb, upperb[, dst[, stream]]) -> dst
  ```
  #### Variant 2:
  Checks if array elements lie between two scalars.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    first input array.

  - **lowerb**: `Evision.scalar()`.

    inclusive lower boundary cv::Scalar.

  - **upperb**: `Evision.scalar()`.

    inclusive upper boundary cv::Scalar.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    output array of the same size as src and CV_8U type.

  The function checks the range as follows:
  - For every element of a single-channel input array:
    \\f[\\texttt{dst} (I)= \\texttt{lowerb}\\_0  \\leq \\texttt{src} (I)\\_0 \\leq  \\texttt{upperb}\\_0\\f]

  - For two-channel arrays:
    \\f[\\texttt{dst} (I)= \\texttt{lowerb}\\_0  \\leq \\texttt{src} (I)\\_0 \\leq  \\texttt{upperb}\\_0  \\land \\texttt{lowerb}\\_1  \\leq \\texttt{src} (I)\\_1 \\leq  \\texttt{upperb}\\_1\\f]

  - and so forth.

  That is, dst (I) is set to 255 (all 1 -bits) if src (I) is within the
  specified 1D, 2D, 3D, ... box and 0 otherwise.
  Note that unlike the CPU inRange, this does NOT accept an array for lowerb or
  upperb, only a cv::Scalar.

  @sa cv::inRange

  Python prototype (for reference only):
  ```python3
  inRange(src, lowerb, upperb[, dst[, stream]]) -> dst
  ```

  """
  @spec inRange(Evision.Mat.maybe_mat_in(), Evision.scalar(), Evision.scalar(), [{:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def inRange(src, lowerb, upperb, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (is_number(lowerb) or is_tuple(lowerb)) and (is_number(upperb) or is_tuple(upperb)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      lowerb: Evision.Internal.Structurise.from_struct(lowerb),
      upperb: Evision.Internal.Structurise.from_struct(upperb)
    ]
    :evision_nif.cuda_inRange(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec inRange(Evision.CUDA.GpuMat.t(), Evision.scalar(), Evision.scalar(), [{:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def inRange(src, lowerb, upperb, opts) when is_struct(src, Evision.CUDA.GpuMat) and (is_number(lowerb) or is_tuple(lowerb)) and (is_number(upperb) or is_tuple(upperb)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      lowerb: Evision.Internal.Structurise.from_struct(lowerb),
      upperb: Evision.Internal.Structurise.from_struct(upperb)
    ]
    :evision_nif.cuda_inRange(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Checks if array elements lie between two scalars.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    first input array.

  - **lowerb**: `Evision.scalar()`.

    inclusive lower boundary cv::Scalar.

  - **upperb**: `Evision.scalar()`.

    inclusive upper boundary cv::Scalar.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    output array of the same size as src and CV_8U type.

  The function checks the range as follows:
  - For every element of a single-channel input array:
    \\f[\\texttt{dst} (I)= \\texttt{lowerb}\\_0  \\leq \\texttt{src} (I)\\_0 \\leq  \\texttt{upperb}\\_0\\f]

  - For two-channel arrays:
    \\f[\\texttt{dst} (I)= \\texttt{lowerb}\\_0  \\leq \\texttt{src} (I)\\_0 \\leq  \\texttt{upperb}\\_0  \\land \\texttt{lowerb}\\_1  \\leq \\texttt{src} (I)\\_1 \\leq  \\texttt{upperb}\\_1\\f]

  - and so forth.

  That is, dst (I) is set to 255 (all 1 -bits) if src (I) is within the
  specified 1D, 2D, 3D, ... box and 0 otherwise.
  Note that unlike the CPU inRange, this does NOT accept an array for lowerb or
  upperb, only a cv::Scalar.

  @sa cv::inRange

  Python prototype (for reference only):
  ```python3
  inRange(src, lowerb, upperb[, dst[, stream]]) -> dst
  ```
  #### Variant 2:
  Checks if array elements lie between two scalars.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    first input array.

  - **lowerb**: `Evision.scalar()`.

    inclusive lower boundary cv::Scalar.

  - **upperb**: `Evision.scalar()`.

    inclusive upper boundary cv::Scalar.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    output array of the same size as src and CV_8U type.

  The function checks the range as follows:
  - For every element of a single-channel input array:
    \\f[\\texttt{dst} (I)= \\texttt{lowerb}\\_0  \\leq \\texttt{src} (I)\\_0 \\leq  \\texttt{upperb}\\_0\\f]

  - For two-channel arrays:
    \\f[\\texttt{dst} (I)= \\texttt{lowerb}\\_0  \\leq \\texttt{src} (I)\\_0 \\leq  \\texttt{upperb}\\_0  \\land \\texttt{lowerb}\\_1  \\leq \\texttt{src} (I)\\_1 \\leq  \\texttt{upperb}\\_1\\f]

  - and so forth.

  That is, dst (I) is set to 255 (all 1 -bits) if src (I) is within the
  specified 1D, 2D, 3D, ... box and 0 otherwise.
  Note that unlike the CPU inRange, this does NOT accept an array for lowerb or
  upperb, only a cv::Scalar.

  @sa cv::inRange

  Python prototype (for reference only):
  ```python3
  inRange(src, lowerb, upperb[, dst[, stream]]) -> dst
  ```

  """
  @spec inRange(Evision.Mat.maybe_mat_in(), Evision.scalar(), Evision.scalar()) :: Evision.Mat.t() | {:error, String.t()}
  def inRange(src, lowerb, upperb) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (is_number(lowerb) or is_tuple(lowerb)) and (is_number(upperb) or is_tuple(upperb))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      lowerb: Evision.Internal.Structurise.from_struct(lowerb),
      upperb: Evision.Internal.Structurise.from_struct(upperb)
    ]
    :evision_nif.cuda_inRange(positional)
    |> to_struct()
  end
  @spec inRange(Evision.CUDA.GpuMat.t(), Evision.scalar(), Evision.scalar()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def inRange(src, lowerb, upperb) when is_struct(src, Evision.CUDA.GpuMat) and (is_number(lowerb) or is_tuple(lowerb)) and (is_number(upperb) or is_tuple(upperb))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      lowerb: Evision.Internal.Structurise.from_struct(lowerb),
      upperb: Evision.Internal.Structurise.from_struct(upperb)
    ]
    :evision_nif.cuda_inRange(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Computes an integral image.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source image. Only CV_8UC1 images are supported for now.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **sum**: `Evision.Mat.t()`.

    Integral image containing 32-bit unsigned integer values packed into CV_32SC1 .

  @sa integral

  Python prototype (for reference only):
  ```python3
  integral(src[, sum[, stream]]) -> sum
  ```
  #### Variant 2:
  Computes an integral image.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source image. Only CV_8UC1 images are supported for now.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **sum**: `Evision.CUDA.GpuMat.t()`.

    Integral image containing 32-bit unsigned integer values packed into CV_32SC1 .

  @sa integral

  Python prototype (for reference only):
  ```python3
  integral(src[, sum[, stream]]) -> sum
  ```

  """
  @spec integral(Evision.Mat.maybe_mat_in(), [{:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def integral(src, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_integral(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec integral(Evision.CUDA.GpuMat.t(), [{:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def integral(src, opts) when is_struct(src, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_integral(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Computes an integral image.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source image. Only CV_8UC1 images are supported for now.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **sum**: `Evision.Mat.t()`.

    Integral image containing 32-bit unsigned integer values packed into CV_32SC1 .

  @sa integral

  Python prototype (for reference only):
  ```python3
  integral(src[, sum[, stream]]) -> sum
  ```
  #### Variant 2:
  Computes an integral image.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source image. Only CV_8UC1 images are supported for now.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **sum**: `Evision.CUDA.GpuMat.t()`.

    Integral image containing 32-bit unsigned integer values packed into CV_32SC1 .

  @sa integral

  Python prototype (for reference only):
  ```python3
  integral(src[, sum[, stream]]) -> sum
  ```

  """
  @spec integral(Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def integral(src) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_integral(positional)
    |> to_struct()
  end
  @spec integral(Evision.CUDA.GpuMat.t()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def integral(src) when is_struct(src, Evision.CUDA.GpuMat)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_integral(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Computes a natural logarithm of absolute value of each matrix element.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source matrix.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination matrix with the same size and type as src .

  @sa log

  Python prototype (for reference only):
  ```python3
  log(src[, dst[, stream]]) -> dst
  ```
  #### Variant 2:
  Computes a natural logarithm of absolute value of each matrix element.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source matrix.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination matrix with the same size and type as src .

  @sa log

  Python prototype (for reference only):
  ```python3
  log(src[, dst[, stream]]) -> dst
  ```

  """
  @spec log(Evision.Mat.maybe_mat_in(), [{:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def log(src, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_log(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec log(Evision.CUDA.GpuMat.t(), [{:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def log(src, opts) when is_struct(src, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_log(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Computes a natural logarithm of absolute value of each matrix element.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source matrix.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination matrix with the same size and type as src .

  @sa log

  Python prototype (for reference only):
  ```python3
  log(src[, dst[, stream]]) -> dst
  ```
  #### Variant 2:
  Computes a natural logarithm of absolute value of each matrix element.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source matrix.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination matrix with the same size and type as src .

  @sa log

  Python prototype (for reference only):
  ```python3
  log(src[, dst[, stream]]) -> dst
  ```

  """
  @spec log(Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def log(src) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_log(positional)
    |> to_struct()
  end
  @spec log(Evision.CUDA.GpuMat.t()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def log(src) when is_struct(src, Evision.CUDA.GpuMat)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_log(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Performs pixel by pixel right left of an image by a constant value.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source matrix. Supports 1, 3 and 4 channels images with CV_8U , CV_16U or CV_32S
    depth.

  - **val**: `Evision.scalar()`.

    Constant values, one per channel.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination matrix with the same size and type as src .

  Python prototype (for reference only):
  ```python3
  lshift(src, val[, dst[, stream]]) -> dst
  ```
  #### Variant 2:
  Performs pixel by pixel right left of an image by a constant value.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source matrix. Supports 1, 3 and 4 channels images with CV_8U , CV_16U or CV_32S
    depth.

  - **val**: `Evision.scalar()`.

    Constant values, one per channel.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination matrix with the same size and type as src .

  Python prototype (for reference only):
  ```python3
  lshift(src, val[, dst[, stream]]) -> dst
  ```

  """
  @spec lshift(Evision.Mat.maybe_mat_in(), Evision.scalar(), [{:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def lshift(src, val, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (is_number(val) or is_tuple(val)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.cuda_lshift(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec lshift(Evision.CUDA.GpuMat.t(), Evision.scalar(), [{:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def lshift(src, val, opts) when is_struct(src, Evision.CUDA.GpuMat) and (is_number(val) or is_tuple(val)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.cuda_lshift(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Performs pixel by pixel right left of an image by a constant value.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source matrix. Supports 1, 3 and 4 channels images with CV_8U , CV_16U or CV_32S
    depth.

  - **val**: `Evision.scalar()`.

    Constant values, one per channel.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination matrix with the same size and type as src .

  Python prototype (for reference only):
  ```python3
  lshift(src, val[, dst[, stream]]) -> dst
  ```
  #### Variant 2:
  Performs pixel by pixel right left of an image by a constant value.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source matrix. Supports 1, 3 and 4 channels images with CV_8U , CV_16U or CV_32S
    depth.

  - **val**: `Evision.scalar()`.

    Constant values, one per channel.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination matrix with the same size and type as src .

  Python prototype (for reference only):
  ```python3
  lshift(src, val[, dst[, stream]]) -> dst
  ```

  """
  @spec lshift(Evision.Mat.maybe_mat_in(), Evision.scalar()) :: Evision.Mat.t() | {:error, String.t()}
  def lshift(src, val) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (is_number(val) or is_tuple(val))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.cuda_lshift(positional)
    |> to_struct()
  end
  @spec lshift(Evision.CUDA.GpuMat.t(), Evision.scalar()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def lshift(src, val) when is_struct(src, Evision.CUDA.GpuMat) and (is_number(val) or is_tuple(val))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.cuda_lshift(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  magnitude

  ##### Positional Arguments
  - **x**: `Evision.Mat`.

    Source matrix containing real components ( CV_32FC1 ).

  - **y**: `Evision.Mat`.

    Source matrix containing imaginary components ( CV_32FC1 ).

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **magnitude**: `Evision.Mat.t()`.

    Destination matrix of float magnitudes ( CV_32FC1 ).

  Has overloading in C++

  computes magnitude of each (x(i), y(i)) vector
  supports only floating-point source

  Python prototype (for reference only):
  ```python3
  magnitude(x, y[, magnitude[, stream]]) -> magnitude
  ```
  #### Variant 2:
  magnitude

  ##### Positional Arguments
  - **x**: `Evision.CUDA.GpuMat.t()`.

    Source matrix containing real components ( CV_32FC1 ).

  - **y**: `Evision.CUDA.GpuMat.t()`.

    Source matrix containing imaginary components ( CV_32FC1 ).

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **magnitude**: `Evision.CUDA.GpuMat.t()`.

    Destination matrix of float magnitudes ( CV_32FC1 ).

  Has overloading in C++

  computes magnitude of each (x(i), y(i)) vector
  supports only floating-point source

  Python prototype (for reference only):
  ```python3
  magnitude(x, y[, magnitude[, stream]]) -> magnitude
  ```

  """
  @spec magnitude(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def magnitude(x, y, opts) when (is_struct(x, Evision.Mat) or is_struct(x, Nx.Tensor) or is_number(x) or is_tuple(x)) and (is_struct(y, Evision.Mat) or is_struct(y, Nx.Tensor) or is_number(y) or is_tuple(y)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      x: Evision.Internal.Structurise.from_struct(x),
      y: Evision.Internal.Structurise.from_struct(y)
    ]
    :evision_nif.cuda_magnitude(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec magnitude(Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t(), [{:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def magnitude(x, y, opts) when is_struct(x, Evision.CUDA.GpuMat) and is_struct(y, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      x: Evision.Internal.Structurise.from_struct(x),
      y: Evision.Internal.Structurise.from_struct(y)
    ]
    :evision_nif.cuda_magnitude(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  magnitude

  ##### Positional Arguments
  - **x**: `Evision.Mat`.

    Source matrix containing real components ( CV_32FC1 ).

  - **y**: `Evision.Mat`.

    Source matrix containing imaginary components ( CV_32FC1 ).

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **magnitude**: `Evision.Mat.t()`.

    Destination matrix of float magnitudes ( CV_32FC1 ).

  Has overloading in C++

  computes magnitude of each (x(i), y(i)) vector
  supports only floating-point source

  Python prototype (for reference only):
  ```python3
  magnitude(x, y[, magnitude[, stream]]) -> magnitude
  ```
  #### Variant 2:
  magnitude

  ##### Positional Arguments
  - **x**: `Evision.CUDA.GpuMat.t()`.

    Source matrix containing real components ( CV_32FC1 ).

  - **y**: `Evision.CUDA.GpuMat.t()`.

    Source matrix containing imaginary components ( CV_32FC1 ).

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **magnitude**: `Evision.CUDA.GpuMat.t()`.

    Destination matrix of float magnitudes ( CV_32FC1 ).

  Has overloading in C++

  computes magnitude of each (x(i), y(i)) vector
  supports only floating-point source

  Python prototype (for reference only):
  ```python3
  magnitude(x, y[, magnitude[, stream]]) -> magnitude
  ```
  #### Variant 3:
  Computes magnitudes of complex matrix elements.

  ##### Positional Arguments
  - **xy**: `Evision.Mat`.

    Source complex matrix in the interleaved format ( CV_32FC2 ).

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **magnitude**: `Evision.Mat.t()`.

    Destination matrix of float magnitudes ( CV_32FC1 ).

  @sa magnitude

  Python prototype (for reference only):
  ```python3
  magnitude(xy[, magnitude[, stream]]) -> magnitude
  ```
  #### Variant 4:
  Computes magnitudes of complex matrix elements.

  ##### Positional Arguments
  - **xy**: `Evision.CUDA.GpuMat.t()`.

    Source complex matrix in the interleaved format ( CV_32FC2 ).

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **magnitude**: `Evision.CUDA.GpuMat.t()`.

    Destination matrix of float magnitudes ( CV_32FC1 ).

  @sa magnitude

  Python prototype (for reference only):
  ```python3
  magnitude(xy[, magnitude[, stream]]) -> magnitude
  ```

  """
  @spec magnitude(Evision.Mat.maybe_mat_in(), [{:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def magnitude(xy, opts) when (is_struct(xy, Evision.Mat) or is_struct(xy, Nx.Tensor) or is_number(xy) or is_tuple(xy)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      xy: Evision.Internal.Structurise.from_struct(xy)
    ]
    :evision_nif.cuda_magnitude(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec magnitude(Evision.CUDA.GpuMat.t(), [{:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def magnitude(xy, opts) when is_struct(xy, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      xy: Evision.Internal.Structurise.from_struct(xy)
    ]
    :evision_nif.cuda_magnitude(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec magnitude(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def magnitude(x, y) when (is_struct(x, Evision.Mat) or is_struct(x, Nx.Tensor) or is_number(x) or is_tuple(x)) and (is_struct(y, Evision.Mat) or is_struct(y, Nx.Tensor) or is_number(y) or is_tuple(y))
  do
    positional = [
      x: Evision.Internal.Structurise.from_struct(x),
      y: Evision.Internal.Structurise.from_struct(y)
    ]
    :evision_nif.cuda_magnitude(positional)
    |> to_struct()
  end
  @spec magnitude(Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def magnitude(x, y) when is_struct(x, Evision.CUDA.GpuMat) and is_struct(y, Evision.CUDA.GpuMat)
  do
    positional = [
      x: Evision.Internal.Structurise.from_struct(x),
      y: Evision.Internal.Structurise.from_struct(y)
    ]
    :evision_nif.cuda_magnitude(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Computes magnitudes of complex matrix elements.

  ##### Positional Arguments
  - **xy**: `Evision.Mat`.

    Source complex matrix in the interleaved format ( CV_32FC2 ).

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **magnitude**: `Evision.Mat.t()`.

    Destination matrix of float magnitudes ( CV_32FC1 ).

  @sa magnitude

  Python prototype (for reference only):
  ```python3
  magnitude(xy[, magnitude[, stream]]) -> magnitude
  ```
  #### Variant 2:
  Computes magnitudes of complex matrix elements.

  ##### Positional Arguments
  - **xy**: `Evision.CUDA.GpuMat.t()`.

    Source complex matrix in the interleaved format ( CV_32FC2 ).

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **magnitude**: `Evision.CUDA.GpuMat.t()`.

    Destination matrix of float magnitudes ( CV_32FC1 ).

  @sa magnitude

  Python prototype (for reference only):
  ```python3
  magnitude(xy[, magnitude[, stream]]) -> magnitude
  ```

  """
  @spec magnitude(Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def magnitude(xy) when (is_struct(xy, Evision.Mat) or is_struct(xy, Nx.Tensor) or is_number(xy) or is_tuple(xy))
  do
    positional = [
      xy: Evision.Internal.Structurise.from_struct(xy)
    ]
    :evision_nif.cuda_magnitude(positional)
    |> to_struct()
  end
  @spec magnitude(Evision.CUDA.GpuMat.t()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def magnitude(xy) when is_struct(xy, Evision.CUDA.GpuMat)
  do
    positional = [
      xy: Evision.Internal.Structurise.from_struct(xy)
    ]
    :evision_nif.cuda_magnitude(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  magnitudeSqr

  ##### Positional Arguments
  - **x**: `Evision.Mat`.

    Source matrix containing real components ( CV_32FC1 ).

  - **y**: `Evision.Mat`.

    Source matrix containing imaginary components ( CV_32FC1 ).

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **magnitude**: `Evision.Mat.t()`.

    Destination matrix of float magnitude squares ( CV_32FC1 ).

  Has overloading in C++

  computes squared magnitude of each (x(i), y(i)) vector
  supports only floating-point source

  Python prototype (for reference only):
  ```python3
  magnitudeSqr(x, y[, magnitude[, stream]]) -> magnitude
  ```
  #### Variant 2:
  magnitudeSqr

  ##### Positional Arguments
  - **x**: `Evision.CUDA.GpuMat.t()`.

    Source matrix containing real components ( CV_32FC1 ).

  - **y**: `Evision.CUDA.GpuMat.t()`.

    Source matrix containing imaginary components ( CV_32FC1 ).

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **magnitude**: `Evision.CUDA.GpuMat.t()`.

    Destination matrix of float magnitude squares ( CV_32FC1 ).

  Has overloading in C++

  computes squared magnitude of each (x(i), y(i)) vector
  supports only floating-point source

  Python prototype (for reference only):
  ```python3
  magnitudeSqr(x, y[, magnitude[, stream]]) -> magnitude
  ```

  """
  @spec magnitudeSqr(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def magnitudeSqr(x, y, opts) when (is_struct(x, Evision.Mat) or is_struct(x, Nx.Tensor) or is_number(x) or is_tuple(x)) and (is_struct(y, Evision.Mat) or is_struct(y, Nx.Tensor) or is_number(y) or is_tuple(y)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      x: Evision.Internal.Structurise.from_struct(x),
      y: Evision.Internal.Structurise.from_struct(y)
    ]
    :evision_nif.cuda_magnitudeSqr(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec magnitudeSqr(Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t(), [{:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def magnitudeSqr(x, y, opts) when is_struct(x, Evision.CUDA.GpuMat) and is_struct(y, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      x: Evision.Internal.Structurise.from_struct(x),
      y: Evision.Internal.Structurise.from_struct(y)
    ]
    :evision_nif.cuda_magnitudeSqr(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  magnitudeSqr

  ##### Positional Arguments
  - **x**: `Evision.Mat`.

    Source matrix containing real components ( CV_32FC1 ).

  - **y**: `Evision.Mat`.

    Source matrix containing imaginary components ( CV_32FC1 ).

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **magnitude**: `Evision.Mat.t()`.

    Destination matrix of float magnitude squares ( CV_32FC1 ).

  Has overloading in C++

  computes squared magnitude of each (x(i), y(i)) vector
  supports only floating-point source

  Python prototype (for reference only):
  ```python3
  magnitudeSqr(x, y[, magnitude[, stream]]) -> magnitude
  ```
  #### Variant 2:
  magnitudeSqr

  ##### Positional Arguments
  - **x**: `Evision.CUDA.GpuMat.t()`.

    Source matrix containing real components ( CV_32FC1 ).

  - **y**: `Evision.CUDA.GpuMat.t()`.

    Source matrix containing imaginary components ( CV_32FC1 ).

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **magnitude**: `Evision.CUDA.GpuMat.t()`.

    Destination matrix of float magnitude squares ( CV_32FC1 ).

  Has overloading in C++

  computes squared magnitude of each (x(i), y(i)) vector
  supports only floating-point source

  Python prototype (for reference only):
  ```python3
  magnitudeSqr(x, y[, magnitude[, stream]]) -> magnitude
  ```
  #### Variant 3:
  Computes squared magnitudes of complex matrix elements.

  ##### Positional Arguments
  - **xy**: `Evision.Mat`.

    Source complex matrix in the interleaved format ( CV_32FC2 ).

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **magnitude**: `Evision.Mat.t()`.

    Destination matrix of float magnitude squares ( CV_32FC1 ).

  Python prototype (for reference only):
  ```python3
  magnitudeSqr(xy[, magnitude[, stream]]) -> magnitude
  ```
  #### Variant 4:
  Computes squared magnitudes of complex matrix elements.

  ##### Positional Arguments
  - **xy**: `Evision.CUDA.GpuMat.t()`.

    Source complex matrix in the interleaved format ( CV_32FC2 ).

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **magnitude**: `Evision.CUDA.GpuMat.t()`.

    Destination matrix of float magnitude squares ( CV_32FC1 ).

  Python prototype (for reference only):
  ```python3
  magnitudeSqr(xy[, magnitude[, stream]]) -> magnitude
  ```

  """
  @spec magnitudeSqr(Evision.Mat.maybe_mat_in(), [{:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def magnitudeSqr(xy, opts) when (is_struct(xy, Evision.Mat) or is_struct(xy, Nx.Tensor) or is_number(xy) or is_tuple(xy)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      xy: Evision.Internal.Structurise.from_struct(xy)
    ]
    :evision_nif.cuda_magnitudeSqr(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec magnitudeSqr(Evision.CUDA.GpuMat.t(), [{:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def magnitudeSqr(xy, opts) when is_struct(xy, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      xy: Evision.Internal.Structurise.from_struct(xy)
    ]
    :evision_nif.cuda_magnitudeSqr(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec magnitudeSqr(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def magnitudeSqr(x, y) when (is_struct(x, Evision.Mat) or is_struct(x, Nx.Tensor) or is_number(x) or is_tuple(x)) and (is_struct(y, Evision.Mat) or is_struct(y, Nx.Tensor) or is_number(y) or is_tuple(y))
  do
    positional = [
      x: Evision.Internal.Structurise.from_struct(x),
      y: Evision.Internal.Structurise.from_struct(y)
    ]
    :evision_nif.cuda_magnitudeSqr(positional)
    |> to_struct()
  end
  @spec magnitudeSqr(Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def magnitudeSqr(x, y) when is_struct(x, Evision.CUDA.GpuMat) and is_struct(y, Evision.CUDA.GpuMat)
  do
    positional = [
      x: Evision.Internal.Structurise.from_struct(x),
      y: Evision.Internal.Structurise.from_struct(y)
    ]
    :evision_nif.cuda_magnitudeSqr(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Computes squared magnitudes of complex matrix elements.

  ##### Positional Arguments
  - **xy**: `Evision.Mat`.

    Source complex matrix in the interleaved format ( CV_32FC2 ).

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **magnitude**: `Evision.Mat.t()`.

    Destination matrix of float magnitude squares ( CV_32FC1 ).

  Python prototype (for reference only):
  ```python3
  magnitudeSqr(xy[, magnitude[, stream]]) -> magnitude
  ```
  #### Variant 2:
  Computes squared magnitudes of complex matrix elements.

  ##### Positional Arguments
  - **xy**: `Evision.CUDA.GpuMat.t()`.

    Source complex matrix in the interleaved format ( CV_32FC2 ).

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **magnitude**: `Evision.CUDA.GpuMat.t()`.

    Destination matrix of float magnitude squares ( CV_32FC1 ).

  Python prototype (for reference only):
  ```python3
  magnitudeSqr(xy[, magnitude[, stream]]) -> magnitude
  ```

  """
  @spec magnitudeSqr(Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def magnitudeSqr(xy) when (is_struct(xy, Evision.Mat) or is_struct(xy, Nx.Tensor) or is_number(xy) or is_tuple(xy))
  do
    positional = [
      xy: Evision.Internal.Structurise.from_struct(xy)
    ]
    :evision_nif.cuda_magnitudeSqr(positional)
    |> to_struct()
  end
  @spec magnitudeSqr(Evision.CUDA.GpuMat.t()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def magnitudeSqr(xy) when is_struct(xy, Evision.CUDA.GpuMat)
  do
    positional = [
      xy: Evision.Internal.Structurise.from_struct(xy)
    ]
    :evision_nif.cuda_magnitudeSqr(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Computes the per-element maximum of two matrices (or a matrix and a scalar).

  ##### Positional Arguments
  - **src1**: `Evision.Mat`.

    First source matrix or scalar.

  - **src2**: `Evision.Mat`.

    Second source matrix or scalar.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination matrix that has the same size and type as the input array(s).

  @sa max

  Python prototype (for reference only):
  ```python3
  max(src1, src2[, dst[, stream]]) -> dst
  ```
  #### Variant 2:
  Computes the per-element maximum of two matrices (or a matrix and a scalar).

  ##### Positional Arguments
  - **src1**: `Evision.CUDA.GpuMat.t()`.

    First source matrix or scalar.

  - **src2**: `Evision.CUDA.GpuMat.t()`.

    Second source matrix or scalar.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination matrix that has the same size and type as the input array(s).

  @sa max

  Python prototype (for reference only):
  ```python3
  max(src1, src2[, dst[, stream]]) -> dst
  ```

  """
  @spec max(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def max(src1, src2, opts) when (is_struct(src1, Evision.Mat) or is_struct(src1, Nx.Tensor) or is_number(src1) or is_tuple(src1)) and (is_struct(src2, Evision.Mat) or is_struct(src2, Nx.Tensor) or is_number(src2) or is_tuple(src2)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1),
      src2: Evision.Internal.Structurise.from_struct(src2)
    ]
    :evision_nif.cuda_max(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec max(Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t(), [{:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def max(src1, src2, opts) when is_struct(src1, Evision.CUDA.GpuMat) and is_struct(src2, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1),
      src2: Evision.Internal.Structurise.from_struct(src2)
    ]
    :evision_nif.cuda_max(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Computes the per-element maximum of two matrices (or a matrix and a scalar).

  ##### Positional Arguments
  - **src1**: `Evision.Mat`.

    First source matrix or scalar.

  - **src2**: `Evision.Mat`.

    Second source matrix or scalar.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination matrix that has the same size and type as the input array(s).

  @sa max

  Python prototype (for reference only):
  ```python3
  max(src1, src2[, dst[, stream]]) -> dst
  ```
  #### Variant 2:
  Computes the per-element maximum of two matrices (or a matrix and a scalar).

  ##### Positional Arguments
  - **src1**: `Evision.CUDA.GpuMat.t()`.

    First source matrix or scalar.

  - **src2**: `Evision.CUDA.GpuMat.t()`.

    Second source matrix or scalar.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination matrix that has the same size and type as the input array(s).

  @sa max

  Python prototype (for reference only):
  ```python3
  max(src1, src2[, dst[, stream]]) -> dst
  ```

  """
  @spec max(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def max(src1, src2) when (is_struct(src1, Evision.Mat) or is_struct(src1, Nx.Tensor) or is_number(src1) or is_tuple(src1)) and (is_struct(src2, Evision.Mat) or is_struct(src2, Nx.Tensor) or is_number(src2) or is_tuple(src2))
  do
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1),
      src2: Evision.Internal.Structurise.from_struct(src2)
    ]
    :evision_nif.cuda_max(positional)
    |> to_struct()
  end
  @spec max(Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def max(src1, src2) when is_struct(src1, Evision.CUDA.GpuMat) and is_struct(src2, Evision.CUDA.GpuMat)
  do
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1),
      src2: Evision.Internal.Structurise.from_struct(src2)
    ]
    :evision_nif.cuda_max(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Performs mean-shift filtering for each point of the source image.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source image. Only CV_8UC4 images are supported for now.

  - **sp**: `integer()`.

    Spatial window radius.

  - **sr**: `integer()`.

    Color window radius.

  ##### Keyword Arguments
  - **criteria**: `TermCriteria`.

    Termination criteria. See TermCriteria.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination image containing the color of mapped points. It has the same size and type
    as src .

  It maps each point of the source image into another point. As a result, you have a new color and new
  position of each point.

  Python prototype (for reference only):
  ```python3
  meanShiftFiltering(src, sp, sr[, dst[, criteria[, stream]]]) -> dst
  ```
  #### Variant 2:
  Performs mean-shift filtering for each point of the source image.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source image. Only CV_8UC4 images are supported for now.

  - **sp**: `integer()`.

    Spatial window radius.

  - **sr**: `integer()`.

    Color window radius.

  ##### Keyword Arguments
  - **criteria**: `TermCriteria`.

    Termination criteria. See TermCriteria.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination image containing the color of mapped points. It has the same size and type
    as src .

  It maps each point of the source image into another point. As a result, you have a new color and new
  position of each point.

  Python prototype (for reference only):
  ```python3
  meanShiftFiltering(src, sp, sr[, dst[, criteria[, stream]]]) -> dst
  ```

  """
  @spec meanShiftFiltering(Evision.Mat.maybe_mat_in(), integer(), integer(), [{:criteria, term()} | {:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def meanShiftFiltering(src, sp, sr, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_integer(sp) and is_integer(sr) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:criteria, :stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      sp: Evision.Internal.Structurise.from_struct(sp),
      sr: Evision.Internal.Structurise.from_struct(sr)
    ]
    :evision_nif.cuda_meanShiftFiltering(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec meanShiftFiltering(Evision.CUDA.GpuMat.t(), integer(), integer(), [{:criteria, term()} | {:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def meanShiftFiltering(src, sp, sr, opts) when is_struct(src, Evision.CUDA.GpuMat) and is_integer(sp) and is_integer(sr) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:criteria, :stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      sp: Evision.Internal.Structurise.from_struct(sp),
      sr: Evision.Internal.Structurise.from_struct(sr)
    ]
    :evision_nif.cuda_meanShiftFiltering(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Performs mean-shift filtering for each point of the source image.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source image. Only CV_8UC4 images are supported for now.

  - **sp**: `integer()`.

    Spatial window radius.

  - **sr**: `integer()`.

    Color window radius.

  ##### Keyword Arguments
  - **criteria**: `TermCriteria`.

    Termination criteria. See TermCriteria.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination image containing the color of mapped points. It has the same size and type
    as src .

  It maps each point of the source image into another point. As a result, you have a new color and new
  position of each point.

  Python prototype (for reference only):
  ```python3
  meanShiftFiltering(src, sp, sr[, dst[, criteria[, stream]]]) -> dst
  ```
  #### Variant 2:
  Performs mean-shift filtering for each point of the source image.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source image. Only CV_8UC4 images are supported for now.

  - **sp**: `integer()`.

    Spatial window radius.

  - **sr**: `integer()`.

    Color window radius.

  ##### Keyword Arguments
  - **criteria**: `TermCriteria`.

    Termination criteria. See TermCriteria.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination image containing the color of mapped points. It has the same size and type
    as src .

  It maps each point of the source image into another point. As a result, you have a new color and new
  position of each point.

  Python prototype (for reference only):
  ```python3
  meanShiftFiltering(src, sp, sr[, dst[, criteria[, stream]]]) -> dst
  ```

  """
  @spec meanShiftFiltering(Evision.Mat.maybe_mat_in(), integer(), integer()) :: Evision.Mat.t() | {:error, String.t()}
  def meanShiftFiltering(src, sp, sr) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_integer(sp) and is_integer(sr)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      sp: Evision.Internal.Structurise.from_struct(sp),
      sr: Evision.Internal.Structurise.from_struct(sr)
    ]
    :evision_nif.cuda_meanShiftFiltering(positional)
    |> to_struct()
  end
  @spec meanShiftFiltering(Evision.CUDA.GpuMat.t(), integer(), integer()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def meanShiftFiltering(src, sp, sr) when is_struct(src, Evision.CUDA.GpuMat) and is_integer(sp) and is_integer(sr)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      sp: Evision.Internal.Structurise.from_struct(sp),
      sr: Evision.Internal.Structurise.from_struct(sr)
    ]
    :evision_nif.cuda_meanShiftFiltering(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Performs a mean-shift procedure and stores information about processed points (their colors and
  positions) in two images.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source image. Only CV_8UC4 images are supported for now.

  - **sp**: `integer()`.

    Spatial window radius.

  - **sr**: `integer()`.

    Color window radius.

  ##### Keyword Arguments
  - **criteria**: `TermCriteria`.

    Termination criteria. See TermCriteria.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dstr**: `Evision.Mat.t()`.

    Destination image containing the color of mapped points. The size and type is the same
    as src .

  - **dstsp**: `Evision.Mat.t()`.

    Destination image containing the position of mapped points. The size is the same as
    src size. The type is CV_16SC2 .

  @sa cuda::meanShiftFiltering

  Python prototype (for reference only):
  ```python3
  meanShiftProc(src, sp, sr[, dstr[, dstsp[, criteria[, stream]]]]) -> dstr, dstsp
  ```
  #### Variant 2:
  Performs a mean-shift procedure and stores information about processed points (their colors and
  positions) in two images.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source image. Only CV_8UC4 images are supported for now.

  - **sp**: `integer()`.

    Spatial window radius.

  - **sr**: `integer()`.

    Color window radius.

  ##### Keyword Arguments
  - **criteria**: `TermCriteria`.

    Termination criteria. See TermCriteria.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dstr**: `Evision.CUDA.GpuMat.t()`.

    Destination image containing the color of mapped points. The size and type is the same
    as src .

  - **dstsp**: `Evision.CUDA.GpuMat.t()`.

    Destination image containing the position of mapped points. The size is the same as
    src size. The type is CV_16SC2 .

  @sa cuda::meanShiftFiltering

  Python prototype (for reference only):
  ```python3
  meanShiftProc(src, sp, sr[, dstr[, dstsp[, criteria[, stream]]]]) -> dstr, dstsp
  ```

  """
  @spec meanShiftProc(Evision.Mat.maybe_mat_in(), integer(), integer(), [{:criteria, term()} | {:stream, term()}] | nil) :: {Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def meanShiftProc(src, sp, sr, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_integer(sp) and is_integer(sr) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:criteria, :stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      sp: Evision.Internal.Structurise.from_struct(sp),
      sr: Evision.Internal.Structurise.from_struct(sr)
    ]
    :evision_nif.cuda_meanShiftProc(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec meanShiftProc(Evision.CUDA.GpuMat.t(), integer(), integer(), [{:criteria, term()} | {:stream, term()}] | nil) :: {Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t()} | {:error, String.t()}
  def meanShiftProc(src, sp, sr, opts) when is_struct(src, Evision.CUDA.GpuMat) and is_integer(sp) and is_integer(sr) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:criteria, :stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      sp: Evision.Internal.Structurise.from_struct(sp),
      sr: Evision.Internal.Structurise.from_struct(sr)
    ]
    :evision_nif.cuda_meanShiftProc(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Performs a mean-shift procedure and stores information about processed points (their colors and
  positions) in two images.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source image. Only CV_8UC4 images are supported for now.

  - **sp**: `integer()`.

    Spatial window radius.

  - **sr**: `integer()`.

    Color window radius.

  ##### Keyword Arguments
  - **criteria**: `TermCriteria`.

    Termination criteria. See TermCriteria.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dstr**: `Evision.Mat.t()`.

    Destination image containing the color of mapped points. The size and type is the same
    as src .

  - **dstsp**: `Evision.Mat.t()`.

    Destination image containing the position of mapped points. The size is the same as
    src size. The type is CV_16SC2 .

  @sa cuda::meanShiftFiltering

  Python prototype (for reference only):
  ```python3
  meanShiftProc(src, sp, sr[, dstr[, dstsp[, criteria[, stream]]]]) -> dstr, dstsp
  ```
  #### Variant 2:
  Performs a mean-shift procedure and stores information about processed points (their colors and
  positions) in two images.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source image. Only CV_8UC4 images are supported for now.

  - **sp**: `integer()`.

    Spatial window radius.

  - **sr**: `integer()`.

    Color window radius.

  ##### Keyword Arguments
  - **criteria**: `TermCriteria`.

    Termination criteria. See TermCriteria.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dstr**: `Evision.CUDA.GpuMat.t()`.

    Destination image containing the color of mapped points. The size and type is the same
    as src .

  - **dstsp**: `Evision.CUDA.GpuMat.t()`.

    Destination image containing the position of mapped points. The size is the same as
    src size. The type is CV_16SC2 .

  @sa cuda::meanShiftFiltering

  Python prototype (for reference only):
  ```python3
  meanShiftProc(src, sp, sr[, dstr[, dstsp[, criteria[, stream]]]]) -> dstr, dstsp
  ```

  """
  @spec meanShiftProc(Evision.Mat.maybe_mat_in(), integer(), integer()) :: {Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def meanShiftProc(src, sp, sr) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_integer(sp) and is_integer(sr)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      sp: Evision.Internal.Structurise.from_struct(sp),
      sr: Evision.Internal.Structurise.from_struct(sr)
    ]
    :evision_nif.cuda_meanShiftProc(positional)
    |> to_struct()
  end
  @spec meanShiftProc(Evision.CUDA.GpuMat.t(), integer(), integer()) :: {Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t()} | {:error, String.t()}
  def meanShiftProc(src, sp, sr) when is_struct(src, Evision.CUDA.GpuMat) and is_integer(sp) and is_integer(sr)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      sp: Evision.Internal.Structurise.from_struct(sp),
      sr: Evision.Internal.Structurise.from_struct(sr)
    ]
    :evision_nif.cuda_meanShiftProc(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Performs a mean-shift segmentation of the source image and eliminates small segments.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source image. Only CV_8UC4 images are supported for now.

  - **sp**: `integer()`.

    Spatial window radius.

  - **sr**: `integer()`.

    Color window radius.

  - **minsize**: `integer()`.

    Minimum segment size. Smaller segments are merged.

  ##### Keyword Arguments
  - **criteria**: `TermCriteria`.

    Termination criteria. See TermCriteria.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Segmented image with the same size and type as src (host or gpu memory).

  Python prototype (for reference only):
  ```python3
  meanShiftSegmentation(src, sp, sr, minsize[, dst[, criteria[, stream]]]) -> dst
  ```
  #### Variant 2:
  Performs a mean-shift segmentation of the source image and eliminates small segments.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source image. Only CV_8UC4 images are supported for now.

  - **sp**: `integer()`.

    Spatial window radius.

  - **sr**: `integer()`.

    Color window radius.

  - **minsize**: `integer()`.

    Minimum segment size. Smaller segments are merged.

  ##### Keyword Arguments
  - **criteria**: `TermCriteria`.

    Termination criteria. See TermCriteria.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Segmented image with the same size and type as src (host or gpu memory).

  Python prototype (for reference only):
  ```python3
  meanShiftSegmentation(src, sp, sr, minsize[, dst[, criteria[, stream]]]) -> dst
  ```

  """
  @spec meanShiftSegmentation(Evision.Mat.maybe_mat_in(), integer(), integer(), integer(), [{:criteria, term()} | {:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def meanShiftSegmentation(src, sp, sr, minsize, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_integer(sp) and is_integer(sr) and is_integer(minsize) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:criteria, :stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      sp: Evision.Internal.Structurise.from_struct(sp),
      sr: Evision.Internal.Structurise.from_struct(sr),
      minsize: Evision.Internal.Structurise.from_struct(minsize)
    ]
    :evision_nif.cuda_meanShiftSegmentation(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec meanShiftSegmentation(Evision.CUDA.GpuMat.t(), integer(), integer(), integer(), [{:criteria, term()} | {:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def meanShiftSegmentation(src, sp, sr, minsize, opts) when is_struct(src, Evision.CUDA.GpuMat) and is_integer(sp) and is_integer(sr) and is_integer(minsize) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:criteria, :stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      sp: Evision.Internal.Structurise.from_struct(sp),
      sr: Evision.Internal.Structurise.from_struct(sr),
      minsize: Evision.Internal.Structurise.from_struct(minsize)
    ]
    :evision_nif.cuda_meanShiftSegmentation(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Performs a mean-shift segmentation of the source image and eliminates small segments.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source image. Only CV_8UC4 images are supported for now.

  - **sp**: `integer()`.

    Spatial window radius.

  - **sr**: `integer()`.

    Color window radius.

  - **minsize**: `integer()`.

    Minimum segment size. Smaller segments are merged.

  ##### Keyword Arguments
  - **criteria**: `TermCriteria`.

    Termination criteria. See TermCriteria.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Segmented image with the same size and type as src (host or gpu memory).

  Python prototype (for reference only):
  ```python3
  meanShiftSegmentation(src, sp, sr, minsize[, dst[, criteria[, stream]]]) -> dst
  ```
  #### Variant 2:
  Performs a mean-shift segmentation of the source image and eliminates small segments.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source image. Only CV_8UC4 images are supported for now.

  - **sp**: `integer()`.

    Spatial window radius.

  - **sr**: `integer()`.

    Color window radius.

  - **minsize**: `integer()`.

    Minimum segment size. Smaller segments are merged.

  ##### Keyword Arguments
  - **criteria**: `TermCriteria`.

    Termination criteria. See TermCriteria.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Segmented image with the same size and type as src (host or gpu memory).

  Python prototype (for reference only):
  ```python3
  meanShiftSegmentation(src, sp, sr, minsize[, dst[, criteria[, stream]]]) -> dst
  ```

  """
  @spec meanShiftSegmentation(Evision.Mat.maybe_mat_in(), integer(), integer(), integer()) :: Evision.Mat.t() | {:error, String.t()}
  def meanShiftSegmentation(src, sp, sr, minsize) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_integer(sp) and is_integer(sr) and is_integer(minsize)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      sp: Evision.Internal.Structurise.from_struct(sp),
      sr: Evision.Internal.Structurise.from_struct(sr),
      minsize: Evision.Internal.Structurise.from_struct(minsize)
    ]
    :evision_nif.cuda_meanShiftSegmentation(positional)
    |> to_struct()
  end
  @spec meanShiftSegmentation(Evision.CUDA.GpuMat.t(), integer(), integer(), integer()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def meanShiftSegmentation(src, sp, sr, minsize) when is_struct(src, Evision.CUDA.GpuMat) and is_integer(sp) and is_integer(sr) and is_integer(minsize)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      sp: Evision.Internal.Structurise.from_struct(sp),
      sr: Evision.Internal.Structurise.from_struct(sr),
      minsize: Evision.Internal.Structurise.from_struct(minsize)
    ]
    :evision_nif.cuda_meanShiftSegmentation(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  meanStdDev

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source matrix. CV_8UC1 and CV_32FC1 matrices are supported for now.

  - **mask**: `Evision.Mat`.

    Operation mask.

  ##### Return
  - **mean**: `Evision.scalar().t()`.

    Mean value.

  - **stddev**: `Evision.scalar().t()`.

    Standard deviation value.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  meanStdDev(src, mask) -> mean, stddev
  ```
  #### Variant 2:
  meanStdDev

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source matrix. CV_8UC1 and CV_32FC1 matrices are supported for now.

  - **mask**: `Evision.CUDA.GpuMat.t()`.

    Operation mask.

  ##### Return
  - **mean**: `Evision.scalar().t()`.

    Mean value.

  - **stddev**: `Evision.scalar().t()`.

    Standard deviation value.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  meanStdDev(src, mask) -> mean, stddev
  ```

  """
  @spec meanStdDev(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: {Evision.scalar(), Evision.scalar()} | {:error, String.t()}
  def meanStdDev(src, mask) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (is_struct(mask, Evision.Mat) or is_struct(mask, Nx.Tensor) or is_number(mask) or is_tuple(mask))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      mask: Evision.Internal.Structurise.from_struct(mask)
    ]
    :evision_nif.cuda_meanStdDev(positional)
    |> to_struct()
  end
  @spec meanStdDev(Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t()) :: {Evision.scalar(), Evision.scalar()} | {:error, String.t()}
  def meanStdDev(src, mask) when is_struct(src, Evision.CUDA.GpuMat) and is_struct(mask, Evision.CUDA.GpuMat)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      mask: Evision.Internal.Structurise.from_struct(mask)
    ]
    :evision_nif.cuda_meanStdDev(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  meanStdDev

  ##### Positional Arguments
  - **mtx**: `Evision.Mat`.

    Source matrix. CV_8UC1 and CV_32FC1 matrices are supported for now.

  ##### Return
  - **mean**: `Evision.scalar().t()`.

    Mean value.

  - **stddev**: `Evision.scalar().t()`.

    Standard deviation value.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  meanStdDev(mtx) -> mean, stddev
  ```
  #### Variant 2:
  meanStdDev

  ##### Positional Arguments
  - **mtx**: `Evision.CUDA.GpuMat.t()`.

    Source matrix. CV_8UC1 and CV_32FC1 matrices are supported for now.

  ##### Return
  - **mean**: `Evision.scalar().t()`.

    Mean value.

  - **stddev**: `Evision.scalar().t()`.

    Standard deviation value.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  meanStdDev(mtx) -> mean, stddev
  ```

  """
  @spec meanStdDev(Evision.Mat.maybe_mat_in()) :: {Evision.scalar(), Evision.scalar()} | {:error, String.t()}
  def meanStdDev(mtx) when (is_struct(mtx, Evision.Mat) or is_struct(mtx, Nx.Tensor) or is_number(mtx) or is_tuple(mtx))
  do
    positional = [
      mtx: Evision.Internal.Structurise.from_struct(mtx)
    ]
    :evision_nif.cuda_meanStdDev(positional)
    |> to_struct()
  end
  @spec meanStdDev(Evision.CUDA.GpuMat.t()) :: {Evision.scalar(), Evision.scalar()} | {:error, String.t()}
  def meanStdDev(mtx) when is_struct(mtx, Evision.CUDA.GpuMat)
  do
    positional = [
      mtx: Evision.Internal.Structurise.from_struct(mtx)
    ]
    :evision_nif.cuda_meanStdDev(positional)
    |> to_struct()
  end

  @doc """
  merge

  ##### Positional Arguments
  - **src**: `[Evision.CUDA.GpuMat]`

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  merge(src[, dst[, stream]]) -> dst
  ```
  """
  @spec merge(list(Evision.CUDA.GpuMat.t()), [{:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def merge(src, opts) when is_list(src) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_merge(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  merge

  ##### Positional Arguments
  - **src**: `[Evision.CUDA.GpuMat]`

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  merge(src[, dst[, stream]]) -> dst
  ```
  """
  @spec merge(list(Evision.CUDA.GpuMat.t())) :: Evision.Mat.t() | {:error, String.t()}
  def merge(src) when is_list(src)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_merge(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Computes the per-element minimum of two matrices (or a matrix and a scalar).

  ##### Positional Arguments
  - **src1**: `Evision.Mat`.

    First source matrix or scalar.

  - **src2**: `Evision.Mat`.

    Second source matrix or scalar.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination matrix that has the same size and type as the input array(s).

  @sa min

  Python prototype (for reference only):
  ```python3
  min(src1, src2[, dst[, stream]]) -> dst
  ```
  #### Variant 2:
  Computes the per-element minimum of two matrices (or a matrix and a scalar).

  ##### Positional Arguments
  - **src1**: `Evision.CUDA.GpuMat.t()`.

    First source matrix or scalar.

  - **src2**: `Evision.CUDA.GpuMat.t()`.

    Second source matrix or scalar.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination matrix that has the same size and type as the input array(s).

  @sa min

  Python prototype (for reference only):
  ```python3
  min(src1, src2[, dst[, stream]]) -> dst
  ```

  """
  @spec min(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def min(src1, src2, opts) when (is_struct(src1, Evision.Mat) or is_struct(src1, Nx.Tensor) or is_number(src1) or is_tuple(src1)) and (is_struct(src2, Evision.Mat) or is_struct(src2, Nx.Tensor) or is_number(src2) or is_tuple(src2)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1),
      src2: Evision.Internal.Structurise.from_struct(src2)
    ]
    :evision_nif.cuda_min(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec min(Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t(), [{:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def min(src1, src2, opts) when is_struct(src1, Evision.CUDA.GpuMat) and is_struct(src2, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1),
      src2: Evision.Internal.Structurise.from_struct(src2)
    ]
    :evision_nif.cuda_min(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Computes the per-element minimum of two matrices (or a matrix and a scalar).

  ##### Positional Arguments
  - **src1**: `Evision.Mat`.

    First source matrix or scalar.

  - **src2**: `Evision.Mat`.

    Second source matrix or scalar.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination matrix that has the same size and type as the input array(s).

  @sa min

  Python prototype (for reference only):
  ```python3
  min(src1, src2[, dst[, stream]]) -> dst
  ```
  #### Variant 2:
  Computes the per-element minimum of two matrices (or a matrix and a scalar).

  ##### Positional Arguments
  - **src1**: `Evision.CUDA.GpuMat.t()`.

    First source matrix or scalar.

  - **src2**: `Evision.CUDA.GpuMat.t()`.

    Second source matrix or scalar.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination matrix that has the same size and type as the input array(s).

  @sa min

  Python prototype (for reference only):
  ```python3
  min(src1, src2[, dst[, stream]]) -> dst
  ```

  """
  @spec min(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def min(src1, src2) when (is_struct(src1, Evision.Mat) or is_struct(src1, Nx.Tensor) or is_number(src1) or is_tuple(src1)) and (is_struct(src2, Evision.Mat) or is_struct(src2, Nx.Tensor) or is_number(src2) or is_tuple(src2))
  do
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1),
      src2: Evision.Internal.Structurise.from_struct(src2)
    ]
    :evision_nif.cuda_min(positional)
    |> to_struct()
  end
  @spec min(Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def min(src1, src2) when is_struct(src1, Evision.CUDA.GpuMat) and is_struct(src2, Evision.CUDA.GpuMat)
  do
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1),
      src2: Evision.Internal.Structurise.from_struct(src2)
    ]
    :evision_nif.cuda_min(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Finds global minimum and maximum matrix elements and returns their values.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Single-channel source image.

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.

    Optional mask to select a sub-matrix.

  ##### Return
  - **minVal**: `double*`.

    Pointer to the returned minimum value. Use NULL if not required.

  - **maxVal**: `double*`.

    Pointer to the returned maximum value. Use NULL if not required.

  The function does not work with CV_64F images on GPUs with the compute capability \\< 1.3.
  @sa minMaxLoc

  Python prototype (for reference only):
  ```python3
  minMax(src[, mask]) -> minVal, maxVal
  ```
  #### Variant 2:
  Finds global minimum and maximum matrix elements and returns their values.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Single-channel source image.

  ##### Keyword Arguments
  - **mask**: `Evision.CUDA.GpuMat.t()`.

    Optional mask to select a sub-matrix.

  ##### Return
  - **minVal**: `double*`.

    Pointer to the returned minimum value. Use NULL if not required.

  - **maxVal**: `double*`.

    Pointer to the returned maximum value. Use NULL if not required.

  The function does not work with CV_64F images on GPUs with the compute capability \\< 1.3.
  @sa minMaxLoc

  Python prototype (for reference only):
  ```python3
  minMax(src[, mask]) -> minVal, maxVal
  ```

  """
  @spec minMax(Evision.Mat.maybe_mat_in(), [{:mask, term()}] | nil) :: {number(), number()} | {:error, String.t()}
  def minMax(src, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:mask])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_minMax(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec minMax(Evision.CUDA.GpuMat.t(), [{:mask, term()}] | nil) :: {number(), number()} | {:error, String.t()}
  def minMax(src, opts) when is_struct(src, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:mask])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_minMax(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Finds global minimum and maximum matrix elements and returns their values.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Single-channel source image.

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.

    Optional mask to select a sub-matrix.

  ##### Return
  - **minVal**: `double*`.

    Pointer to the returned minimum value. Use NULL if not required.

  - **maxVal**: `double*`.

    Pointer to the returned maximum value. Use NULL if not required.

  The function does not work with CV_64F images on GPUs with the compute capability \\< 1.3.
  @sa minMaxLoc

  Python prototype (for reference only):
  ```python3
  minMax(src[, mask]) -> minVal, maxVal
  ```
  #### Variant 2:
  Finds global minimum and maximum matrix elements and returns their values.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Single-channel source image.

  ##### Keyword Arguments
  - **mask**: `Evision.CUDA.GpuMat.t()`.

    Optional mask to select a sub-matrix.

  ##### Return
  - **minVal**: `double*`.

    Pointer to the returned minimum value. Use NULL if not required.

  - **maxVal**: `double*`.

    Pointer to the returned maximum value. Use NULL if not required.

  The function does not work with CV_64F images on GPUs with the compute capability \\< 1.3.
  @sa minMaxLoc

  Python prototype (for reference only):
  ```python3
  minMax(src[, mask]) -> minVal, maxVal
  ```

  """
  @spec minMax(Evision.Mat.maybe_mat_in()) :: {number(), number()} | {:error, String.t()}
  def minMax(src) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_minMax(positional)
    |> to_struct()
  end
  @spec minMax(Evision.CUDA.GpuMat.t()) :: {number(), number()} | {:error, String.t()}
  def minMax(src) when is_struct(src, Evision.CUDA.GpuMat)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_minMax(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Finds global minimum and maximum matrix elements and returns their values with locations.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Single-channel source image.

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.

    Optional mask to select a sub-matrix.

  ##### Return
  - **minVal**: `double*`.

    Pointer to the returned minimum value. Use NULL if not required.

  - **maxVal**: `double*`.

    Pointer to the returned maximum value. Use NULL if not required.

  - **minLoc**: `Point*`.

    Pointer to the returned minimum location. Use NULL if not required.

  - **maxLoc**: `Point*`.

    Pointer to the returned maximum location. Use NULL if not required.

  The function does not work with CV_64F images on GPU with the compute capability \\< 1.3.
  @sa minMaxLoc

  Python prototype (for reference only):
  ```python3
  minMaxLoc(src[, mask]) -> minVal, maxVal, minLoc, maxLoc
  ```
  #### Variant 2:
  Finds global minimum and maximum matrix elements and returns their values with locations.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Single-channel source image.

  ##### Keyword Arguments
  - **mask**: `Evision.CUDA.GpuMat.t()`.

    Optional mask to select a sub-matrix.

  ##### Return
  - **minVal**: `double*`.

    Pointer to the returned minimum value. Use NULL if not required.

  - **maxVal**: `double*`.

    Pointer to the returned maximum value. Use NULL if not required.

  - **minLoc**: `Point*`.

    Pointer to the returned minimum location. Use NULL if not required.

  - **maxLoc**: `Point*`.

    Pointer to the returned maximum location. Use NULL if not required.

  The function does not work with CV_64F images on GPU with the compute capability \\< 1.3.
  @sa minMaxLoc

  Python prototype (for reference only):
  ```python3
  minMaxLoc(src[, mask]) -> minVal, maxVal, minLoc, maxLoc
  ```

  """
  @spec minMaxLoc(Evision.Mat.maybe_mat_in(), [{:mask, term()}] | nil) :: {number(), number(), {number(), number()}, {number(), number()}} | {:error, String.t()}
  def minMaxLoc(src, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:mask])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_minMaxLoc(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec minMaxLoc(Evision.CUDA.GpuMat.t(), [{:mask, term()}] | nil) :: {number(), number(), {number(), number()}, {number(), number()}} | {:error, String.t()}
  def minMaxLoc(src, opts) when is_struct(src, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:mask])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_minMaxLoc(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Finds global minimum and maximum matrix elements and returns their values with locations.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Single-channel source image.

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.

    Optional mask to select a sub-matrix.

  ##### Return
  - **minVal**: `double*`.

    Pointer to the returned minimum value. Use NULL if not required.

  - **maxVal**: `double*`.

    Pointer to the returned maximum value. Use NULL if not required.

  - **minLoc**: `Point*`.

    Pointer to the returned minimum location. Use NULL if not required.

  - **maxLoc**: `Point*`.

    Pointer to the returned maximum location. Use NULL if not required.

  The function does not work with CV_64F images on GPU with the compute capability \\< 1.3.
  @sa minMaxLoc

  Python prototype (for reference only):
  ```python3
  minMaxLoc(src[, mask]) -> minVal, maxVal, minLoc, maxLoc
  ```
  #### Variant 2:
  Finds global minimum and maximum matrix elements and returns their values with locations.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Single-channel source image.

  ##### Keyword Arguments
  - **mask**: `Evision.CUDA.GpuMat.t()`.

    Optional mask to select a sub-matrix.

  ##### Return
  - **minVal**: `double*`.

    Pointer to the returned minimum value. Use NULL if not required.

  - **maxVal**: `double*`.

    Pointer to the returned maximum value. Use NULL if not required.

  - **minLoc**: `Point*`.

    Pointer to the returned minimum location. Use NULL if not required.

  - **maxLoc**: `Point*`.

    Pointer to the returned maximum location. Use NULL if not required.

  The function does not work with CV_64F images on GPU with the compute capability \\< 1.3.
  @sa minMaxLoc

  Python prototype (for reference only):
  ```python3
  minMaxLoc(src[, mask]) -> minVal, maxVal, minLoc, maxLoc
  ```

  """
  @spec minMaxLoc(Evision.Mat.maybe_mat_in()) :: {number(), number(), {number(), number()}, {number(), number()}} | {:error, String.t()}
  def minMaxLoc(src) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_minMaxLoc(positional)
    |> to_struct()
  end
  @spec minMaxLoc(Evision.CUDA.GpuMat.t()) :: {number(), number(), {number(), number()}, {number(), number()}} | {:error, String.t()}
  def minMaxLoc(src) when is_struct(src, Evision.CUDA.GpuMat)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_minMaxLoc(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Calculates all of the moments up to the 3rd order of a rasterized shape.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Raster image (single-channel 2D array).

  ##### Keyword Arguments
  - **binaryImage**: `bool`.

    If it is true, all non-zero image pixels are treated as 1's.

  - **order**: `MomentsOrder`.

    Order of largest moments to calculate with lower order moments requiring less computation.

  - **momentsType**: `integer()`.

    Precision to use when calculating moments. Available types are \\ref CV_32F and \\ref CV_64F with the performance of \\ref CV_32F an order of magnitude greater than \\ref CV_64F. If the image is small the accuracy from \\ref CV_32F can be equal or very close to \\ref CV_64F.

  ##### Return
  - **retval**: `Moments`

  The function computes moments, up to the 3rd order, of a rasterized shape. The
  results are returned in the structure cv::Moments.

  **Note**: For maximum performance use the asynchronous version cuda::spatialMoments() as this version interally allocates and deallocates both GpuMat and HostMem to respectively perform the calculation on the device and download the result to the host.
  The costly HostMem allocation cannot be avoided however the GpuMat device allocation can be by using BufferPool, e.g.
  ```
  setBufferPoolUsage(true);
  setBufferPoolConfig(getDevice(), numMoments(order) * ((momentsType == CV_64F) ? sizeof(double) : sizeof(float)), 1);
  ```
  see the \\a CUDA_TEST_P(Moments, Accuracy) test inside opencv_contrib_source_code/modules/cudaimgproc/test/test_moments.cpp for an example.
  @returns cv::Moments.
  @sa cuda::spatialMoments, cuda::convertSpatialMoments, cuda::numMoments, cuda::MomentsOrder

  Python prototype (for reference only):
  ```python3
  moments(src[, binaryImage[, order[, momentsType]]]) -> retval
  ```
  #### Variant 2:
  Calculates all of the moments up to the 3rd order of a rasterized shape.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Raster image (single-channel 2D array).

  ##### Keyword Arguments
  - **binaryImage**: `bool`.

    If it is true, all non-zero image pixels are treated as 1's.

  - **order**: `MomentsOrder`.

    Order of largest moments to calculate with lower order moments requiring less computation.

  - **momentsType**: `integer()`.

    Precision to use when calculating moments. Available types are \\ref CV_32F and \\ref CV_64F with the performance of \\ref CV_32F an order of magnitude greater than \\ref CV_64F. If the image is small the accuracy from \\ref CV_32F can be equal or very close to \\ref CV_64F.

  ##### Return
  - **retval**: `Moments`

  The function computes moments, up to the 3rd order, of a rasterized shape. The
  results are returned in the structure cv::Moments.

  **Note**: For maximum performance use the asynchronous version cuda::spatialMoments() as this version interally allocates and deallocates both GpuMat and HostMem to respectively perform the calculation on the device and download the result to the host.
  The costly HostMem allocation cannot be avoided however the GpuMat device allocation can be by using BufferPool, e.g.
  ```
  setBufferPoolUsage(true);
  setBufferPoolConfig(getDevice(), numMoments(order) * ((momentsType == CV_64F) ? sizeof(double) : sizeof(float)), 1);
  ```
  see the \\a CUDA_TEST_P(Moments, Accuracy) test inside opencv_contrib_source_code/modules/cudaimgproc/test/test_moments.cpp for an example.
  @returns cv::Moments.
  @sa cuda::spatialMoments, cuda::convertSpatialMoments, cuda::numMoments, cuda::MomentsOrder

  Python prototype (for reference only):
  ```python3
  moments(src[, binaryImage[, order[, momentsType]]]) -> retval
  ```

  """
  @spec moments(Evision.Mat.maybe_mat_in(), [{:binaryImage, term()} | {:momentsType, term()} | {:order, term()}] | nil) :: map() | {:error, String.t()}
  def moments(src, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:binaryImage, :momentsType, :order])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_moments(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec moments(Evision.CUDA.GpuMat.t(), [{:binaryImage, term()} | {:momentsType, term()} | {:order, term()}] | nil) :: map() | {:error, String.t()}
  def moments(src, opts) when is_struct(src, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:binaryImage, :momentsType, :order])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_moments(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Calculates all of the moments up to the 3rd order of a rasterized shape.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Raster image (single-channel 2D array).

  ##### Keyword Arguments
  - **binaryImage**: `bool`.

    If it is true, all non-zero image pixels are treated as 1's.

  - **order**: `MomentsOrder`.

    Order of largest moments to calculate with lower order moments requiring less computation.

  - **momentsType**: `integer()`.

    Precision to use when calculating moments. Available types are \\ref CV_32F and \\ref CV_64F with the performance of \\ref CV_32F an order of magnitude greater than \\ref CV_64F. If the image is small the accuracy from \\ref CV_32F can be equal or very close to \\ref CV_64F.

  ##### Return
  - **retval**: `Moments`

  The function computes moments, up to the 3rd order, of a rasterized shape. The
  results are returned in the structure cv::Moments.

  **Note**: For maximum performance use the asynchronous version cuda::spatialMoments() as this version interally allocates and deallocates both GpuMat and HostMem to respectively perform the calculation on the device and download the result to the host.
  The costly HostMem allocation cannot be avoided however the GpuMat device allocation can be by using BufferPool, e.g.
  ```
  setBufferPoolUsage(true);
  setBufferPoolConfig(getDevice(), numMoments(order) * ((momentsType == CV_64F) ? sizeof(double) : sizeof(float)), 1);
  ```
  see the \\a CUDA_TEST_P(Moments, Accuracy) test inside opencv_contrib_source_code/modules/cudaimgproc/test/test_moments.cpp for an example.
  @returns cv::Moments.
  @sa cuda::spatialMoments, cuda::convertSpatialMoments, cuda::numMoments, cuda::MomentsOrder

  Python prototype (for reference only):
  ```python3
  moments(src[, binaryImage[, order[, momentsType]]]) -> retval
  ```
  #### Variant 2:
  Calculates all of the moments up to the 3rd order of a rasterized shape.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Raster image (single-channel 2D array).

  ##### Keyword Arguments
  - **binaryImage**: `bool`.

    If it is true, all non-zero image pixels are treated as 1's.

  - **order**: `MomentsOrder`.

    Order of largest moments to calculate with lower order moments requiring less computation.

  - **momentsType**: `integer()`.

    Precision to use when calculating moments. Available types are \\ref CV_32F and \\ref CV_64F with the performance of \\ref CV_32F an order of magnitude greater than \\ref CV_64F. If the image is small the accuracy from \\ref CV_32F can be equal or very close to \\ref CV_64F.

  ##### Return
  - **retval**: `Moments`

  The function computes moments, up to the 3rd order, of a rasterized shape. The
  results are returned in the structure cv::Moments.

  **Note**: For maximum performance use the asynchronous version cuda::spatialMoments() as this version interally allocates and deallocates both GpuMat and HostMem to respectively perform the calculation on the device and download the result to the host.
  The costly HostMem allocation cannot be avoided however the GpuMat device allocation can be by using BufferPool, e.g.
  ```
  setBufferPoolUsage(true);
  setBufferPoolConfig(getDevice(), numMoments(order) * ((momentsType == CV_64F) ? sizeof(double) : sizeof(float)), 1);
  ```
  see the \\a CUDA_TEST_P(Moments, Accuracy) test inside opencv_contrib_source_code/modules/cudaimgproc/test/test_moments.cpp for an example.
  @returns cv::Moments.
  @sa cuda::spatialMoments, cuda::convertSpatialMoments, cuda::numMoments, cuda::MomentsOrder

  Python prototype (for reference only):
  ```python3
  moments(src[, binaryImage[, order[, momentsType]]]) -> retval
  ```

  """
  @spec moments(Evision.Mat.maybe_mat_in()) :: map() | {:error, String.t()}
  def moments(src) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_moments(positional)
    |> to_struct()
  end
  @spec moments(Evision.CUDA.GpuMat.t()) :: map() | {:error, String.t()}
  def moments(src) when is_struct(src, Evision.CUDA.GpuMat)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_moments(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Performs a per-element multiplication of two Fourier spectrums and scales the result.

  ##### Positional Arguments
  - **src1**: `Evision.Mat`.

    First spectrum.

  - **src2**: `Evision.Mat`.

    Second spectrum with the same size and type as a .

  - **flags**: `integer()`.

    Mock parameter used for CPU/CUDA interfaces similarity, simply add a `0` value.

  - **scale**: `float`.

    Scale constant.

  ##### Keyword Arguments
  - **conjB**: `bool`.

    Optional flag to specify if the second spectrum needs to be conjugated before the
    multiplication.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination spectrum.

  Only full (not packed) CV_32FC2 complex spectrums in the interleaved format are supported for now.
  @sa mulSpectrums

  Python prototype (for reference only):
  ```python3
  mulAndScaleSpectrums(src1, src2, flags, scale[, dst[, conjB[, stream]]]) -> dst
  ```
  #### Variant 2:
  Performs a per-element multiplication of two Fourier spectrums and scales the result.

  ##### Positional Arguments
  - **src1**: `Evision.CUDA.GpuMat.t()`.

    First spectrum.

  - **src2**: `Evision.CUDA.GpuMat.t()`.

    Second spectrum with the same size and type as a .

  - **flags**: `integer()`.

    Mock parameter used for CPU/CUDA interfaces similarity, simply add a `0` value.

  - **scale**: `float`.

    Scale constant.

  ##### Keyword Arguments
  - **conjB**: `bool`.

    Optional flag to specify if the second spectrum needs to be conjugated before the
    multiplication.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination spectrum.

  Only full (not packed) CV_32FC2 complex spectrums in the interleaved format are supported for now.
  @sa mulSpectrums

  Python prototype (for reference only):
  ```python3
  mulAndScaleSpectrums(src1, src2, flags, scale[, dst[, conjB[, stream]]]) -> dst
  ```

  """
  @spec mulAndScaleSpectrums(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), integer(), number(), [{:conjB, term()} | {:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def mulAndScaleSpectrums(src1, src2, flags, scale, opts) when (is_struct(src1, Evision.Mat) or is_struct(src1, Nx.Tensor) or is_number(src1) or is_tuple(src1)) and (is_struct(src2, Evision.Mat) or is_struct(src2, Nx.Tensor) or is_number(src2) or is_tuple(src2)) and is_integer(flags) and is_float(scale) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:conjB, :stream])
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1),
      src2: Evision.Internal.Structurise.from_struct(src2),
      flags: Evision.Internal.Structurise.from_struct(flags),
      scale: Evision.Internal.Structurise.from_struct(scale)
    ]
    :evision_nif.cuda_mulAndScaleSpectrums(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec mulAndScaleSpectrums(Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t(), integer(), number(), [{:conjB, term()} | {:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def mulAndScaleSpectrums(src1, src2, flags, scale, opts) when is_struct(src1, Evision.CUDA.GpuMat) and is_struct(src2, Evision.CUDA.GpuMat) and is_integer(flags) and is_float(scale) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:conjB, :stream])
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1),
      src2: Evision.Internal.Structurise.from_struct(src2),
      flags: Evision.Internal.Structurise.from_struct(flags),
      scale: Evision.Internal.Structurise.from_struct(scale)
    ]
    :evision_nif.cuda_mulAndScaleSpectrums(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Performs a per-element multiplication of two Fourier spectrums and scales the result.

  ##### Positional Arguments
  - **src1**: `Evision.Mat`.

    First spectrum.

  - **src2**: `Evision.Mat`.

    Second spectrum with the same size and type as a .

  - **flags**: `integer()`.

    Mock parameter used for CPU/CUDA interfaces similarity, simply add a `0` value.

  - **scale**: `float`.

    Scale constant.

  ##### Keyword Arguments
  - **conjB**: `bool`.

    Optional flag to specify if the second spectrum needs to be conjugated before the
    multiplication.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination spectrum.

  Only full (not packed) CV_32FC2 complex spectrums in the interleaved format are supported for now.
  @sa mulSpectrums

  Python prototype (for reference only):
  ```python3
  mulAndScaleSpectrums(src1, src2, flags, scale[, dst[, conjB[, stream]]]) -> dst
  ```
  #### Variant 2:
  Performs a per-element multiplication of two Fourier spectrums and scales the result.

  ##### Positional Arguments
  - **src1**: `Evision.CUDA.GpuMat.t()`.

    First spectrum.

  - **src2**: `Evision.CUDA.GpuMat.t()`.

    Second spectrum with the same size and type as a .

  - **flags**: `integer()`.

    Mock parameter used for CPU/CUDA interfaces similarity, simply add a `0` value.

  - **scale**: `float`.

    Scale constant.

  ##### Keyword Arguments
  - **conjB**: `bool`.

    Optional flag to specify if the second spectrum needs to be conjugated before the
    multiplication.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination spectrum.

  Only full (not packed) CV_32FC2 complex spectrums in the interleaved format are supported for now.
  @sa mulSpectrums

  Python prototype (for reference only):
  ```python3
  mulAndScaleSpectrums(src1, src2, flags, scale[, dst[, conjB[, stream]]]) -> dst
  ```

  """
  @spec mulAndScaleSpectrums(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), integer(), number()) :: Evision.Mat.t() | {:error, String.t()}
  def mulAndScaleSpectrums(src1, src2, flags, scale) when (is_struct(src1, Evision.Mat) or is_struct(src1, Nx.Tensor) or is_number(src1) or is_tuple(src1)) and (is_struct(src2, Evision.Mat) or is_struct(src2, Nx.Tensor) or is_number(src2) or is_tuple(src2)) and is_integer(flags) and is_float(scale)
  do
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1),
      src2: Evision.Internal.Structurise.from_struct(src2),
      flags: Evision.Internal.Structurise.from_struct(flags),
      scale: Evision.Internal.Structurise.from_struct(scale)
    ]
    :evision_nif.cuda_mulAndScaleSpectrums(positional)
    |> to_struct()
  end
  @spec mulAndScaleSpectrums(Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t(), integer(), number()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def mulAndScaleSpectrums(src1, src2, flags, scale) when is_struct(src1, Evision.CUDA.GpuMat) and is_struct(src2, Evision.CUDA.GpuMat) and is_integer(flags) and is_float(scale)
  do
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1),
      src2: Evision.Internal.Structurise.from_struct(src2),
      flags: Evision.Internal.Structurise.from_struct(flags),
      scale: Evision.Internal.Structurise.from_struct(scale)
    ]
    :evision_nif.cuda_mulAndScaleSpectrums(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Performs a per-element multiplication of two Fourier spectrums.

  ##### Positional Arguments
  - **src1**: `Evision.Mat`.

    First spectrum.

  - **src2**: `Evision.Mat`.

    Second spectrum with the same size and type as a .

  - **flags**: `integer()`.

    Mock parameter used for CPU/CUDA interfaces similarity.

  ##### Keyword Arguments
  - **conjB**: `bool`.

    Optional flag to specify if the second spectrum needs to be conjugated before the
    multiplication.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination spectrum.

  Only full (not packed) CV_32FC2 complex spectrums in the interleaved format are supported for now.
  @sa mulSpectrums

  Python prototype (for reference only):
  ```python3
  mulSpectrums(src1, src2, flags[, dst[, conjB[, stream]]]) -> dst
  ```
  #### Variant 2:
  Performs a per-element multiplication of two Fourier spectrums.

  ##### Positional Arguments
  - **src1**: `Evision.CUDA.GpuMat.t()`.

    First spectrum.

  - **src2**: `Evision.CUDA.GpuMat.t()`.

    Second spectrum with the same size and type as a .

  - **flags**: `integer()`.

    Mock parameter used for CPU/CUDA interfaces similarity.

  ##### Keyword Arguments
  - **conjB**: `bool`.

    Optional flag to specify if the second spectrum needs to be conjugated before the
    multiplication.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination spectrum.

  Only full (not packed) CV_32FC2 complex spectrums in the interleaved format are supported for now.
  @sa mulSpectrums

  Python prototype (for reference only):
  ```python3
  mulSpectrums(src1, src2, flags[, dst[, conjB[, stream]]]) -> dst
  ```

  """
  @spec mulSpectrums(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), integer(), [{:conjB, term()} | {:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def mulSpectrums(src1, src2, flags, opts) when (is_struct(src1, Evision.Mat) or is_struct(src1, Nx.Tensor) or is_number(src1) or is_tuple(src1)) and (is_struct(src2, Evision.Mat) or is_struct(src2, Nx.Tensor) or is_number(src2) or is_tuple(src2)) and is_integer(flags) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:conjB, :stream])
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1),
      src2: Evision.Internal.Structurise.from_struct(src2),
      flags: Evision.Internal.Structurise.from_struct(flags)
    ]
    :evision_nif.cuda_mulSpectrums(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec mulSpectrums(Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t(), integer(), [{:conjB, term()} | {:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def mulSpectrums(src1, src2, flags, opts) when is_struct(src1, Evision.CUDA.GpuMat) and is_struct(src2, Evision.CUDA.GpuMat) and is_integer(flags) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:conjB, :stream])
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1),
      src2: Evision.Internal.Structurise.from_struct(src2),
      flags: Evision.Internal.Structurise.from_struct(flags)
    ]
    :evision_nif.cuda_mulSpectrums(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Performs a per-element multiplication of two Fourier spectrums.

  ##### Positional Arguments
  - **src1**: `Evision.Mat`.

    First spectrum.

  - **src2**: `Evision.Mat`.

    Second spectrum with the same size and type as a .

  - **flags**: `integer()`.

    Mock parameter used for CPU/CUDA interfaces similarity.

  ##### Keyword Arguments
  - **conjB**: `bool`.

    Optional flag to specify if the second spectrum needs to be conjugated before the
    multiplication.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination spectrum.

  Only full (not packed) CV_32FC2 complex spectrums in the interleaved format are supported for now.
  @sa mulSpectrums

  Python prototype (for reference only):
  ```python3
  mulSpectrums(src1, src2, flags[, dst[, conjB[, stream]]]) -> dst
  ```
  #### Variant 2:
  Performs a per-element multiplication of two Fourier spectrums.

  ##### Positional Arguments
  - **src1**: `Evision.CUDA.GpuMat.t()`.

    First spectrum.

  - **src2**: `Evision.CUDA.GpuMat.t()`.

    Second spectrum with the same size and type as a .

  - **flags**: `integer()`.

    Mock parameter used for CPU/CUDA interfaces similarity.

  ##### Keyword Arguments
  - **conjB**: `bool`.

    Optional flag to specify if the second spectrum needs to be conjugated before the
    multiplication.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination spectrum.

  Only full (not packed) CV_32FC2 complex spectrums in the interleaved format are supported for now.
  @sa mulSpectrums

  Python prototype (for reference only):
  ```python3
  mulSpectrums(src1, src2, flags[, dst[, conjB[, stream]]]) -> dst
  ```

  """
  @spec mulSpectrums(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), integer()) :: Evision.Mat.t() | {:error, String.t()}
  def mulSpectrums(src1, src2, flags) when (is_struct(src1, Evision.Mat) or is_struct(src1, Nx.Tensor) or is_number(src1) or is_tuple(src1)) and (is_struct(src2, Evision.Mat) or is_struct(src2, Nx.Tensor) or is_number(src2) or is_tuple(src2)) and is_integer(flags)
  do
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1),
      src2: Evision.Internal.Structurise.from_struct(src2),
      flags: Evision.Internal.Structurise.from_struct(flags)
    ]
    :evision_nif.cuda_mulSpectrums(positional)
    |> to_struct()
  end
  @spec mulSpectrums(Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t(), integer()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def mulSpectrums(src1, src2, flags) when is_struct(src1, Evision.CUDA.GpuMat) and is_struct(src2, Evision.CUDA.GpuMat) and is_integer(flags)
  do
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1),
      src2: Evision.Internal.Structurise.from_struct(src2),
      flags: Evision.Internal.Structurise.from_struct(flags)
    ]
    :evision_nif.cuda_mulSpectrums(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Computes a matrix-matrix or matrix-scalar per-element product.

  ##### Positional Arguments
  - **src1**: `Evision.Mat`.

    First source matrix or scalar.

  - **src2**: `Evision.Mat`.

    Second source matrix or scalar.

  ##### Keyword Arguments
  - **scale**: `double`.

    Optional scale factor.

  - **dtype**: `integer()`.

    Optional depth of the output array.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination matrix that has the same size and number of channels as the input array(s).
    The depth is defined by dtype or src1 depth.

  @sa multiply

  Python prototype (for reference only):
  ```python3
  multiply(src1, src2[, dst[, scale[, dtype[, stream]]]]) -> dst
  ```
  #### Variant 2:
  Computes a matrix-matrix or matrix-scalar per-element product.

  ##### Positional Arguments
  - **src1**: `Evision.CUDA.GpuMat.t()`.

    First source matrix or scalar.

  - **src2**: `Evision.CUDA.GpuMat.t()`.

    Second source matrix or scalar.

  ##### Keyword Arguments
  - **scale**: `double`.

    Optional scale factor.

  - **dtype**: `integer()`.

    Optional depth of the output array.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination matrix that has the same size and number of channels as the input array(s).
    The depth is defined by dtype or src1 depth.

  @sa multiply

  Python prototype (for reference only):
  ```python3
  multiply(src1, src2[, dst[, scale[, dtype[, stream]]]]) -> dst
  ```

  """
  @spec multiply(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:dtype, term()} | {:scale, term()} | {:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def multiply(src1, src2, opts) when (is_struct(src1, Evision.Mat) or is_struct(src1, Nx.Tensor) or is_number(src1) or is_tuple(src1)) and (is_struct(src2, Evision.Mat) or is_struct(src2, Nx.Tensor) or is_number(src2) or is_tuple(src2)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:dtype, :scale, :stream])
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1),
      src2: Evision.Internal.Structurise.from_struct(src2)
    ]
    :evision_nif.cuda_multiply(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec multiply(Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t(), [{:dtype, term()} | {:scale, term()} | {:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def multiply(src1, src2, opts) when is_struct(src1, Evision.CUDA.GpuMat) and is_struct(src2, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:dtype, :scale, :stream])
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1),
      src2: Evision.Internal.Structurise.from_struct(src2)
    ]
    :evision_nif.cuda_multiply(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Computes a matrix-matrix or matrix-scalar per-element product.

  ##### Positional Arguments
  - **src1**: `Evision.Mat`.

    First source matrix or scalar.

  - **src2**: `Evision.Mat`.

    Second source matrix or scalar.

  ##### Keyword Arguments
  - **scale**: `double`.

    Optional scale factor.

  - **dtype**: `integer()`.

    Optional depth of the output array.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination matrix that has the same size and number of channels as the input array(s).
    The depth is defined by dtype or src1 depth.

  @sa multiply

  Python prototype (for reference only):
  ```python3
  multiply(src1, src2[, dst[, scale[, dtype[, stream]]]]) -> dst
  ```
  #### Variant 2:
  Computes a matrix-matrix or matrix-scalar per-element product.

  ##### Positional Arguments
  - **src1**: `Evision.CUDA.GpuMat.t()`.

    First source matrix or scalar.

  - **src2**: `Evision.CUDA.GpuMat.t()`.

    Second source matrix or scalar.

  ##### Keyword Arguments
  - **scale**: `double`.

    Optional scale factor.

  - **dtype**: `integer()`.

    Optional depth of the output array.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination matrix that has the same size and number of channels as the input array(s).
    The depth is defined by dtype or src1 depth.

  @sa multiply

  Python prototype (for reference only):
  ```python3
  multiply(src1, src2[, dst[, scale[, dtype[, stream]]]]) -> dst
  ```

  """
  @spec multiply(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def multiply(src1, src2) when (is_struct(src1, Evision.Mat) or is_struct(src1, Nx.Tensor) or is_number(src1) or is_tuple(src1)) and (is_struct(src2, Evision.Mat) or is_struct(src2, Nx.Tensor) or is_number(src2) or is_tuple(src2))
  do
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1),
      src2: Evision.Internal.Structurise.from_struct(src2)
    ]
    :evision_nif.cuda_multiply(positional)
    |> to_struct()
  end
  @spec multiply(Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def multiply(src1, src2) when is_struct(src1, Evision.CUDA.GpuMat) and is_struct(src2, Evision.CUDA.GpuMat)
  do
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1),
      src2: Evision.Internal.Structurise.from_struct(src2)
    ]
    :evision_nif.cuda_multiply(positional)
    |> to_struct()
  end

  @doc """
  Performs pure non local means denoising without any simplification, and thus it is not fast.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source image. Supports only CV_8UC1, CV_8UC2 and CV_8UC3.

  - **h**: `float`.

    Filter sigma regulating filter strength for color.

  ##### Keyword Arguments
  - **search_window**: `integer()`.

    Size of search window.

  - **block_size**: `integer()`.

    Size of block used for computing weights.

  - **borderMode**: `integer()`.

    Border type. See borderInterpolate for details. BORDER_REFLECT101 ,
    BORDER_REPLICATE , BORDER_CONSTANT , BORDER_REFLECT and BORDER_WRAP are supported for now.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination image.

  @sa
  fastNlMeansDenoising

  Python prototype (for reference only):
  ```python3
  nonLocalMeans(src, h[, dst[, search_window[, block_size[, borderMode[, stream]]]]]) -> dst
  ```
  """
  @spec nonLocalMeans(Evision.CUDA.GpuMat.t(), number(), [{:block_size, term()} | {:borderMode, term()} | {:search_window, term()} | {:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def nonLocalMeans(src, h, opts) when is_struct(src, Evision.CUDA.GpuMat) and is_float(h) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:block_size, :borderMode, :search_window, :stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      h: Evision.Internal.Structurise.from_struct(h)
    ]
    :evision_nif.cuda_nonLocalMeans(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Performs pure non local means denoising without any simplification, and thus it is not fast.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source image. Supports only CV_8UC1, CV_8UC2 and CV_8UC3.

  - **h**: `float`.

    Filter sigma regulating filter strength for color.

  ##### Keyword Arguments
  - **search_window**: `integer()`.

    Size of search window.

  - **block_size**: `integer()`.

    Size of block used for computing weights.

  - **borderMode**: `integer()`.

    Border type. See borderInterpolate for details. BORDER_REFLECT101 ,
    BORDER_REPLICATE , BORDER_CONSTANT , BORDER_REFLECT and BORDER_WRAP are supported for now.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination image.

  @sa
  fastNlMeansDenoising

  Python prototype (for reference only):
  ```python3
  nonLocalMeans(src, h[, dst[, search_window[, block_size[, borderMode[, stream]]]]]) -> dst
  ```
  """
  @spec nonLocalMeans(Evision.CUDA.GpuMat.t(), number()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def nonLocalMeans(src, h) when is_struct(src, Evision.CUDA.GpuMat) and is_float(h)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      h: Evision.Internal.Structurise.from_struct(h)
    ]
    :evision_nif.cuda_nonLocalMeans(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Returns the difference of two matrices.

  ##### Positional Arguments
  - **src1**: `Evision.Mat`.

    Source matrix. Any matrices except 64F are supported.

  - **src2**: `Evision.Mat`.

    Second source matrix (if any) with the same size and type as src1.

  ##### Keyword Arguments
  - **normType**: `integer()`.

    Norm type. NORM_L1 , NORM_L2 , and NORM_INF are supported for now.

  ##### Return
  - **retval**: `double`

  @sa norm

  Python prototype (for reference only):
  ```python3
  norm(src1, src2[, normType]) -> retval
  ```
  #### Variant 2:
  Returns the difference of two matrices.

  ##### Positional Arguments
  - **src1**: `Evision.CUDA.GpuMat.t()`.

    Source matrix. Any matrices except 64F are supported.

  - **src2**: `Evision.CUDA.GpuMat.t()`.

    Second source matrix (if any) with the same size and type as src1.

  ##### Keyword Arguments
  - **normType**: `integer()`.

    Norm type. NORM_L1 , NORM_L2 , and NORM_INF are supported for now.

  ##### Return
  - **retval**: `double`

  @sa norm

  Python prototype (for reference only):
  ```python3
  norm(src1, src2[, normType]) -> retval
  ```
  #### Variant 3:
  Returns the norm of a matrix (or difference of two matrices).

  ##### Positional Arguments
  - **src1**: `Evision.Mat`.

    Source matrix. Any matrices except 64F are supported.

  - **normType**: `integer()`.

    Norm type. NORM_L1 , NORM_L2 , and NORM_INF are supported for now.

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.

    optional operation mask; it must have the same size as src1 and CV_8UC1 type.

  ##### Return
  - **retval**: `double`

  @sa norm

  Python prototype (for reference only):
  ```python3
  norm(src1, normType[, mask]) -> retval
  ```
  #### Variant 4:
  Returns the norm of a matrix (or difference of two matrices).

  ##### Positional Arguments
  - **src1**: `Evision.CUDA.GpuMat.t()`.

    Source matrix. Any matrices except 64F are supported.

  - **normType**: `integer()`.

    Norm type. NORM_L1 , NORM_L2 , and NORM_INF are supported for now.

  ##### Keyword Arguments
  - **mask**: `Evision.CUDA.GpuMat.t()`.

    optional operation mask; it must have the same size as src1 and CV_8UC1 type.

  ##### Return
  - **retval**: `double`

  @sa norm

  Python prototype (for reference only):
  ```python3
  norm(src1, normType[, mask]) -> retval
  ```

  """
  @spec norm(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:normType, term()}] | nil) :: number() | {:error, String.t()}
  def norm(src1, src2, opts) when (is_struct(src1, Evision.Mat) or is_struct(src1, Nx.Tensor) or is_number(src1) or is_tuple(src1)) and (is_struct(src2, Evision.Mat) or is_struct(src2, Nx.Tensor) or is_number(src2) or is_tuple(src2)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:normType])
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1),
      src2: Evision.Internal.Structurise.from_struct(src2)
    ]
    :evision_nif.cuda_norm(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec norm(Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t(), [{:normType, term()}] | nil) :: number() | {:error, String.t()}
  def norm(src1, src2, opts) when is_struct(src1, Evision.CUDA.GpuMat) and is_struct(src2, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:normType])
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1),
      src2: Evision.Internal.Structurise.from_struct(src2)
    ]
    :evision_nif.cuda_norm(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec norm(Evision.Mat.maybe_mat_in(), integer(), [{:mask, term()}] | nil) :: number() | {:error, String.t()}
  def norm(src1, normType, opts) when (is_struct(src1, Evision.Mat) or is_struct(src1, Nx.Tensor) or is_number(src1) or is_tuple(src1)) and is_integer(normType) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:mask])
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1),
      normType: Evision.Internal.Structurise.from_struct(normType)
    ]
    :evision_nif.cuda_norm(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec norm(Evision.CUDA.GpuMat.t(), integer(), [{:mask, term()}] | nil) :: number() | {:error, String.t()}
  def norm(src1, normType, opts) when is_struct(src1, Evision.CUDA.GpuMat) and is_integer(normType) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:mask])
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1),
      normType: Evision.Internal.Structurise.from_struct(normType)
    ]
    :evision_nif.cuda_norm(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Returns the difference of two matrices.

  ##### Positional Arguments
  - **src1**: `Evision.Mat`.

    Source matrix. Any matrices except 64F are supported.

  - **src2**: `Evision.Mat`.

    Second source matrix (if any) with the same size and type as src1.

  ##### Keyword Arguments
  - **normType**: `integer()`.

    Norm type. NORM_L1 , NORM_L2 , and NORM_INF are supported for now.

  ##### Return
  - **retval**: `double`

  @sa norm

  Python prototype (for reference only):
  ```python3
  norm(src1, src2[, normType]) -> retval
  ```
  #### Variant 2:
  Returns the difference of two matrices.

  ##### Positional Arguments
  - **src1**: `Evision.CUDA.GpuMat.t()`.

    Source matrix. Any matrices except 64F are supported.

  - **src2**: `Evision.CUDA.GpuMat.t()`.

    Second source matrix (if any) with the same size and type as src1.

  ##### Keyword Arguments
  - **normType**: `integer()`.

    Norm type. NORM_L1 , NORM_L2 , and NORM_INF are supported for now.

  ##### Return
  - **retval**: `double`

  @sa norm

  Python prototype (for reference only):
  ```python3
  norm(src1, src2[, normType]) -> retval
  ```
  #### Variant 3:
  Returns the norm of a matrix (or difference of two matrices).

  ##### Positional Arguments
  - **src1**: `Evision.Mat`.

    Source matrix. Any matrices except 64F are supported.

  - **normType**: `integer()`.

    Norm type. NORM_L1 , NORM_L2 , and NORM_INF are supported for now.

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.

    optional operation mask; it must have the same size as src1 and CV_8UC1 type.

  ##### Return
  - **retval**: `double`

  @sa norm

  Python prototype (for reference only):
  ```python3
  norm(src1, normType[, mask]) -> retval
  ```
  #### Variant 4:
  Returns the norm of a matrix (or difference of two matrices).

  ##### Positional Arguments
  - **src1**: `Evision.CUDA.GpuMat.t()`.

    Source matrix. Any matrices except 64F are supported.

  - **normType**: `integer()`.

    Norm type. NORM_L1 , NORM_L2 , and NORM_INF are supported for now.

  ##### Keyword Arguments
  - **mask**: `Evision.CUDA.GpuMat.t()`.

    optional operation mask; it must have the same size as src1 and CV_8UC1 type.

  ##### Return
  - **retval**: `double`

  @sa norm

  Python prototype (for reference only):
  ```python3
  norm(src1, normType[, mask]) -> retval
  ```

  """
  @spec norm(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: number() | {:error, String.t()}
  def norm(src1, src2) when (is_struct(src1, Evision.Mat) or is_struct(src1, Nx.Tensor) or is_number(src1) or is_tuple(src1)) and (is_struct(src2, Evision.Mat) or is_struct(src2, Nx.Tensor) or is_number(src2) or is_tuple(src2))
  do
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1),
      src2: Evision.Internal.Structurise.from_struct(src2)
    ]
    :evision_nif.cuda_norm(positional)
    |> to_struct()
  end
  @spec norm(Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t()) :: number() | {:error, String.t()}
  def norm(src1, src2) when is_struct(src1, Evision.CUDA.GpuMat) and is_struct(src2, Evision.CUDA.GpuMat)
  do
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1),
      src2: Evision.Internal.Structurise.from_struct(src2)
    ]
    :evision_nif.cuda_norm(positional)
    |> to_struct()
  end
  @spec norm(Evision.Mat.maybe_mat_in(), integer()) :: number() | {:error, String.t()}
  def norm(src1, normType) when (is_struct(src1, Evision.Mat) or is_struct(src1, Nx.Tensor) or is_number(src1) or is_tuple(src1)) and is_integer(normType)
  do
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1),
      normType: Evision.Internal.Structurise.from_struct(normType)
    ]
    :evision_nif.cuda_norm(positional)
    |> to_struct()
  end
  @spec norm(Evision.CUDA.GpuMat.t(), integer()) :: number() | {:error, String.t()}
  def norm(src1, normType) when is_struct(src1, Evision.CUDA.GpuMat) and is_integer(normType)
  do
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1),
      normType: Evision.Internal.Structurise.from_struct(normType)
    ]
    :evision_nif.cuda_norm(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Normalizes the norm or value range of an array.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Input array.

  - **alpha**: `double`.

    Norm value to normalize to or the lower range boundary in case of the range
    normalization.

  - **beta**: `double`.

    Upper range boundary in case of the range normalization; it is not used for the norm
    normalization.

  - **norm_type**: `integer()`.

    Normalization type ( NORM_MINMAX , NORM_L2 , NORM_L1 or NORM_INF ).

  - **dtype**: `integer()`.

    When negative, the output array has the same type as src; otherwise, it has the same
    number of channels as src and the depth =CV_MAT_DEPTH(dtype).

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.

    Optional operation mask.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Output array of the same size as src .

  @sa normalize

  Python prototype (for reference only):
  ```python3
  normalize(src, alpha, beta, norm_type, dtype[, dst[, mask[, stream]]]) -> dst
  ```
  #### Variant 2:
  Normalizes the norm or value range of an array.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Input array.

  - **alpha**: `double`.

    Norm value to normalize to or the lower range boundary in case of the range
    normalization.

  - **beta**: `double`.

    Upper range boundary in case of the range normalization; it is not used for the norm
    normalization.

  - **norm_type**: `integer()`.

    Normalization type ( NORM_MINMAX , NORM_L2 , NORM_L1 or NORM_INF ).

  - **dtype**: `integer()`.

    When negative, the output array has the same type as src; otherwise, it has the same
    number of channels as src and the depth =CV_MAT_DEPTH(dtype).

  ##### Keyword Arguments
  - **mask**: `Evision.CUDA.GpuMat.t()`.

    Optional operation mask.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Output array of the same size as src .

  @sa normalize

  Python prototype (for reference only):
  ```python3
  normalize(src, alpha, beta, norm_type, dtype[, dst[, mask[, stream]]]) -> dst
  ```

  """
  @spec normalize(Evision.Mat.maybe_mat_in(), number(), number(), integer(), integer(), [{:mask, term()} | {:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def normalize(src, alpha, beta, norm_type, dtype, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_number(alpha) and is_number(beta) and is_integer(norm_type) and is_integer(dtype) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:mask, :stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      alpha: Evision.Internal.Structurise.from_struct(alpha),
      beta: Evision.Internal.Structurise.from_struct(beta),
      norm_type: Evision.Internal.Structurise.from_struct(norm_type),
      dtype: Evision.Internal.Structurise.from_struct(dtype)
    ]
    :evision_nif.cuda_normalize(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec normalize(Evision.CUDA.GpuMat.t(), number(), number(), integer(), integer(), [{:mask, term()} | {:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def normalize(src, alpha, beta, norm_type, dtype, opts) when is_struct(src, Evision.CUDA.GpuMat) and is_number(alpha) and is_number(beta) and is_integer(norm_type) and is_integer(dtype) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:mask, :stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      alpha: Evision.Internal.Structurise.from_struct(alpha),
      beta: Evision.Internal.Structurise.from_struct(beta),
      norm_type: Evision.Internal.Structurise.from_struct(norm_type),
      dtype: Evision.Internal.Structurise.from_struct(dtype)
    ]
    :evision_nif.cuda_normalize(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Normalizes the norm or value range of an array.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Input array.

  - **alpha**: `double`.

    Norm value to normalize to or the lower range boundary in case of the range
    normalization.

  - **beta**: `double`.

    Upper range boundary in case of the range normalization; it is not used for the norm
    normalization.

  - **norm_type**: `integer()`.

    Normalization type ( NORM_MINMAX , NORM_L2 , NORM_L1 or NORM_INF ).

  - **dtype**: `integer()`.

    When negative, the output array has the same type as src; otherwise, it has the same
    number of channels as src and the depth =CV_MAT_DEPTH(dtype).

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.

    Optional operation mask.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Output array of the same size as src .

  @sa normalize

  Python prototype (for reference only):
  ```python3
  normalize(src, alpha, beta, norm_type, dtype[, dst[, mask[, stream]]]) -> dst
  ```
  #### Variant 2:
  Normalizes the norm or value range of an array.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Input array.

  - **alpha**: `double`.

    Norm value to normalize to or the lower range boundary in case of the range
    normalization.

  - **beta**: `double`.

    Upper range boundary in case of the range normalization; it is not used for the norm
    normalization.

  - **norm_type**: `integer()`.

    Normalization type ( NORM_MINMAX , NORM_L2 , NORM_L1 or NORM_INF ).

  - **dtype**: `integer()`.

    When negative, the output array has the same type as src; otherwise, it has the same
    number of channels as src and the depth =CV_MAT_DEPTH(dtype).

  ##### Keyword Arguments
  - **mask**: `Evision.CUDA.GpuMat.t()`.

    Optional operation mask.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Output array of the same size as src .

  @sa normalize

  Python prototype (for reference only):
  ```python3
  normalize(src, alpha, beta, norm_type, dtype[, dst[, mask[, stream]]]) -> dst
  ```

  """
  @spec normalize(Evision.Mat.maybe_mat_in(), number(), number(), integer(), integer()) :: Evision.Mat.t() | {:error, String.t()}
  def normalize(src, alpha, beta, norm_type, dtype) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_number(alpha) and is_number(beta) and is_integer(norm_type) and is_integer(dtype)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      alpha: Evision.Internal.Structurise.from_struct(alpha),
      beta: Evision.Internal.Structurise.from_struct(beta),
      norm_type: Evision.Internal.Structurise.from_struct(norm_type),
      dtype: Evision.Internal.Structurise.from_struct(dtype)
    ]
    :evision_nif.cuda_normalize(positional)
    |> to_struct()
  end
  @spec normalize(Evision.CUDA.GpuMat.t(), number(), number(), integer(), integer()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def normalize(src, alpha, beta, norm_type, dtype) when is_struct(src, Evision.CUDA.GpuMat) and is_number(alpha) and is_number(beta) and is_integer(norm_type) and is_integer(dtype)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      alpha: Evision.Internal.Structurise.from_struct(alpha),
      beta: Evision.Internal.Structurise.from_struct(beta),
      norm_type: Evision.Internal.Structurise.from_struct(norm_type),
      dtype: Evision.Internal.Structurise.from_struct(dtype)
    ]
    :evision_nif.cuda_normalize(positional)
    |> to_struct()
  end

  @doc """
  Returns the number of image moments less than or equal to the largest image moments \\a order.

  ##### Positional Arguments
  - **order**: `MomentsOrder`.

    Order of largest moments to calculate with lower order moments requiring less computation.

  ##### Return
  - **retval**: `integer()`

  @returns number of image moments.
  @sa cuda::spatialMoments, cuda::moments, cuda::MomentsOrder

  Python prototype (for reference only):
  ```python3
  numMoments(order) -> retval
  ```
  """
  @spec numMoments(Evision.CUDA.MomentsOrder.t()) :: integer() | {:error, String.t()}
  def numMoments(order) do
    positional = [
      order: Evision.Internal.Structurise.from_struct(order)
    ]
    :evision_nif.cuda_numMoments(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Computes polar angles of complex matrix elements.

  ##### Positional Arguments
  - **x**: `Evision.Mat`.

    Source matrix containing real components ( CV_32FC1 ).

  - **y**: `Evision.Mat`.

    Source matrix containing imaginary components ( CV_32FC1 ).

  ##### Keyword Arguments
  - **angleInDegrees**: `bool`.

    Flag for angles that must be evaluated in degrees.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **angle**: `Evision.Mat.t()`.

    Destination matrix of angles ( CV_32FC1 ).

  @sa phase

  Python prototype (for reference only):
  ```python3
  phase(x, y[, angle[, angleInDegrees[, stream]]]) -> angle
  ```
  #### Variant 2:
  Computes polar angles of complex matrix elements.

  ##### Positional Arguments
  - **x**: `Evision.CUDA.GpuMat.t()`.

    Source matrix containing real components ( CV_32FC1 ).

  - **y**: `Evision.CUDA.GpuMat.t()`.

    Source matrix containing imaginary components ( CV_32FC1 ).

  ##### Keyword Arguments
  - **angleInDegrees**: `bool`.

    Flag for angles that must be evaluated in degrees.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **angle**: `Evision.CUDA.GpuMat.t()`.

    Destination matrix of angles ( CV_32FC1 ).

  @sa phase

  Python prototype (for reference only):
  ```python3
  phase(x, y[, angle[, angleInDegrees[, stream]]]) -> angle
  ```

  """
  @spec phase(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:angleInDegrees, term()} | {:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def phase(x, y, opts) when (is_struct(x, Evision.Mat) or is_struct(x, Nx.Tensor) or is_number(x) or is_tuple(x)) and (is_struct(y, Evision.Mat) or is_struct(y, Nx.Tensor) or is_number(y) or is_tuple(y)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:angleInDegrees, :stream])
    positional = [
      x: Evision.Internal.Structurise.from_struct(x),
      y: Evision.Internal.Structurise.from_struct(y)
    ]
    :evision_nif.cuda_phase(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec phase(Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t(), [{:angleInDegrees, term()} | {:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def phase(x, y, opts) when is_struct(x, Evision.CUDA.GpuMat) and is_struct(y, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:angleInDegrees, :stream])
    positional = [
      x: Evision.Internal.Structurise.from_struct(x),
      y: Evision.Internal.Structurise.from_struct(y)
    ]
    :evision_nif.cuda_phase(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Computes polar angles of complex matrix elements.

  ##### Positional Arguments
  - **x**: `Evision.Mat`.

    Source matrix containing real components ( CV_32FC1 ).

  - **y**: `Evision.Mat`.

    Source matrix containing imaginary components ( CV_32FC1 ).

  ##### Keyword Arguments
  - **angleInDegrees**: `bool`.

    Flag for angles that must be evaluated in degrees.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **angle**: `Evision.Mat.t()`.

    Destination matrix of angles ( CV_32FC1 ).

  @sa phase

  Python prototype (for reference only):
  ```python3
  phase(x, y[, angle[, angleInDegrees[, stream]]]) -> angle
  ```
  #### Variant 2:
  Computes polar angles of complex matrix elements.

  ##### Positional Arguments
  - **x**: `Evision.CUDA.GpuMat.t()`.

    Source matrix containing real components ( CV_32FC1 ).

  - **y**: `Evision.CUDA.GpuMat.t()`.

    Source matrix containing imaginary components ( CV_32FC1 ).

  ##### Keyword Arguments
  - **angleInDegrees**: `bool`.

    Flag for angles that must be evaluated in degrees.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **angle**: `Evision.CUDA.GpuMat.t()`.

    Destination matrix of angles ( CV_32FC1 ).

  @sa phase

  Python prototype (for reference only):
  ```python3
  phase(x, y[, angle[, angleInDegrees[, stream]]]) -> angle
  ```

  """
  @spec phase(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def phase(x, y) when (is_struct(x, Evision.Mat) or is_struct(x, Nx.Tensor) or is_number(x) or is_tuple(x)) and (is_struct(y, Evision.Mat) or is_struct(y, Nx.Tensor) or is_number(y) or is_tuple(y))
  do
    positional = [
      x: Evision.Internal.Structurise.from_struct(x),
      y: Evision.Internal.Structurise.from_struct(y)
    ]
    :evision_nif.cuda_phase(positional)
    |> to_struct()
  end
  @spec phase(Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def phase(x, y) when is_struct(x, Evision.CUDA.GpuMat) and is_struct(y, Evision.CUDA.GpuMat)
  do
    positional = [
      x: Evision.Internal.Structurise.from_struct(x),
      y: Evision.Internal.Structurise.from_struct(y)
    ]
    :evision_nif.cuda_phase(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Converts polar coordinates into Cartesian.

  ##### Positional Arguments
  - **magnitude**: `Evision.Mat`.

    Source matrix containing magnitudes ( CV_32FC1 or CV_64FC1 ).

  - **angle**: `Evision.Mat`.

    Source matrix containing angles ( same type as magnitude ).

  ##### Keyword Arguments
  - **angleInDegrees**: `bool`.

    Flag that indicates angles in degrees.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **x**: `Evision.Mat.t()`.

    Destination matrix of real components ( same type as magnitude ).

  - **y**: `Evision.Mat.t()`.

    Destination matrix of imaginary components ( same type as magnitude ).

  Python prototype (for reference only):
  ```python3
  polarToCart(magnitude, angle[, x[, y[, angleInDegrees[, stream]]]]) -> x, y
  ```
  #### Variant 2:
  Converts polar coordinates into Cartesian.

  ##### Positional Arguments
  - **magnitude**: `Evision.CUDA.GpuMat.t()`.

    Source matrix containing magnitudes ( CV_32FC1 or CV_64FC1 ).

  - **angle**: `Evision.CUDA.GpuMat.t()`.

    Source matrix containing angles ( same type as magnitude ).

  ##### Keyword Arguments
  - **angleInDegrees**: `bool`.

    Flag that indicates angles in degrees.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **x**: `Evision.CUDA.GpuMat.t()`.

    Destination matrix of real components ( same type as magnitude ).

  - **y**: `Evision.CUDA.GpuMat.t()`.

    Destination matrix of imaginary components ( same type as magnitude ).

  Python prototype (for reference only):
  ```python3
  polarToCart(magnitude, angle[, x[, y[, angleInDegrees[, stream]]]]) -> x, y
  ```

  """
  @spec polarToCart(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:angleInDegrees, term()} | {:stream, term()}] | nil) :: {Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def polarToCart(magnitude, angle, opts) when (is_struct(magnitude, Evision.Mat) or is_struct(magnitude, Nx.Tensor) or is_number(magnitude) or is_tuple(magnitude)) and (is_struct(angle, Evision.Mat) or is_struct(angle, Nx.Tensor) or is_number(angle) or is_tuple(angle)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:angleInDegrees, :stream])
    positional = [
      magnitude: Evision.Internal.Structurise.from_struct(magnitude),
      angle: Evision.Internal.Structurise.from_struct(angle)
    ]
    :evision_nif.cuda_polarToCart(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec polarToCart(Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t(), [{:angleInDegrees, term()} | {:stream, term()}] | nil) :: {Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t()} | {:error, String.t()}
  def polarToCart(magnitude, angle, opts) when is_struct(magnitude, Evision.CUDA.GpuMat) and is_struct(angle, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:angleInDegrees, :stream])
    positional = [
      magnitude: Evision.Internal.Structurise.from_struct(magnitude),
      angle: Evision.Internal.Structurise.from_struct(angle)
    ]
    :evision_nif.cuda_polarToCart(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Converts polar coordinates into Cartesian.

  ##### Positional Arguments
  - **magnitude**: `Evision.Mat`.

    Source matrix containing magnitudes ( CV_32FC1 or CV_64FC1 ).

  - **angle**: `Evision.Mat`.

    Source matrix containing angles ( same type as magnitude ).

  ##### Keyword Arguments
  - **angleInDegrees**: `bool`.

    Flag that indicates angles in degrees.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **x**: `Evision.Mat.t()`.

    Destination matrix of real components ( same type as magnitude ).

  - **y**: `Evision.Mat.t()`.

    Destination matrix of imaginary components ( same type as magnitude ).

  Python prototype (for reference only):
  ```python3
  polarToCart(magnitude, angle[, x[, y[, angleInDegrees[, stream]]]]) -> x, y
  ```
  #### Variant 2:
  Converts polar coordinates into Cartesian.

  ##### Positional Arguments
  - **magnitude**: `Evision.CUDA.GpuMat.t()`.

    Source matrix containing magnitudes ( CV_32FC1 or CV_64FC1 ).

  - **angle**: `Evision.CUDA.GpuMat.t()`.

    Source matrix containing angles ( same type as magnitude ).

  ##### Keyword Arguments
  - **angleInDegrees**: `bool`.

    Flag that indicates angles in degrees.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **x**: `Evision.CUDA.GpuMat.t()`.

    Destination matrix of real components ( same type as magnitude ).

  - **y**: `Evision.CUDA.GpuMat.t()`.

    Destination matrix of imaginary components ( same type as magnitude ).

  Python prototype (for reference only):
  ```python3
  polarToCart(magnitude, angle[, x[, y[, angleInDegrees[, stream]]]]) -> x, y
  ```

  """
  @spec polarToCart(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: {Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def polarToCart(magnitude, angle) when (is_struct(magnitude, Evision.Mat) or is_struct(magnitude, Nx.Tensor) or is_number(magnitude) or is_tuple(magnitude)) and (is_struct(angle, Evision.Mat) or is_struct(angle, Nx.Tensor) or is_number(angle) or is_tuple(angle))
  do
    positional = [
      magnitude: Evision.Internal.Structurise.from_struct(magnitude),
      angle: Evision.Internal.Structurise.from_struct(angle)
    ]
    :evision_nif.cuda_polarToCart(positional)
    |> to_struct()
  end
  @spec polarToCart(Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t()) :: {Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t()} | {:error, String.t()}
  def polarToCart(magnitude, angle) when is_struct(magnitude, Evision.CUDA.GpuMat) and is_struct(angle, Evision.CUDA.GpuMat)
  do
    positional = [
      magnitude: Evision.Internal.Structurise.from_struct(magnitude),
      angle: Evision.Internal.Structurise.from_struct(angle)
    ]
    :evision_nif.cuda_polarToCart(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Raises every matrix element to a power.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source matrix.

  - **power**: `double`.

    Exponent of power.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination matrix with the same size and type as src .

  The function pow raises every element of the input matrix to power :
  \\f[\\texttt{dst} (I) =  \\fork{\\texttt{src}(I)^power}{if \\texttt{power} is integer}{|\\texttt{src}(I)|^power}{otherwise}\\f]
  @sa pow

  Python prototype (for reference only):
  ```python3
  pow(src, power[, dst[, stream]]) -> dst
  ```
  #### Variant 2:
  Raises every matrix element to a power.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source matrix.

  - **power**: `double`.

    Exponent of power.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination matrix with the same size and type as src .

  The function pow raises every element of the input matrix to power :
  \\f[\\texttt{dst} (I) =  \\fork{\\texttt{src}(I)^power}{if \\texttt{power} is integer}{|\\texttt{src}(I)|^power}{otherwise}\\f]
  @sa pow

  Python prototype (for reference only):
  ```python3
  pow(src, power[, dst[, stream]]) -> dst
  ```

  """
  @spec pow(Evision.Mat.maybe_mat_in(), number(), [{:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def pow(src, power, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_number(power) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      power: Evision.Internal.Structurise.from_struct(power)
    ]
    :evision_nif.cuda_pow(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec pow(Evision.CUDA.GpuMat.t(), number(), [{:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def pow(src, power, opts) when is_struct(src, Evision.CUDA.GpuMat) and is_number(power) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      power: Evision.Internal.Structurise.from_struct(power)
    ]
    :evision_nif.cuda_pow(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Raises every matrix element to a power.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source matrix.

  - **power**: `double`.

    Exponent of power.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination matrix with the same size and type as src .

  The function pow raises every element of the input matrix to power :
  \\f[\\texttt{dst} (I) =  \\fork{\\texttt{src}(I)^power}{if \\texttt{power} is integer}{|\\texttt{src}(I)|^power}{otherwise}\\f]
  @sa pow

  Python prototype (for reference only):
  ```python3
  pow(src, power[, dst[, stream]]) -> dst
  ```
  #### Variant 2:
  Raises every matrix element to a power.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source matrix.

  - **power**: `double`.

    Exponent of power.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination matrix with the same size and type as src .

  The function pow raises every element of the input matrix to power :
  \\f[\\texttt{dst} (I) =  \\fork{\\texttt{src}(I)^power}{if \\texttt{power} is integer}{|\\texttt{src}(I)|^power}{otherwise}\\f]
  @sa pow

  Python prototype (for reference only):
  ```python3
  pow(src, power[, dst[, stream]]) -> dst
  ```

  """
  @spec pow(Evision.Mat.maybe_mat_in(), number()) :: Evision.Mat.t() | {:error, String.t()}
  def pow(src, power) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_number(power)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      power: Evision.Internal.Structurise.from_struct(power)
    ]
    :evision_nif.cuda_pow(positional)
    |> to_struct()
  end
  @spec pow(Evision.CUDA.GpuMat.t(), number()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def pow(src, power) when is_struct(src, Evision.CUDA.GpuMat) and is_number(power)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      power: Evision.Internal.Structurise.from_struct(power)
    ]
    :evision_nif.cuda_pow(positional)
    |> to_struct()
  end

  @doc """
  printCudaDeviceInfo

  ##### Positional Arguments
  - **device**: `integer()`

  Python prototype (for reference only):
  ```python3
  printCudaDeviceInfo(device) -> None
  ```
  """
  @spec printCudaDeviceInfo(integer()) :: :ok | {:error, String.t()}
  def printCudaDeviceInfo(device) when is_integer(device)
  do
    positional = [
      device: Evision.Internal.Structurise.from_struct(device)
    ]
    :evision_nif.cuda_printCudaDeviceInfo(positional)
    |> to_struct()
  end

  @doc """
  printShortCudaDeviceInfo

  ##### Positional Arguments
  - **device**: `integer()`

  Python prototype (for reference only):
  ```python3
  printShortCudaDeviceInfo(device) -> None
  ```
  """
  @spec printShortCudaDeviceInfo(integer()) :: :ok | {:error, String.t()}
  def printShortCudaDeviceInfo(device) when is_integer(device)
  do
    positional = [
      device: Evision.Internal.Structurise.from_struct(device)
    ]
    :evision_nif.cuda_printShortCudaDeviceInfo(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Smoothes an image and downsamples it.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source image.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination image. Will have Size((src.cols+1)/2, (src.rows+1)/2) size and the same
    type as src .

  @sa pyrDown

  Python prototype (for reference only):
  ```python3
  pyrDown(src[, dst[, stream]]) -> dst
  ```
  #### Variant 2:
  Smoothes an image and downsamples it.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source image.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination image. Will have Size((src.cols+1)/2, (src.rows+1)/2) size and the same
    type as src .

  @sa pyrDown

  Python prototype (for reference only):
  ```python3
  pyrDown(src[, dst[, stream]]) -> dst
  ```

  """
  @spec pyrDown(Evision.Mat.maybe_mat_in(), [{:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def pyrDown(src, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_pyrDown(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec pyrDown(Evision.CUDA.GpuMat.t(), [{:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def pyrDown(src, opts) when is_struct(src, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_pyrDown(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Smoothes an image and downsamples it.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source image.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination image. Will have Size((src.cols+1)/2, (src.rows+1)/2) size and the same
    type as src .

  @sa pyrDown

  Python prototype (for reference only):
  ```python3
  pyrDown(src[, dst[, stream]]) -> dst
  ```
  #### Variant 2:
  Smoothes an image and downsamples it.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source image.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination image. Will have Size((src.cols+1)/2, (src.rows+1)/2) size and the same
    type as src .

  @sa pyrDown

  Python prototype (for reference only):
  ```python3
  pyrDown(src[, dst[, stream]]) -> dst
  ```

  """
  @spec pyrDown(Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def pyrDown(src) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_pyrDown(positional)
    |> to_struct()
  end
  @spec pyrDown(Evision.CUDA.GpuMat.t()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def pyrDown(src) when is_struct(src, Evision.CUDA.GpuMat)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_pyrDown(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Upsamples an image and then smoothes it.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source image.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination image. Will have Size(src.cols\\*2, src.rows\\*2) size and the same type as
    src .

  Python prototype (for reference only):
  ```python3
  pyrUp(src[, dst[, stream]]) -> dst
  ```
  #### Variant 2:
  Upsamples an image and then smoothes it.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source image.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination image. Will have Size(src.cols\\*2, src.rows\\*2) size and the same type as
    src .

  Python prototype (for reference only):
  ```python3
  pyrUp(src[, dst[, stream]]) -> dst
  ```

  """
  @spec pyrUp(Evision.Mat.maybe_mat_in(), [{:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def pyrUp(src, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_pyrUp(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec pyrUp(Evision.CUDA.GpuMat.t(), [{:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def pyrUp(src, opts) when is_struct(src, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_pyrUp(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Upsamples an image and then smoothes it.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source image.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination image. Will have Size(src.cols\\*2, src.rows\\*2) size and the same type as
    src .

  Python prototype (for reference only):
  ```python3
  pyrUp(src[, dst[, stream]]) -> dst
  ```
  #### Variant 2:
  Upsamples an image and then smoothes it.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source image.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination image. Will have Size(src.cols\\*2, src.rows\\*2) size and the same type as
    src .

  Python prototype (for reference only):
  ```python3
  pyrUp(src[, dst[, stream]]) -> dst
  ```

  """
  @spec pyrUp(Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def pyrUp(src) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_pyrUp(positional)
    |> to_struct()
  end
  @spec pyrUp(Evision.CUDA.GpuMat.t()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def pyrUp(src) when is_struct(src, Evision.CUDA.GpuMat)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_pyrUp(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Computes a standard deviation of integral images.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source image. Only the CV_32SC1 type is supported.

  - **sqr**: `Evision.Mat`.

    Squared source image. Only the CV_32FC1 type is supported.

  - **rect**: `Rect`.

    Rectangular window.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination image with the same type and size as src.

  Python prototype (for reference only):
  ```python3
  rectStdDev(src, sqr, rect[, dst[, stream]]) -> dst
  ```
  #### Variant 2:
  Computes a standard deviation of integral images.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source image. Only the CV_32SC1 type is supported.

  - **sqr**: `Evision.CUDA.GpuMat.t()`.

    Squared source image. Only the CV_32FC1 type is supported.

  - **rect**: `Rect`.

    Rectangular window.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination image with the same type and size as src.

  Python prototype (for reference only):
  ```python3
  rectStdDev(src, sqr, rect[, dst[, stream]]) -> dst
  ```

  """
  @spec rectStdDev(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), {number(), number(), number(), number()}, [{:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def rectStdDev(src, sqr, rect, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (is_struct(sqr, Evision.Mat) or is_struct(sqr, Nx.Tensor) or is_number(sqr) or is_tuple(sqr)) and is_tuple(rect) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      sqr: Evision.Internal.Structurise.from_struct(sqr),
      rect: Evision.Internal.Structurise.from_struct(rect)
    ]
    :evision_nif.cuda_rectStdDev(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec rectStdDev(Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t(), {number(), number(), number(), number()}, [{:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def rectStdDev(src, sqr, rect, opts) when is_struct(src, Evision.CUDA.GpuMat) and is_struct(sqr, Evision.CUDA.GpuMat) and is_tuple(rect) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      sqr: Evision.Internal.Structurise.from_struct(sqr),
      rect: Evision.Internal.Structurise.from_struct(rect)
    ]
    :evision_nif.cuda_rectStdDev(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Computes a standard deviation of integral images.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source image. Only the CV_32SC1 type is supported.

  - **sqr**: `Evision.Mat`.

    Squared source image. Only the CV_32FC1 type is supported.

  - **rect**: `Rect`.

    Rectangular window.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination image with the same type and size as src.

  Python prototype (for reference only):
  ```python3
  rectStdDev(src, sqr, rect[, dst[, stream]]) -> dst
  ```
  #### Variant 2:
  Computes a standard deviation of integral images.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source image. Only the CV_32SC1 type is supported.

  - **sqr**: `Evision.CUDA.GpuMat.t()`.

    Squared source image. Only the CV_32FC1 type is supported.

  - **rect**: `Rect`.

    Rectangular window.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination image with the same type and size as src.

  Python prototype (for reference only):
  ```python3
  rectStdDev(src, sqr, rect[, dst[, stream]]) -> dst
  ```

  """
  @spec rectStdDev(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), {number(), number(), number(), number()}) :: Evision.Mat.t() | {:error, String.t()}
  def rectStdDev(src, sqr, rect) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (is_struct(sqr, Evision.Mat) or is_struct(sqr, Nx.Tensor) or is_number(sqr) or is_tuple(sqr)) and is_tuple(rect)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      sqr: Evision.Internal.Structurise.from_struct(sqr),
      rect: Evision.Internal.Structurise.from_struct(rect)
    ]
    :evision_nif.cuda_rectStdDev(positional)
    |> to_struct()
  end
  @spec rectStdDev(Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t(), {number(), number(), number(), number()}) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def rectStdDev(src, sqr, rect) when is_struct(src, Evision.CUDA.GpuMat) and is_struct(sqr, Evision.CUDA.GpuMat) and is_tuple(rect)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      sqr: Evision.Internal.Structurise.from_struct(sqr),
      rect: Evision.Internal.Structurise.from_struct(rect)
    ]
    :evision_nif.cuda_rectStdDev(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Reduces a matrix to a vector.

  ##### Positional Arguments
  - **mtx**: `Evision.Mat`.

    Source 2D matrix.

  - **dim**: `integer()`.

    Dimension index along which the matrix is reduced. 0 means that the matrix is reduced
    to a single row. 1 means that the matrix is reduced to a single column.

  - **reduceOp**: `integer()`.

    Reduction operation that could be one of the following:
    - **REDUCE_SUM** The output is the sum of all rows/columns of the matrix.
    - **REDUCE_AVG** The output is the mean vector of all rows/columns of the matrix.
    - **REDUCE_MAX** The output is the maximum (column/row-wise) of all rows/columns of the
      matrix.
    - **REDUCE_MIN** The output is the minimum (column/row-wise) of all rows/columns of the
      matrix.

  ##### Keyword Arguments
  - **dtype**: `integer()`.

    When it is negative, the destination vector will have the same type as the source
    matrix. Otherwise, its type will be CV_MAKE_TYPE(CV_MAT_DEPTH(dtype), mtx.channels()) .

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **vec**: `Evision.Mat.t()`.

    Destination vector. Its size and type is defined by dim and dtype parameters.

  The function reduce reduces the matrix to a vector by treating the matrix rows/columns as a set of
  1D vectors and performing the specified operation on the vectors until a single row/column is
  obtained. For example, the function can be used to compute horizontal and vertical projections of a
  raster image. In case of REDUCE_SUM and REDUCE_AVG , the output may have a larger element
  bit-depth to preserve accuracy. And multi-channel arrays are also supported in these two reduction
  modes.
  @sa reduce

  Python prototype (for reference only):
  ```python3
  reduce(mtx, dim, reduceOp[, vec[, dtype[, stream]]]) -> vec
  ```
  #### Variant 2:
  Reduces a matrix to a vector.

  ##### Positional Arguments
  - **mtx**: `Evision.CUDA.GpuMat.t()`.

    Source 2D matrix.

  - **dim**: `integer()`.

    Dimension index along which the matrix is reduced. 0 means that the matrix is reduced
    to a single row. 1 means that the matrix is reduced to a single column.

  - **reduceOp**: `integer()`.

    Reduction operation that could be one of the following:
    - **REDUCE_SUM** The output is the sum of all rows/columns of the matrix.
    - **REDUCE_AVG** The output is the mean vector of all rows/columns of the matrix.
    - **REDUCE_MAX** The output is the maximum (column/row-wise) of all rows/columns of the
      matrix.
    - **REDUCE_MIN** The output is the minimum (column/row-wise) of all rows/columns of the
      matrix.

  ##### Keyword Arguments
  - **dtype**: `integer()`.

    When it is negative, the destination vector will have the same type as the source
    matrix. Otherwise, its type will be CV_MAKE_TYPE(CV_MAT_DEPTH(dtype), mtx.channels()) .

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **vec**: `Evision.CUDA.GpuMat.t()`.

    Destination vector. Its size and type is defined by dim and dtype parameters.

  The function reduce reduces the matrix to a vector by treating the matrix rows/columns as a set of
  1D vectors and performing the specified operation on the vectors until a single row/column is
  obtained. For example, the function can be used to compute horizontal and vertical projections of a
  raster image. In case of REDUCE_SUM and REDUCE_AVG , the output may have a larger element
  bit-depth to preserve accuracy. And multi-channel arrays are also supported in these two reduction
  modes.
  @sa reduce

  Python prototype (for reference only):
  ```python3
  reduce(mtx, dim, reduceOp[, vec[, dtype[, stream]]]) -> vec
  ```

  """
  @spec reduce(Evision.Mat.maybe_mat_in(), integer(), integer(), [{:dtype, term()} | {:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def reduce(mtx, dim, reduceOp, opts) when (is_struct(mtx, Evision.Mat) or is_struct(mtx, Nx.Tensor) or is_number(mtx) or is_tuple(mtx)) and is_integer(dim) and is_integer(reduceOp) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:dtype, :stream])
    positional = [
      mtx: Evision.Internal.Structurise.from_struct(mtx),
      dim: Evision.Internal.Structurise.from_struct(dim),
      reduceOp: Evision.Internal.Structurise.from_struct(reduceOp)
    ]
    :evision_nif.cuda_reduce(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec reduce(Evision.CUDA.GpuMat.t(), integer(), integer(), [{:dtype, term()} | {:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def reduce(mtx, dim, reduceOp, opts) when is_struct(mtx, Evision.CUDA.GpuMat) and is_integer(dim) and is_integer(reduceOp) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:dtype, :stream])
    positional = [
      mtx: Evision.Internal.Structurise.from_struct(mtx),
      dim: Evision.Internal.Structurise.from_struct(dim),
      reduceOp: Evision.Internal.Structurise.from_struct(reduceOp)
    ]
    :evision_nif.cuda_reduce(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Reduces a matrix to a vector.

  ##### Positional Arguments
  - **mtx**: `Evision.Mat`.

    Source 2D matrix.

  - **dim**: `integer()`.

    Dimension index along which the matrix is reduced. 0 means that the matrix is reduced
    to a single row. 1 means that the matrix is reduced to a single column.

  - **reduceOp**: `integer()`.

    Reduction operation that could be one of the following:
    - **REDUCE_SUM** The output is the sum of all rows/columns of the matrix.
    - **REDUCE_AVG** The output is the mean vector of all rows/columns of the matrix.
    - **REDUCE_MAX** The output is the maximum (column/row-wise) of all rows/columns of the
      matrix.
    - **REDUCE_MIN** The output is the minimum (column/row-wise) of all rows/columns of the
      matrix.

  ##### Keyword Arguments
  - **dtype**: `integer()`.

    When it is negative, the destination vector will have the same type as the source
    matrix. Otherwise, its type will be CV_MAKE_TYPE(CV_MAT_DEPTH(dtype), mtx.channels()) .

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **vec**: `Evision.Mat.t()`.

    Destination vector. Its size and type is defined by dim and dtype parameters.

  The function reduce reduces the matrix to a vector by treating the matrix rows/columns as a set of
  1D vectors and performing the specified operation on the vectors until a single row/column is
  obtained. For example, the function can be used to compute horizontal and vertical projections of a
  raster image. In case of REDUCE_SUM and REDUCE_AVG , the output may have a larger element
  bit-depth to preserve accuracy. And multi-channel arrays are also supported in these two reduction
  modes.
  @sa reduce

  Python prototype (for reference only):
  ```python3
  reduce(mtx, dim, reduceOp[, vec[, dtype[, stream]]]) -> vec
  ```
  #### Variant 2:
  Reduces a matrix to a vector.

  ##### Positional Arguments
  - **mtx**: `Evision.CUDA.GpuMat.t()`.

    Source 2D matrix.

  - **dim**: `integer()`.

    Dimension index along which the matrix is reduced. 0 means that the matrix is reduced
    to a single row. 1 means that the matrix is reduced to a single column.

  - **reduceOp**: `integer()`.

    Reduction operation that could be one of the following:
    - **REDUCE_SUM** The output is the sum of all rows/columns of the matrix.
    - **REDUCE_AVG** The output is the mean vector of all rows/columns of the matrix.
    - **REDUCE_MAX** The output is the maximum (column/row-wise) of all rows/columns of the
      matrix.
    - **REDUCE_MIN** The output is the minimum (column/row-wise) of all rows/columns of the
      matrix.

  ##### Keyword Arguments
  - **dtype**: `integer()`.

    When it is negative, the destination vector will have the same type as the source
    matrix. Otherwise, its type will be CV_MAKE_TYPE(CV_MAT_DEPTH(dtype), mtx.channels()) .

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **vec**: `Evision.CUDA.GpuMat.t()`.

    Destination vector. Its size and type is defined by dim and dtype parameters.

  The function reduce reduces the matrix to a vector by treating the matrix rows/columns as a set of
  1D vectors and performing the specified operation on the vectors until a single row/column is
  obtained. For example, the function can be used to compute horizontal and vertical projections of a
  raster image. In case of REDUCE_SUM and REDUCE_AVG , the output may have a larger element
  bit-depth to preserve accuracy. And multi-channel arrays are also supported in these two reduction
  modes.
  @sa reduce

  Python prototype (for reference only):
  ```python3
  reduce(mtx, dim, reduceOp[, vec[, dtype[, stream]]]) -> vec
  ```

  """
  @spec reduce(Evision.Mat.maybe_mat_in(), integer(), integer()) :: Evision.Mat.t() | {:error, String.t()}
  def reduce(mtx, dim, reduceOp) when (is_struct(mtx, Evision.Mat) or is_struct(mtx, Nx.Tensor) or is_number(mtx) or is_tuple(mtx)) and is_integer(dim) and is_integer(reduceOp)
  do
    positional = [
      mtx: Evision.Internal.Structurise.from_struct(mtx),
      dim: Evision.Internal.Structurise.from_struct(dim),
      reduceOp: Evision.Internal.Structurise.from_struct(reduceOp)
    ]
    :evision_nif.cuda_reduce(positional)
    |> to_struct()
  end
  @spec reduce(Evision.CUDA.GpuMat.t(), integer(), integer()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def reduce(mtx, dim, reduceOp) when is_struct(mtx, Evision.CUDA.GpuMat) and is_integer(dim) and is_integer(reduceOp)
  do
    positional = [
      mtx: Evision.Internal.Structurise.from_struct(mtx),
      dim: Evision.Internal.Structurise.from_struct(dim),
      reduceOp: Evision.Internal.Structurise.from_struct(reduceOp)
    ]
    :evision_nif.cuda_reduce(positional)
    |> to_struct()
  end

  @doc """
  Page-locks the memory of matrix and maps it for the device(s).

  ##### Positional Arguments
  - **m**: `Evision.Mat`.

    Input matrix.

  Python prototype (for reference only):
  ```python3
  registerPageLocked(m) -> None
  ```
  """
  @spec registerPageLocked(Evision.Mat.maybe_mat_in()) :: :ok | {:error, String.t()}
  def registerPageLocked(m) when (is_struct(m, Evision.Mat) or is_struct(m, Nx.Tensor) or is_number(m) or is_tuple(m))
  do
    positional = [
      m: Evision.Internal.Structurise.from_struct(m)
    ]
    :evision_nif.cuda_registerPageLocked(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Applies a generic geometrical transformation to an image.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source image.

  - **xmap**: `Evision.Mat`.

    X values. Only CV_32FC1 type is supported.

  - **ymap**: `Evision.Mat`.

    Y values. Only CV_32FC1 type is supported.

  - **interpolation**: `integer()`.

    Interpolation method (see resize ). INTER_NEAREST , INTER_LINEAR and
    INTER_CUBIC are supported for now.

  ##### Keyword Arguments
  - **borderMode**: `integer()`.

    Pixel extrapolation method (see borderInterpolate ). BORDER_REFLECT101 ,
    BORDER_REPLICATE , BORDER_CONSTANT , BORDER_REFLECT and BORDER_WRAP are supported for now.

  - **borderValue**: `Evision.scalar()`.

    Value used in case of a constant border. By default, it is 0.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination image with the size the same as xmap and the type the same as src .

  The function transforms the source image using the specified map:
  \\f[\\texttt{dst} (x,y) =  \\texttt{src} (xmap(x,y), ymap(x,y))\\f]
  Values of pixels with non-integer coordinates are computed using the bilinear interpolation.
  @sa remap

  Python prototype (for reference only):
  ```python3
  remap(src, xmap, ymap, interpolation[, dst[, borderMode[, borderValue[, stream]]]]) -> dst
  ```
  #### Variant 2:
  Applies a generic geometrical transformation to an image.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source image.

  - **xmap**: `Evision.CUDA.GpuMat.t()`.

    X values. Only CV_32FC1 type is supported.

  - **ymap**: `Evision.CUDA.GpuMat.t()`.

    Y values. Only CV_32FC1 type is supported.

  - **interpolation**: `integer()`.

    Interpolation method (see resize ). INTER_NEAREST , INTER_LINEAR and
    INTER_CUBIC are supported for now.

  ##### Keyword Arguments
  - **borderMode**: `integer()`.

    Pixel extrapolation method (see borderInterpolate ). BORDER_REFLECT101 ,
    BORDER_REPLICATE , BORDER_CONSTANT , BORDER_REFLECT and BORDER_WRAP are supported for now.

  - **borderValue**: `Evision.scalar()`.

    Value used in case of a constant border. By default, it is 0.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination image with the size the same as xmap and the type the same as src .

  The function transforms the source image using the specified map:
  \\f[\\texttt{dst} (x,y) =  \\texttt{src} (xmap(x,y), ymap(x,y))\\f]
  Values of pixels with non-integer coordinates are computed using the bilinear interpolation.
  @sa remap

  Python prototype (for reference only):
  ```python3
  remap(src, xmap, ymap, interpolation[, dst[, borderMode[, borderValue[, stream]]]]) -> dst
  ```

  """
  @spec remap(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), integer(), [{:borderMode, term()} | {:borderValue, term()} | {:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def remap(src, xmap, ymap, interpolation, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (is_struct(xmap, Evision.Mat) or is_struct(xmap, Nx.Tensor) or is_number(xmap) or is_tuple(xmap)) and (is_struct(ymap, Evision.Mat) or is_struct(ymap, Nx.Tensor) or is_number(ymap) or is_tuple(ymap)) and is_integer(interpolation) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:borderMode, :borderValue, :stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      xmap: Evision.Internal.Structurise.from_struct(xmap),
      ymap: Evision.Internal.Structurise.from_struct(ymap),
      interpolation: Evision.Internal.Structurise.from_struct(interpolation)
    ]
    :evision_nif.cuda_remap(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec remap(Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t(), integer(), [{:borderMode, term()} | {:borderValue, term()} | {:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def remap(src, xmap, ymap, interpolation, opts) when is_struct(src, Evision.CUDA.GpuMat) and is_struct(xmap, Evision.CUDA.GpuMat) and is_struct(ymap, Evision.CUDA.GpuMat) and is_integer(interpolation) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:borderMode, :borderValue, :stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      xmap: Evision.Internal.Structurise.from_struct(xmap),
      ymap: Evision.Internal.Structurise.from_struct(ymap),
      interpolation: Evision.Internal.Structurise.from_struct(interpolation)
    ]
    :evision_nif.cuda_remap(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Applies a generic geometrical transformation to an image.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source image.

  - **xmap**: `Evision.Mat`.

    X values. Only CV_32FC1 type is supported.

  - **ymap**: `Evision.Mat`.

    Y values. Only CV_32FC1 type is supported.

  - **interpolation**: `integer()`.

    Interpolation method (see resize ). INTER_NEAREST , INTER_LINEAR and
    INTER_CUBIC are supported for now.

  ##### Keyword Arguments
  - **borderMode**: `integer()`.

    Pixel extrapolation method (see borderInterpolate ). BORDER_REFLECT101 ,
    BORDER_REPLICATE , BORDER_CONSTANT , BORDER_REFLECT and BORDER_WRAP are supported for now.

  - **borderValue**: `Evision.scalar()`.

    Value used in case of a constant border. By default, it is 0.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination image with the size the same as xmap and the type the same as src .

  The function transforms the source image using the specified map:
  \\f[\\texttt{dst} (x,y) =  \\texttt{src} (xmap(x,y), ymap(x,y))\\f]
  Values of pixels with non-integer coordinates are computed using the bilinear interpolation.
  @sa remap

  Python prototype (for reference only):
  ```python3
  remap(src, xmap, ymap, interpolation[, dst[, borderMode[, borderValue[, stream]]]]) -> dst
  ```
  #### Variant 2:
  Applies a generic geometrical transformation to an image.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source image.

  - **xmap**: `Evision.CUDA.GpuMat.t()`.

    X values. Only CV_32FC1 type is supported.

  - **ymap**: `Evision.CUDA.GpuMat.t()`.

    Y values. Only CV_32FC1 type is supported.

  - **interpolation**: `integer()`.

    Interpolation method (see resize ). INTER_NEAREST , INTER_LINEAR and
    INTER_CUBIC are supported for now.

  ##### Keyword Arguments
  - **borderMode**: `integer()`.

    Pixel extrapolation method (see borderInterpolate ). BORDER_REFLECT101 ,
    BORDER_REPLICATE , BORDER_CONSTANT , BORDER_REFLECT and BORDER_WRAP are supported for now.

  - **borderValue**: `Evision.scalar()`.

    Value used in case of a constant border. By default, it is 0.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination image with the size the same as xmap and the type the same as src .

  The function transforms the source image using the specified map:
  \\f[\\texttt{dst} (x,y) =  \\texttt{src} (xmap(x,y), ymap(x,y))\\f]
  Values of pixels with non-integer coordinates are computed using the bilinear interpolation.
  @sa remap

  Python prototype (for reference only):
  ```python3
  remap(src, xmap, ymap, interpolation[, dst[, borderMode[, borderValue[, stream]]]]) -> dst
  ```

  """
  @spec remap(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), integer()) :: Evision.Mat.t() | {:error, String.t()}
  def remap(src, xmap, ymap, interpolation) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (is_struct(xmap, Evision.Mat) or is_struct(xmap, Nx.Tensor) or is_number(xmap) or is_tuple(xmap)) and (is_struct(ymap, Evision.Mat) or is_struct(ymap, Nx.Tensor) or is_number(ymap) or is_tuple(ymap)) and is_integer(interpolation)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      xmap: Evision.Internal.Structurise.from_struct(xmap),
      ymap: Evision.Internal.Structurise.from_struct(ymap),
      interpolation: Evision.Internal.Structurise.from_struct(interpolation)
    ]
    :evision_nif.cuda_remap(positional)
    |> to_struct()
  end
  @spec remap(Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t(), integer()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def remap(src, xmap, ymap, interpolation) when is_struct(src, Evision.CUDA.GpuMat) and is_struct(xmap, Evision.CUDA.GpuMat) and is_struct(ymap, Evision.CUDA.GpuMat) and is_integer(interpolation)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      xmap: Evision.Internal.Structurise.from_struct(xmap),
      ymap: Evision.Internal.Structurise.from_struct(ymap),
      interpolation: Evision.Internal.Structurise.from_struct(interpolation)
    ]
    :evision_nif.cuda_remap(positional)
    |> to_struct()
  end

  @doc """
  Reprojects a disparity image to 3D space.

  ##### Positional Arguments
  - **disp**: `Evision.CUDA.GpuMat.t()`.

    Input single-channel 8-bit unsigned, 16-bit signed, 32-bit signed or 32-bit
    floating-point disparity image. If 16-bit signed format is used, the values are assumed to have no
    fractional bits.

  - **q**: `Evision.Mat`.

    \\f$4 \\times 4\\f$ perspective transformation matrix that can be obtained via stereoRectify .

  ##### Keyword Arguments
  - **dst_cn**: `integer()`.

    The number of channels for output image. Can be 3 or 4.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **xyzw**: `Evision.CUDA.GpuMat.t()`.

    Output 3- or 4-channel floating-point image of the same size as disp . Each element of
    xyzw(x,y) contains 3D coordinates (x,y,z) or (x,y,z,1) of the point (x,y) , computed from the
    disparity map.

  @sa reprojectImageTo3D

  Python prototype (for reference only):
  ```python3
  reprojectImageTo3D(disp, Q[, xyzw[, dst_cn[, stream]]]) -> xyzw
  ```
  """
  @spec reprojectImageTo3D(Evision.CUDA.GpuMat.t(), Evision.Mat.maybe_mat_in(), [{:dst_cn, term()} | {:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def reprojectImageTo3D(disp, q, opts) when is_struct(disp, Evision.CUDA.GpuMat) and (is_struct(q, Evision.Mat) or is_struct(q, Nx.Tensor) or is_number(q) or is_tuple(q)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:dst_cn, :stream])
    positional = [
      disp: Evision.Internal.Structurise.from_struct(disp),
      q: Evision.Internal.Structurise.from_struct(q)
    ]
    :evision_nif.cuda_reprojectImageTo3D(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Reprojects a disparity image to 3D space.

  ##### Positional Arguments
  - **disp**: `Evision.CUDA.GpuMat.t()`.

    Input single-channel 8-bit unsigned, 16-bit signed, 32-bit signed or 32-bit
    floating-point disparity image. If 16-bit signed format is used, the values are assumed to have no
    fractional bits.

  - **q**: `Evision.Mat`.

    \\f$4 \\times 4\\f$ perspective transformation matrix that can be obtained via stereoRectify .

  ##### Keyword Arguments
  - **dst_cn**: `integer()`.

    The number of channels for output image. Can be 3 or 4.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **xyzw**: `Evision.CUDA.GpuMat.t()`.

    Output 3- or 4-channel floating-point image of the same size as disp . Each element of
    xyzw(x,y) contains 3D coordinates (x,y,z) or (x,y,z,1) of the point (x,y) , computed from the
    disparity map.

  @sa reprojectImageTo3D

  Python prototype (for reference only):
  ```python3
  reprojectImageTo3D(disp, Q[, xyzw[, dst_cn[, stream]]]) -> xyzw
  ```
  """
  @spec reprojectImageTo3D(Evision.CUDA.GpuMat.t(), Evision.Mat.maybe_mat_in()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def reprojectImageTo3D(disp, q) when is_struct(disp, Evision.CUDA.GpuMat) and (is_struct(q, Evision.Mat) or is_struct(q, Nx.Tensor) or is_number(q) or is_tuple(q))
  do
    positional = [
      disp: Evision.Internal.Structurise.from_struct(disp),
      q: Evision.Internal.Structurise.from_struct(q)
    ]
    :evision_nif.cuda_reprojectImageTo3D(positional)
    |> to_struct()
  end

  @doc """
  Explicitly destroys and cleans up all resources associated with the current device in the current
  process.

  Any subsequent API call to this device will reinitialize the device.

  Python prototype (for reference only):
  ```python3
  resetDevice() -> None
  ```
  """
  @spec resetDevice() :: :ok | {:error, String.t()}
  def resetDevice() do
    positional = [
    ]
    :evision_nif.cuda_resetDevice(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Resizes an image.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source image.

  - **dsize**: `Size`.

    Destination image size. If it is zero, it is computed as:
    \\f[\\texttt{dsize = Size(round(fx*src.cols), round(fy*src.rows))}\\f]
    Either dsize or both fx and fy must be non-zero.

  ##### Keyword Arguments
  - **fx**: `double`.

    Scale factor along the horizontal axis. If it is zero, it is computed as:
    \\f[\\texttt{(double)dsize.width/src.cols}\\f]

  - **fy**: `double`.

    Scale factor along the vertical axis. If it is zero, it is computed as:
    \\f[\\texttt{(double)dsize.height/src.rows}\\f]

  - **interpolation**: `integer()`.

    Interpolation method. INTER_NEAREST , INTER_LINEAR and INTER_CUBIC are
    supported for now.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination image with the same type as src . The size is dsize (when it is non-zero)
    or the size is computed from src.size() , fx , and fy .

  @sa resize

  Python prototype (for reference only):
  ```python3
  resize(src, dsize[, dst[, fx[, fy[, interpolation[, stream]]]]]) -> dst
  ```
  #### Variant 2:
  Resizes an image.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source image.

  - **dsize**: `Size`.

    Destination image size. If it is zero, it is computed as:
    \\f[\\texttt{dsize = Size(round(fx*src.cols), round(fy*src.rows))}\\f]
    Either dsize or both fx and fy must be non-zero.

  ##### Keyword Arguments
  - **fx**: `double`.

    Scale factor along the horizontal axis. If it is zero, it is computed as:
    \\f[\\texttt{(double)dsize.width/src.cols}\\f]

  - **fy**: `double`.

    Scale factor along the vertical axis. If it is zero, it is computed as:
    \\f[\\texttt{(double)dsize.height/src.rows}\\f]

  - **interpolation**: `integer()`.

    Interpolation method. INTER_NEAREST , INTER_LINEAR and INTER_CUBIC are
    supported for now.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination image with the same type as src . The size is dsize (when it is non-zero)
    or the size is computed from src.size() , fx , and fy .

  @sa resize

  Python prototype (for reference only):
  ```python3
  resize(src, dsize[, dst[, fx[, fy[, interpolation[, stream]]]]]) -> dst
  ```

  """
  @spec resize(Evision.Mat.maybe_mat_in(), {number(), number()}, [{:fx, term()} | {:fy, term()} | {:interpolation, term()} | {:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def resize(src, dsize, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_tuple(dsize) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:fx, :fy, :interpolation, :stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      dsize: Evision.Internal.Structurise.from_struct(dsize)
    ]
    :evision_nif.cuda_resize(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec resize(Evision.CUDA.GpuMat.t(), {number(), number()}, [{:fx, term()} | {:fy, term()} | {:interpolation, term()} | {:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def resize(src, dsize, opts) when is_struct(src, Evision.CUDA.GpuMat) and is_tuple(dsize) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:fx, :fy, :interpolation, :stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      dsize: Evision.Internal.Structurise.from_struct(dsize)
    ]
    :evision_nif.cuda_resize(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Resizes an image.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source image.

  - **dsize**: `Size`.

    Destination image size. If it is zero, it is computed as:
    \\f[\\texttt{dsize = Size(round(fx*src.cols), round(fy*src.rows))}\\f]
    Either dsize or both fx and fy must be non-zero.

  ##### Keyword Arguments
  - **fx**: `double`.

    Scale factor along the horizontal axis. If it is zero, it is computed as:
    \\f[\\texttt{(double)dsize.width/src.cols}\\f]

  - **fy**: `double`.

    Scale factor along the vertical axis. If it is zero, it is computed as:
    \\f[\\texttt{(double)dsize.height/src.rows}\\f]

  - **interpolation**: `integer()`.

    Interpolation method. INTER_NEAREST , INTER_LINEAR and INTER_CUBIC are
    supported for now.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination image with the same type as src . The size is dsize (when it is non-zero)
    or the size is computed from src.size() , fx , and fy .

  @sa resize

  Python prototype (for reference only):
  ```python3
  resize(src, dsize[, dst[, fx[, fy[, interpolation[, stream]]]]]) -> dst
  ```
  #### Variant 2:
  Resizes an image.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source image.

  - **dsize**: `Size`.

    Destination image size. If it is zero, it is computed as:
    \\f[\\texttt{dsize = Size(round(fx*src.cols), round(fy*src.rows))}\\f]
    Either dsize or both fx and fy must be non-zero.

  ##### Keyword Arguments
  - **fx**: `double`.

    Scale factor along the horizontal axis. If it is zero, it is computed as:
    \\f[\\texttt{(double)dsize.width/src.cols}\\f]

  - **fy**: `double`.

    Scale factor along the vertical axis. If it is zero, it is computed as:
    \\f[\\texttt{(double)dsize.height/src.rows}\\f]

  - **interpolation**: `integer()`.

    Interpolation method. INTER_NEAREST , INTER_LINEAR and INTER_CUBIC are
    supported for now.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination image with the same type as src . The size is dsize (when it is non-zero)
    or the size is computed from src.size() , fx , and fy .

  @sa resize

  Python prototype (for reference only):
  ```python3
  resize(src, dsize[, dst[, fx[, fy[, interpolation[, stream]]]]]) -> dst
  ```

  """
  @spec resize(Evision.Mat.maybe_mat_in(), {number(), number()}) :: Evision.Mat.t() | {:error, String.t()}
  def resize(src, dsize) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_tuple(dsize)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      dsize: Evision.Internal.Structurise.from_struct(dsize)
    ]
    :evision_nif.cuda_resize(positional)
    |> to_struct()
  end
  @spec resize(Evision.CUDA.GpuMat.t(), {number(), number()}) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def resize(src, dsize) when is_struct(src, Evision.CUDA.GpuMat) and is_tuple(dsize)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      dsize: Evision.Internal.Structurise.from_struct(dsize)
    ]
    :evision_nif.cuda_resize(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Rotates an image around the origin (0,0) and then shifts it.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source image. Supports 1, 3 or 4 channels images with CV_8U , CV_16U or CV_32F
    depth.

  - **dsize**: `Size`.

    Size of the destination image.

  - **angle**: `double`.

    Angle of rotation in degrees.

  ##### Keyword Arguments
  - **xShift**: `double`.

    Shift along the horizontal axis.

  - **yShift**: `double`.

    Shift along the vertical axis.

  - **interpolation**: `integer()`.

    Interpolation method. Only INTER_NEAREST , INTER_LINEAR , and INTER_CUBIC
    are supported.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination image with the same type as src . The size is dsize .

  @sa cuda::warpAffine

  Python prototype (for reference only):
  ```python3
  rotate(src, dsize, angle[, dst[, xShift[, yShift[, interpolation[, stream]]]]]) -> dst
  ```
  #### Variant 2:
  Rotates an image around the origin (0,0) and then shifts it.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source image. Supports 1, 3 or 4 channels images with CV_8U , CV_16U or CV_32F
    depth.

  - **dsize**: `Size`.

    Size of the destination image.

  - **angle**: `double`.

    Angle of rotation in degrees.

  ##### Keyword Arguments
  - **xShift**: `double`.

    Shift along the horizontal axis.

  - **yShift**: `double`.

    Shift along the vertical axis.

  - **interpolation**: `integer()`.

    Interpolation method. Only INTER_NEAREST , INTER_LINEAR , and INTER_CUBIC
    are supported.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination image with the same type as src . The size is dsize .

  @sa cuda::warpAffine

  Python prototype (for reference only):
  ```python3
  rotate(src, dsize, angle[, dst[, xShift[, yShift[, interpolation[, stream]]]]]) -> dst
  ```

  """
  @spec rotate(Evision.Mat.maybe_mat_in(), {number(), number()}, number(), [{:interpolation, term()} | {:stream, term()} | {:xShift, term()} | {:yShift, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def rotate(src, dsize, angle, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_tuple(dsize) and is_number(angle) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:interpolation, :stream, :xShift, :yShift])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      dsize: Evision.Internal.Structurise.from_struct(dsize),
      angle: Evision.Internal.Structurise.from_struct(angle)
    ]
    :evision_nif.cuda_rotate(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec rotate(Evision.CUDA.GpuMat.t(), {number(), number()}, number(), [{:interpolation, term()} | {:stream, term()} | {:xShift, term()} | {:yShift, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def rotate(src, dsize, angle, opts) when is_struct(src, Evision.CUDA.GpuMat) and is_tuple(dsize) and is_number(angle) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:interpolation, :stream, :xShift, :yShift])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      dsize: Evision.Internal.Structurise.from_struct(dsize),
      angle: Evision.Internal.Structurise.from_struct(angle)
    ]
    :evision_nif.cuda_rotate(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Rotates an image around the origin (0,0) and then shifts it.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source image. Supports 1, 3 or 4 channels images with CV_8U , CV_16U or CV_32F
    depth.

  - **dsize**: `Size`.

    Size of the destination image.

  - **angle**: `double`.

    Angle of rotation in degrees.

  ##### Keyword Arguments
  - **xShift**: `double`.

    Shift along the horizontal axis.

  - **yShift**: `double`.

    Shift along the vertical axis.

  - **interpolation**: `integer()`.

    Interpolation method. Only INTER_NEAREST , INTER_LINEAR , and INTER_CUBIC
    are supported.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination image with the same type as src . The size is dsize .

  @sa cuda::warpAffine

  Python prototype (for reference only):
  ```python3
  rotate(src, dsize, angle[, dst[, xShift[, yShift[, interpolation[, stream]]]]]) -> dst
  ```
  #### Variant 2:
  Rotates an image around the origin (0,0) and then shifts it.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source image. Supports 1, 3 or 4 channels images with CV_8U , CV_16U or CV_32F
    depth.

  - **dsize**: `Size`.

    Size of the destination image.

  - **angle**: `double`.

    Angle of rotation in degrees.

  ##### Keyword Arguments
  - **xShift**: `double`.

    Shift along the horizontal axis.

  - **yShift**: `double`.

    Shift along the vertical axis.

  - **interpolation**: `integer()`.

    Interpolation method. Only INTER_NEAREST , INTER_LINEAR , and INTER_CUBIC
    are supported.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination image with the same type as src . The size is dsize .

  @sa cuda::warpAffine

  Python prototype (for reference only):
  ```python3
  rotate(src, dsize, angle[, dst[, xShift[, yShift[, interpolation[, stream]]]]]) -> dst
  ```

  """
  @spec rotate(Evision.Mat.maybe_mat_in(), {number(), number()}, number()) :: Evision.Mat.t() | {:error, String.t()}
  def rotate(src, dsize, angle) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_tuple(dsize) and is_number(angle)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      dsize: Evision.Internal.Structurise.from_struct(dsize),
      angle: Evision.Internal.Structurise.from_struct(angle)
    ]
    :evision_nif.cuda_rotate(positional)
    |> to_struct()
  end
  @spec rotate(Evision.CUDA.GpuMat.t(), {number(), number()}, number()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def rotate(src, dsize, angle) when is_struct(src, Evision.CUDA.GpuMat) and is_tuple(dsize) and is_number(angle)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      dsize: Evision.Internal.Structurise.from_struct(dsize),
      angle: Evision.Internal.Structurise.from_struct(angle)
    ]
    :evision_nif.cuda_rotate(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Performs pixel by pixel right shift of an image by a constant value.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source matrix. Supports 1, 3 and 4 channels images with integers elements.

  - **val**: `Evision.scalar()`.

    Constant values, one per channel.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination matrix with the same size and type as src .

  Python prototype (for reference only):
  ```python3
  rshift(src, val[, dst[, stream]]) -> dst
  ```
  #### Variant 2:
  Performs pixel by pixel right shift of an image by a constant value.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source matrix. Supports 1, 3 and 4 channels images with integers elements.

  - **val**: `Evision.scalar()`.

    Constant values, one per channel.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination matrix with the same size and type as src .

  Python prototype (for reference only):
  ```python3
  rshift(src, val[, dst[, stream]]) -> dst
  ```

  """
  @spec rshift(Evision.Mat.maybe_mat_in(), Evision.scalar(), [{:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def rshift(src, val, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (is_number(val) or is_tuple(val)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.cuda_rshift(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec rshift(Evision.CUDA.GpuMat.t(), Evision.scalar(), [{:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def rshift(src, val, opts) when is_struct(src, Evision.CUDA.GpuMat) and (is_number(val) or is_tuple(val)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.cuda_rshift(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Performs pixel by pixel right shift of an image by a constant value.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source matrix. Supports 1, 3 and 4 channels images with integers elements.

  - **val**: `Evision.scalar()`.

    Constant values, one per channel.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination matrix with the same size and type as src .

  Python prototype (for reference only):
  ```python3
  rshift(src, val[, dst[, stream]]) -> dst
  ```
  #### Variant 2:
  Performs pixel by pixel right shift of an image by a constant value.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source matrix. Supports 1, 3 and 4 channels images with integers elements.

  - **val**: `Evision.scalar()`.

    Constant values, one per channel.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination matrix with the same size and type as src .

  Python prototype (for reference only):
  ```python3
  rshift(src, val[, dst[, stream]]) -> dst
  ```

  """
  @spec rshift(Evision.Mat.maybe_mat_in(), Evision.scalar()) :: Evision.Mat.t() | {:error, String.t()}
  def rshift(src, val) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (is_number(val) or is_tuple(val))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.cuda_rshift(positional)
    |> to_struct()
  end
  @spec rshift(Evision.CUDA.GpuMat.t(), Evision.scalar()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def rshift(src, val) when is_struct(src, Evision.CUDA.GpuMat) and (is_number(val) or is_tuple(val))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.cuda_rshift(positional)
    |> to_struct()
  end

  @doc """
  setBufferPoolConfig

  ##### Positional Arguments
  - **deviceId**: `integer()`
  - **stackSize**: `size_t`
  - **stackCount**: `integer()`

  Python prototype (for reference only):
  ```python3
  setBufferPoolConfig(deviceId, stackSize, stackCount) -> None
  ```
  """
  @spec setBufferPoolConfig(integer(), integer(), integer()) :: :ok | {:error, String.t()}
  def setBufferPoolConfig(deviceId, stackSize, stackCount) when is_integer(deviceId) and is_integer(stackSize) and is_integer(stackCount)
  do
    positional = [
      deviceId: Evision.Internal.Structurise.from_struct(deviceId),
      stackSize: Evision.Internal.Structurise.from_struct(stackSize),
      stackCount: Evision.Internal.Structurise.from_struct(stackCount)
    ]
    :evision_nif.cuda_setBufferPoolConfig(positional)
    |> to_struct()
  end

  @doc """
  setBufferPoolUsage

  ##### Positional Arguments
  - **on**: `bool`

  Python prototype (for reference only):
  ```python3
  setBufferPoolUsage(on) -> None
  ```
  """
  @spec setBufferPoolUsage(boolean()) :: :ok | {:error, String.t()}
  def setBufferPoolUsage(on) when is_boolean(on)
  do
    positional = [
      on: Evision.Internal.Structurise.from_struct(on)
    ]
    :evision_nif.cuda_setBufferPoolUsage(positional)
    |> to_struct()
  end

  @doc """
  Sets a device and initializes it for the current thread.

  ##### Positional Arguments
  - **device**: `integer()`.

    System index of a CUDA device starting with 0.

  If the call of this function is omitted, a default device is initialized at the fist CUDA usage.

  Python prototype (for reference only):
  ```python3
  setDevice(device) -> None
  ```
  """
  @spec setDevice(integer()) :: :ok | {:error, String.t()}
  def setDevice(device) when is_integer(device)
  do
    positional = [
      device: Evision.Internal.Structurise.from_struct(device)
    ]
    :evision_nif.cuda_setDevice(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Calculates all of the spatial moments up to the 3rd order of a rasterized shape.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Raster image (single-channel 2D array).

  ##### Keyword Arguments
  - **binaryImage**: `bool`.

    If it is true, all non-zero image pixels are treated as 1's.

  - **order**: `MomentsOrder`.

    Order of largest moments to calculate with lower order moments requiring less computation.

  - **momentsType**: `integer()`.

    Precision to use when calculating moments. Available types are \\ref CV_32F and \\ref CV_64F with the performance of \\ref CV_32F an order of magnitude greater than \\ref CV_64F. If the image is small the accuracy from \\ref CV_32F can be equal or very close to \\ref CV_64F.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **moments**: `Evision.Mat.t()`.

  Asynchronous version of cuda::moments() which only calculates the spatial (not centralized or normalized) moments, up to the 3rd order, of a rasterized shape.
  Each moment is returned as a column entry in the 1D \\a moments array.

  **Note**: For maximum performance pre-allocate a 1D GpuMat for \\a moments of the correct type and size large enough to store the all the image moments of up to the desired \\a order. e.g. With \\a order === MomentsOrder::SECOND_ORDER_MOMENTS and \\a momentsType == \\ref CV_32F \\a moments can be allocated as
  ```
  GpuMat momentsDevice(1,numMoments(MomentsOrder::SECOND_ORDER_MOMENTS),CV_32F)
  ```
  The central and normalized moments can easily be calculated on the host by downloading the \\a moments array and using the cuda::convertSpatialMoments helper function. e.g.
  ```
  HostMem spatialMomentsHostMem(1, numMoments(MomentsOrder::SECOND_ORDER_MOMENTS), CV_32F);
  spatialMomentsDevice.download(spatialMomentsHostMem, stream);
  stream.waitForCompletion();
  Mat spatialMoments = spatialMomentsHostMem.createMatHeader();
  cv::Moments cvMoments = convertSpatialMoments<float>(spatialMoments, order);
  ```
  see the \\a CUDA_TEST_P(Moments, Async) test inside opencv_contrib_source_code/modules/cudaimgproc/test/test_moments.cpp for an example.
  @sa cuda::moments, cuda::convertSpatialMoments, cuda::numMoments, cuda::MomentsOrder

  Python prototype (for reference only):
  ```python3
  spatialMoments(src[, moments[, binaryImage[, order[, momentsType[, stream]]]]]) -> moments
  ```
  #### Variant 2:
  Calculates all of the spatial moments up to the 3rd order of a rasterized shape.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Raster image (single-channel 2D array).

  ##### Keyword Arguments
  - **binaryImage**: `bool`.

    If it is true, all non-zero image pixels are treated as 1's.

  - **order**: `MomentsOrder`.

    Order of largest moments to calculate with lower order moments requiring less computation.

  - **momentsType**: `integer()`.

    Precision to use when calculating moments. Available types are \\ref CV_32F and \\ref CV_64F with the performance of \\ref CV_32F an order of magnitude greater than \\ref CV_64F. If the image is small the accuracy from \\ref CV_32F can be equal or very close to \\ref CV_64F.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **moments**: `Evision.CUDA.GpuMat.t()`.

  Asynchronous version of cuda::moments() which only calculates the spatial (not centralized or normalized) moments, up to the 3rd order, of a rasterized shape.
  Each moment is returned as a column entry in the 1D \\a moments array.

  **Note**: For maximum performance pre-allocate a 1D GpuMat for \\a moments of the correct type and size large enough to store the all the image moments of up to the desired \\a order. e.g. With \\a order === MomentsOrder::SECOND_ORDER_MOMENTS and \\a momentsType == \\ref CV_32F \\a moments can be allocated as
  ```
  GpuMat momentsDevice(1,numMoments(MomentsOrder::SECOND_ORDER_MOMENTS),CV_32F)
  ```
  The central and normalized moments can easily be calculated on the host by downloading the \\a moments array and using the cuda::convertSpatialMoments helper function. e.g.
  ```
  HostMem spatialMomentsHostMem(1, numMoments(MomentsOrder::SECOND_ORDER_MOMENTS), CV_32F);
  spatialMomentsDevice.download(spatialMomentsHostMem, stream);
  stream.waitForCompletion();
  Mat spatialMoments = spatialMomentsHostMem.createMatHeader();
  cv::Moments cvMoments = convertSpatialMoments<float>(spatialMoments, order);
  ```
  see the \\a CUDA_TEST_P(Moments, Async) test inside opencv_contrib_source_code/modules/cudaimgproc/test/test_moments.cpp for an example.
  @sa cuda::moments, cuda::convertSpatialMoments, cuda::numMoments, cuda::MomentsOrder

  Python prototype (for reference only):
  ```python3
  spatialMoments(src[, moments[, binaryImage[, order[, momentsType[, stream]]]]]) -> moments
  ```

  """
  @spec spatialMoments(Evision.Mat.maybe_mat_in(), [{:binaryImage, term()} | {:momentsType, term()} | {:order, term()} | {:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def spatialMoments(src, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:binaryImage, :momentsType, :order, :stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_spatialMoments(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec spatialMoments(Evision.CUDA.GpuMat.t(), [{:binaryImage, term()} | {:momentsType, term()} | {:order, term()} | {:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def spatialMoments(src, opts) when is_struct(src, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:binaryImage, :momentsType, :order, :stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_spatialMoments(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Calculates all of the spatial moments up to the 3rd order of a rasterized shape.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Raster image (single-channel 2D array).

  ##### Keyword Arguments
  - **binaryImage**: `bool`.

    If it is true, all non-zero image pixels are treated as 1's.

  - **order**: `MomentsOrder`.

    Order of largest moments to calculate with lower order moments requiring less computation.

  - **momentsType**: `integer()`.

    Precision to use when calculating moments. Available types are \\ref CV_32F and \\ref CV_64F with the performance of \\ref CV_32F an order of magnitude greater than \\ref CV_64F. If the image is small the accuracy from \\ref CV_32F can be equal or very close to \\ref CV_64F.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **moments**: `Evision.Mat.t()`.

  Asynchronous version of cuda::moments() which only calculates the spatial (not centralized or normalized) moments, up to the 3rd order, of a rasterized shape.
  Each moment is returned as a column entry in the 1D \\a moments array.

  **Note**: For maximum performance pre-allocate a 1D GpuMat for \\a moments of the correct type and size large enough to store the all the image moments of up to the desired \\a order. e.g. With \\a order === MomentsOrder::SECOND_ORDER_MOMENTS and \\a momentsType == \\ref CV_32F \\a moments can be allocated as
  ```
  GpuMat momentsDevice(1,numMoments(MomentsOrder::SECOND_ORDER_MOMENTS),CV_32F)
  ```
  The central and normalized moments can easily be calculated on the host by downloading the \\a moments array and using the cuda::convertSpatialMoments helper function. e.g.
  ```
  HostMem spatialMomentsHostMem(1, numMoments(MomentsOrder::SECOND_ORDER_MOMENTS), CV_32F);
  spatialMomentsDevice.download(spatialMomentsHostMem, stream);
  stream.waitForCompletion();
  Mat spatialMoments = spatialMomentsHostMem.createMatHeader();
  cv::Moments cvMoments = convertSpatialMoments<float>(spatialMoments, order);
  ```
  see the \\a CUDA_TEST_P(Moments, Async) test inside opencv_contrib_source_code/modules/cudaimgproc/test/test_moments.cpp for an example.
  @sa cuda::moments, cuda::convertSpatialMoments, cuda::numMoments, cuda::MomentsOrder

  Python prototype (for reference only):
  ```python3
  spatialMoments(src[, moments[, binaryImage[, order[, momentsType[, stream]]]]]) -> moments
  ```
  #### Variant 2:
  Calculates all of the spatial moments up to the 3rd order of a rasterized shape.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Raster image (single-channel 2D array).

  ##### Keyword Arguments
  - **binaryImage**: `bool`.

    If it is true, all non-zero image pixels are treated as 1's.

  - **order**: `MomentsOrder`.

    Order of largest moments to calculate with lower order moments requiring less computation.

  - **momentsType**: `integer()`.

    Precision to use when calculating moments. Available types are \\ref CV_32F and \\ref CV_64F with the performance of \\ref CV_32F an order of magnitude greater than \\ref CV_64F. If the image is small the accuracy from \\ref CV_32F can be equal or very close to \\ref CV_64F.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **moments**: `Evision.CUDA.GpuMat.t()`.

  Asynchronous version of cuda::moments() which only calculates the spatial (not centralized or normalized) moments, up to the 3rd order, of a rasterized shape.
  Each moment is returned as a column entry in the 1D \\a moments array.

  **Note**: For maximum performance pre-allocate a 1D GpuMat for \\a moments of the correct type and size large enough to store the all the image moments of up to the desired \\a order. e.g. With \\a order === MomentsOrder::SECOND_ORDER_MOMENTS and \\a momentsType == \\ref CV_32F \\a moments can be allocated as
  ```
  GpuMat momentsDevice(1,numMoments(MomentsOrder::SECOND_ORDER_MOMENTS),CV_32F)
  ```
  The central and normalized moments can easily be calculated on the host by downloading the \\a moments array and using the cuda::convertSpatialMoments helper function. e.g.
  ```
  HostMem spatialMomentsHostMem(1, numMoments(MomentsOrder::SECOND_ORDER_MOMENTS), CV_32F);
  spatialMomentsDevice.download(spatialMomentsHostMem, stream);
  stream.waitForCompletion();
  Mat spatialMoments = spatialMomentsHostMem.createMatHeader();
  cv::Moments cvMoments = convertSpatialMoments<float>(spatialMoments, order);
  ```
  see the \\a CUDA_TEST_P(Moments, Async) test inside opencv_contrib_source_code/modules/cudaimgproc/test/test_moments.cpp for an example.
  @sa cuda::moments, cuda::convertSpatialMoments, cuda::numMoments, cuda::MomentsOrder

  Python prototype (for reference only):
  ```python3
  spatialMoments(src[, moments[, binaryImage[, order[, momentsType[, stream]]]]]) -> moments
  ```

  """
  @spec spatialMoments(Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def spatialMoments(src) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_spatialMoments(positional)
    |> to_struct()
  end
  @spec spatialMoments(Evision.CUDA.GpuMat.t()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def spatialMoments(src) when is_struct(src, Evision.CUDA.GpuMat)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_spatialMoments(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  split

  ##### Positional Arguments
  - **src**: `Evision.Mat`

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **dst**: `[Evision.CUDA.GpuMat]`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  split(src[, dst[, stream]]) -> dst
  ```
  #### Variant 2:
  split

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **dst**: `[Evision.CUDA.GpuMat]`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  split(src[, dst[, stream]]) -> dst
  ```

  """
  @spec split(Evision.Mat.maybe_mat_in(), [{:stream, term()}] | nil) :: list(Evision.CUDA.GpuMat.t()) | {:error, String.t()}
  def split(src, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_split(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec split(Evision.CUDA.GpuMat.t(), [{:stream, term()}] | nil) :: list(Evision.CUDA.GpuMat.t()) | {:error, String.t()}
  def split(src, opts) when is_struct(src, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_split(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  split

  ##### Positional Arguments
  - **src**: `Evision.Mat`

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **dst**: `[Evision.CUDA.GpuMat]`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  split(src[, dst[, stream]]) -> dst
  ```
  #### Variant 2:
  split

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **dst**: `[Evision.CUDA.GpuMat]`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  split(src[, dst[, stream]]) -> dst
  ```

  """
  @spec split(Evision.Mat.maybe_mat_in()) :: list(Evision.CUDA.GpuMat.t()) | {:error, String.t()}
  def split(src) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_split(positional)
    |> to_struct()
  end
  @spec split(Evision.CUDA.GpuMat.t()) :: list(Evision.CUDA.GpuMat.t()) | {:error, String.t()}
  def split(src) when is_struct(src, Evision.CUDA.GpuMat)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_split(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Computes a square value of each matrix element.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source matrix.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination matrix with the same size and type as src .

  Python prototype (for reference only):
  ```python3
  sqr(src[, dst[, stream]]) -> dst
  ```
  #### Variant 2:
  Computes a square value of each matrix element.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source matrix.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination matrix with the same size and type as src .

  Python prototype (for reference only):
  ```python3
  sqr(src[, dst[, stream]]) -> dst
  ```

  """
  @spec sqr(Evision.Mat.maybe_mat_in(), [{:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def sqr(src, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_sqr(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec sqr(Evision.CUDA.GpuMat.t(), [{:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def sqr(src, opts) when is_struct(src, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_sqr(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Computes a square value of each matrix element.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source matrix.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination matrix with the same size and type as src .

  Python prototype (for reference only):
  ```python3
  sqr(src[, dst[, stream]]) -> dst
  ```
  #### Variant 2:
  Computes a square value of each matrix element.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source matrix.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination matrix with the same size and type as src .

  Python prototype (for reference only):
  ```python3
  sqr(src[, dst[, stream]]) -> dst
  ```

  """
  @spec sqr(Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def sqr(src) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_sqr(positional)
    |> to_struct()
  end
  @spec sqr(Evision.CUDA.GpuMat.t()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def sqr(src) when is_struct(src, Evision.CUDA.GpuMat)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_sqr(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Computes a squared integral image.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source image. Only CV_8UC1 images are supported for now.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **sqsum**: `Evision.Mat.t()`.

    Squared integral image containing 64-bit unsigned integer values packed into
    CV_64FC1 .

  Python prototype (for reference only):
  ```python3
  sqrIntegral(src[, sqsum[, stream]]) -> sqsum
  ```
  #### Variant 2:
  Computes a squared integral image.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source image. Only CV_8UC1 images are supported for now.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **sqsum**: `Evision.CUDA.GpuMat.t()`.

    Squared integral image containing 64-bit unsigned integer values packed into
    CV_64FC1 .

  Python prototype (for reference only):
  ```python3
  sqrIntegral(src[, sqsum[, stream]]) -> sqsum
  ```

  """
  @spec sqrIntegral(Evision.Mat.maybe_mat_in(), [{:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def sqrIntegral(src, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_sqrIntegral(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec sqrIntegral(Evision.CUDA.GpuMat.t(), [{:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def sqrIntegral(src, opts) when is_struct(src, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_sqrIntegral(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Computes a squared integral image.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source image. Only CV_8UC1 images are supported for now.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **sqsum**: `Evision.Mat.t()`.

    Squared integral image containing 64-bit unsigned integer values packed into
    CV_64FC1 .

  Python prototype (for reference only):
  ```python3
  sqrIntegral(src[, sqsum[, stream]]) -> sqsum
  ```
  #### Variant 2:
  Computes a squared integral image.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source image. Only CV_8UC1 images are supported for now.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **sqsum**: `Evision.CUDA.GpuMat.t()`.

    Squared integral image containing 64-bit unsigned integer values packed into
    CV_64FC1 .

  Python prototype (for reference only):
  ```python3
  sqrIntegral(src[, sqsum[, stream]]) -> sqsum
  ```

  """
  @spec sqrIntegral(Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def sqrIntegral(src) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_sqrIntegral(positional)
    |> to_struct()
  end
  @spec sqrIntegral(Evision.CUDA.GpuMat.t()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def sqrIntegral(src) when is_struct(src, Evision.CUDA.GpuMat)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_sqrIntegral(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Returns the squared sum of matrix elements.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source image of any depth except for CV_64F .

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.

    optional operation mask; it must have the same size as src1 and CV_8UC1 type.

  ##### Return
  - **retval**: `Evision.scalar().t()`

  Python prototype (for reference only):
  ```python3
  sqrSum(src[, mask]) -> retval
  ```
  #### Variant 2:
  Returns the squared sum of matrix elements.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source image of any depth except for CV_64F .

  ##### Keyword Arguments
  - **mask**: `Evision.CUDA.GpuMat.t()`.

    optional operation mask; it must have the same size as src1 and CV_8UC1 type.

  ##### Return
  - **retval**: `Evision.scalar().t()`

  Python prototype (for reference only):
  ```python3
  sqrSum(src[, mask]) -> retval
  ```

  """
  @spec sqrSum(Evision.Mat.maybe_mat_in(), [{:mask, term()}] | nil) :: Evision.scalar() | {:error, String.t()}
  def sqrSum(src, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:mask])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_sqrSum(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec sqrSum(Evision.CUDA.GpuMat.t(), [{:mask, term()}] | nil) :: Evision.scalar() | {:error, String.t()}
  def sqrSum(src, opts) when is_struct(src, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:mask])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_sqrSum(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Returns the squared sum of matrix elements.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source image of any depth except for CV_64F .

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.

    optional operation mask; it must have the same size as src1 and CV_8UC1 type.

  ##### Return
  - **retval**: `Evision.scalar().t()`

  Python prototype (for reference only):
  ```python3
  sqrSum(src[, mask]) -> retval
  ```
  #### Variant 2:
  Returns the squared sum of matrix elements.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source image of any depth except for CV_64F .

  ##### Keyword Arguments
  - **mask**: `Evision.CUDA.GpuMat.t()`.

    optional operation mask; it must have the same size as src1 and CV_8UC1 type.

  ##### Return
  - **retval**: `Evision.scalar().t()`

  Python prototype (for reference only):
  ```python3
  sqrSum(src[, mask]) -> retval
  ```

  """
  @spec sqrSum(Evision.Mat.maybe_mat_in()) :: Evision.scalar() | {:error, String.t()}
  def sqrSum(src) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_sqrSum(positional)
    |> to_struct()
  end
  @spec sqrSum(Evision.CUDA.GpuMat.t()) :: Evision.scalar() | {:error, String.t()}
  def sqrSum(src) when is_struct(src, Evision.CUDA.GpuMat)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_sqrSum(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Computes a square root of each matrix element.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source matrix.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination matrix with the same size and type as src .

  @sa sqrt

  Python prototype (for reference only):
  ```python3
  sqrt(src[, dst[, stream]]) -> dst
  ```
  #### Variant 2:
  Computes a square root of each matrix element.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source matrix.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination matrix with the same size and type as src .

  @sa sqrt

  Python prototype (for reference only):
  ```python3
  sqrt(src[, dst[, stream]]) -> dst
  ```

  """
  @spec sqrt(Evision.Mat.maybe_mat_in(), [{:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def sqrt(src, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_sqrt(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec sqrt(Evision.CUDA.GpuMat.t(), [{:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def sqrt(src, opts) when is_struct(src, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_sqrt(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Computes a square root of each matrix element.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source matrix.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination matrix with the same size and type as src .

  @sa sqrt

  Python prototype (for reference only):
  ```python3
  sqrt(src[, dst[, stream]]) -> dst
  ```
  #### Variant 2:
  Computes a square root of each matrix element.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source matrix.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination matrix with the same size and type as src .

  @sa sqrt

  Python prototype (for reference only):
  ```python3
  sqrt(src[, dst[, stream]]) -> dst
  ```

  """
  @spec sqrt(Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def sqrt(src) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_sqrt(positional)
    |> to_struct()
  end
  @spec sqrt(Evision.CUDA.GpuMat.t()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def sqrt(src) when is_struct(src, Evision.CUDA.GpuMat)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_sqrt(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Computes a matrix-matrix or matrix-scalar difference.

  ##### Positional Arguments
  - **src1**: `Evision.Mat`.

    First source matrix or scalar.

  - **src2**: `Evision.Mat`.

    Second source matrix or scalar. Matrix should have the same size and type as src1 .

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.

    Optional operation mask, 8-bit single channel array, that specifies elements of the
    destination array to be changed. The mask can be used only with single channel images.

  - **dtype**: `integer()`.

    Optional depth of the output array.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination matrix that has the same size and number of channels as the input array(s).
    The depth is defined by dtype or src1 depth.

  @sa subtract

  Python prototype (for reference only):
  ```python3
  subtract(src1, src2[, dst[, mask[, dtype[, stream]]]]) -> dst
  ```
  #### Variant 2:
  Computes a matrix-matrix or matrix-scalar difference.

  ##### Positional Arguments
  - **src1**: `Evision.CUDA.GpuMat.t()`.

    First source matrix or scalar.

  - **src2**: `Evision.CUDA.GpuMat.t()`.

    Second source matrix or scalar. Matrix should have the same size and type as src1 .

  ##### Keyword Arguments
  - **mask**: `Evision.CUDA.GpuMat.t()`.

    Optional operation mask, 8-bit single channel array, that specifies elements of the
    destination array to be changed. The mask can be used only with single channel images.

  - **dtype**: `integer()`.

    Optional depth of the output array.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination matrix that has the same size and number of channels as the input array(s).
    The depth is defined by dtype or src1 depth.

  @sa subtract

  Python prototype (for reference only):
  ```python3
  subtract(src1, src2[, dst[, mask[, dtype[, stream]]]]) -> dst
  ```

  """
  @spec subtract(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:dtype, term()} | {:mask, term()} | {:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def subtract(src1, src2, opts) when (is_struct(src1, Evision.Mat) or is_struct(src1, Nx.Tensor) or is_number(src1) or is_tuple(src1)) and (is_struct(src2, Evision.Mat) or is_struct(src2, Nx.Tensor) or is_number(src2) or is_tuple(src2)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:dtype, :mask, :stream])
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1),
      src2: Evision.Internal.Structurise.from_struct(src2)
    ]
    :evision_nif.cuda_subtract(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec subtract(Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t(), [{:dtype, term()} | {:mask, term()} | {:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def subtract(src1, src2, opts) when is_struct(src1, Evision.CUDA.GpuMat) and is_struct(src2, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:dtype, :mask, :stream])
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1),
      src2: Evision.Internal.Structurise.from_struct(src2)
    ]
    :evision_nif.cuda_subtract(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Computes a matrix-matrix or matrix-scalar difference.

  ##### Positional Arguments
  - **src1**: `Evision.Mat`.

    First source matrix or scalar.

  - **src2**: `Evision.Mat`.

    Second source matrix or scalar. Matrix should have the same size and type as src1 .

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.

    Optional operation mask, 8-bit single channel array, that specifies elements of the
    destination array to be changed. The mask can be used only with single channel images.

  - **dtype**: `integer()`.

    Optional depth of the output array.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination matrix that has the same size and number of channels as the input array(s).
    The depth is defined by dtype or src1 depth.

  @sa subtract

  Python prototype (for reference only):
  ```python3
  subtract(src1, src2[, dst[, mask[, dtype[, stream]]]]) -> dst
  ```
  #### Variant 2:
  Computes a matrix-matrix or matrix-scalar difference.

  ##### Positional Arguments
  - **src1**: `Evision.CUDA.GpuMat.t()`.

    First source matrix or scalar.

  - **src2**: `Evision.CUDA.GpuMat.t()`.

    Second source matrix or scalar. Matrix should have the same size and type as src1 .

  ##### Keyword Arguments
  - **mask**: `Evision.CUDA.GpuMat.t()`.

    Optional operation mask, 8-bit single channel array, that specifies elements of the
    destination array to be changed. The mask can be used only with single channel images.

  - **dtype**: `integer()`.

    Optional depth of the output array.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination matrix that has the same size and number of channels as the input array(s).
    The depth is defined by dtype or src1 depth.

  @sa subtract

  Python prototype (for reference only):
  ```python3
  subtract(src1, src2[, dst[, mask[, dtype[, stream]]]]) -> dst
  ```

  """
  @spec subtract(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def subtract(src1, src2) when (is_struct(src1, Evision.Mat) or is_struct(src1, Nx.Tensor) or is_number(src1) or is_tuple(src1)) and (is_struct(src2, Evision.Mat) or is_struct(src2, Nx.Tensor) or is_number(src2) or is_tuple(src2))
  do
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1),
      src2: Evision.Internal.Structurise.from_struct(src2)
    ]
    :evision_nif.cuda_subtract(positional)
    |> to_struct()
  end
  @spec subtract(Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def subtract(src1, src2) when is_struct(src1, Evision.CUDA.GpuMat) and is_struct(src2, Evision.CUDA.GpuMat)
  do
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1),
      src2: Evision.Internal.Structurise.from_struct(src2)
    ]
    :evision_nif.cuda_subtract(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Returns the sum of matrix elements.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source image of any depth except for CV_64F .

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.

    optional operation mask; it must have the same size as src1 and CV_8UC1 type.

  ##### Return
  - **retval**: `Evision.scalar().t()`

  @sa sum

  Python prototype (for reference only):
  ```python3
  sum(src[, mask]) -> retval
  ```
  #### Variant 2:
  Returns the sum of matrix elements.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source image of any depth except for CV_64F .

  ##### Keyword Arguments
  - **mask**: `Evision.CUDA.GpuMat.t()`.

    optional operation mask; it must have the same size as src1 and CV_8UC1 type.

  ##### Return
  - **retval**: `Evision.scalar().t()`

  @sa sum

  Python prototype (for reference only):
  ```python3
  sum(src[, mask]) -> retval
  ```

  """
  @spec sum(Evision.Mat.maybe_mat_in(), [{:mask, term()}] | nil) :: Evision.scalar() | {:error, String.t()}
  def sum(src, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:mask])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_sum(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec sum(Evision.CUDA.GpuMat.t(), [{:mask, term()}] | nil) :: Evision.scalar() | {:error, String.t()}
  def sum(src, opts) when is_struct(src, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:mask])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_sum(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Returns the sum of matrix elements.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source image of any depth except for CV_64F .

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.

    optional operation mask; it must have the same size as src1 and CV_8UC1 type.

  ##### Return
  - **retval**: `Evision.scalar().t()`

  @sa sum

  Python prototype (for reference only):
  ```python3
  sum(src[, mask]) -> retval
  ```
  #### Variant 2:
  Returns the sum of matrix elements.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source image of any depth except for CV_64F .

  ##### Keyword Arguments
  - **mask**: `Evision.CUDA.GpuMat.t()`.

    optional operation mask; it must have the same size as src1 and CV_8UC1 type.

  ##### Return
  - **retval**: `Evision.scalar().t()`

  @sa sum

  Python prototype (for reference only):
  ```python3
  sum(src[, mask]) -> retval
  ```

  """
  @spec sum(Evision.Mat.maybe_mat_in()) :: Evision.scalar() | {:error, String.t()}
  def sum(src) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_sum(positional)
    |> to_struct()
  end
  @spec sum(Evision.CUDA.GpuMat.t()) :: Evision.scalar() | {:error, String.t()}
  def sum(src) when is_struct(src, Evision.CUDA.GpuMat)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_sum(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Applies a fixed-level threshold to each array element.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source array (single-channel).

  - **thresh**: `double`.

    Threshold value.

  - **maxval**: `double`.

    Maximum value to use with THRESH_BINARY and THRESH_BINARY_INV threshold types.

  - **type**: `integer()`.

    Threshold type. For details, see threshold . The THRESH_OTSU and THRESH_TRIANGLE
    threshold types are not supported.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **retval**: `double`
  - **dst**: `Evision.Mat.t()`.

    Destination array with the same size and type as src .

  @sa threshold

  Python prototype (for reference only):
  ```python3
  threshold(src, thresh, maxval, type[, dst[, stream]]) -> retval, dst
  ```
  #### Variant 2:
  Applies a fixed-level threshold to each array element.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source array (single-channel).

  - **thresh**: `double`.

    Threshold value.

  - **maxval**: `double`.

    Maximum value to use with THRESH_BINARY and THRESH_BINARY_INV threshold types.

  - **type**: `integer()`.

    Threshold type. For details, see threshold . The THRESH_OTSU and THRESH_TRIANGLE
    threshold types are not supported.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **retval**: `double`
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination array with the same size and type as src .

  @sa threshold

  Python prototype (for reference only):
  ```python3
  threshold(src, thresh, maxval, type[, dst[, stream]]) -> retval, dst
  ```

  """
  @spec threshold(Evision.Mat.maybe_mat_in(), number(), number(), integer(), [{:stream, term()}] | nil) :: {number(), Evision.Mat.t()} | {:error, String.t()}
  def threshold(src, thresh, maxval, type, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_number(thresh) and is_number(maxval) and is_integer(type) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      thresh: Evision.Internal.Structurise.from_struct(thresh),
      maxval: Evision.Internal.Structurise.from_struct(maxval),
      type: Evision.Internal.Structurise.from_struct(type)
    ]
    :evision_nif.cuda_threshold(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec threshold(Evision.CUDA.GpuMat.t(), number(), number(), integer(), [{:stream, term()}] | nil) :: {number(), Evision.CUDA.GpuMat.t()} | {:error, String.t()}
  def threshold(src, thresh, maxval, type, opts) when is_struct(src, Evision.CUDA.GpuMat) and is_number(thresh) and is_number(maxval) and is_integer(type) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      thresh: Evision.Internal.Structurise.from_struct(thresh),
      maxval: Evision.Internal.Structurise.from_struct(maxval),
      type: Evision.Internal.Structurise.from_struct(type)
    ]
    :evision_nif.cuda_threshold(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Applies a fixed-level threshold to each array element.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source array (single-channel).

  - **thresh**: `double`.

    Threshold value.

  - **maxval**: `double`.

    Maximum value to use with THRESH_BINARY and THRESH_BINARY_INV threshold types.

  - **type**: `integer()`.

    Threshold type. For details, see threshold . The THRESH_OTSU and THRESH_TRIANGLE
    threshold types are not supported.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **retval**: `double`
  - **dst**: `Evision.Mat.t()`.

    Destination array with the same size and type as src .

  @sa threshold

  Python prototype (for reference only):
  ```python3
  threshold(src, thresh, maxval, type[, dst[, stream]]) -> retval, dst
  ```
  #### Variant 2:
  Applies a fixed-level threshold to each array element.

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source array (single-channel).

  - **thresh**: `double`.

    Threshold value.

  - **maxval**: `double`.

    Maximum value to use with THRESH_BINARY and THRESH_BINARY_INV threshold types.

  - **type**: `integer()`.

    Threshold type. For details, see threshold . The THRESH_OTSU and THRESH_TRIANGLE
    threshold types are not supported.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **retval**: `double`
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination array with the same size and type as src .

  @sa threshold

  Python prototype (for reference only):
  ```python3
  threshold(src, thresh, maxval, type[, dst[, stream]]) -> retval, dst
  ```

  """
  @spec threshold(Evision.Mat.maybe_mat_in(), number(), number(), integer()) :: {number(), Evision.Mat.t()} | {:error, String.t()}
  def threshold(src, thresh, maxval, type) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_number(thresh) and is_number(maxval) and is_integer(type)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      thresh: Evision.Internal.Structurise.from_struct(thresh),
      maxval: Evision.Internal.Structurise.from_struct(maxval),
      type: Evision.Internal.Structurise.from_struct(type)
    ]
    :evision_nif.cuda_threshold(positional)
    |> to_struct()
  end
  @spec threshold(Evision.CUDA.GpuMat.t(), number(), number(), integer()) :: {number(), Evision.CUDA.GpuMat.t()} | {:error, String.t()}
  def threshold(src, thresh, maxval, type) when is_struct(src, Evision.CUDA.GpuMat) and is_number(thresh) and is_number(maxval) and is_integer(type)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      thresh: Evision.Internal.Structurise.from_struct(thresh),
      maxval: Evision.Internal.Structurise.from_struct(maxval),
      type: Evision.Internal.Structurise.from_struct(type)
    ]
    :evision_nif.cuda_threshold(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Transposes a matrix.

  ##### Positional Arguments
  - **src1**: `Evision.Mat`.

    Source matrix. 1-, 4-, 8-byte element sizes are supported for now.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination matrix.

  @sa transpose

  Python prototype (for reference only):
  ```python3
  transpose(src1[, dst[, stream]]) -> dst
  ```
  #### Variant 2:
  Transposes a matrix.

  ##### Positional Arguments
  - **src1**: `Evision.CUDA.GpuMat.t()`.

    Source matrix. 1-, 4-, 8-byte element sizes are supported for now.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination matrix.

  @sa transpose

  Python prototype (for reference only):
  ```python3
  transpose(src1[, dst[, stream]]) -> dst
  ```

  """
  @spec transpose(Evision.Mat.maybe_mat_in(), [{:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def transpose(src1, opts) when (is_struct(src1, Evision.Mat) or is_struct(src1, Nx.Tensor) or is_number(src1) or is_tuple(src1)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1)
    ]
    :evision_nif.cuda_transpose(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec transpose(Evision.CUDA.GpuMat.t(), [{:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def transpose(src1, opts) when is_struct(src1, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1)
    ]
    :evision_nif.cuda_transpose(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Transposes a matrix.

  ##### Positional Arguments
  - **src1**: `Evision.Mat`.

    Source matrix. 1-, 4-, 8-byte element sizes are supported for now.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination matrix.

  @sa transpose

  Python prototype (for reference only):
  ```python3
  transpose(src1[, dst[, stream]]) -> dst
  ```
  #### Variant 2:
  Transposes a matrix.

  ##### Positional Arguments
  - **src1**: `Evision.CUDA.GpuMat.t()`.

    Source matrix. 1-, 4-, 8-byte element sizes are supported for now.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination matrix.

  @sa transpose

  Python prototype (for reference only):
  ```python3
  transpose(src1[, dst[, stream]]) -> dst
  ```

  """
  @spec transpose(Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def transpose(src1) when (is_struct(src1, Evision.Mat) or is_struct(src1, Nx.Tensor) or is_number(src1) or is_tuple(src1))
  do
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1)
    ]
    :evision_nif.cuda_transpose(positional)
    |> to_struct()
  end
  @spec transpose(Evision.CUDA.GpuMat.t()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def transpose(src1) when is_struct(src1, Evision.CUDA.GpuMat)
  do
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1)
    ]
    :evision_nif.cuda_transpose(positional)
    |> to_struct()
  end

  @doc """
  Unmaps the memory of matrix and makes it pageable again.

  ##### Positional Arguments
  - **m**: `Evision.Mat`.

    Input matrix.

  Python prototype (for reference only):
  ```python3
  unregisterPageLocked(m) -> None
  ```
  """
  @spec unregisterPageLocked(Evision.Mat.maybe_mat_in()) :: :ok | {:error, String.t()}
  def unregisterPageLocked(m) when (is_struct(m, Evision.Mat) or is_struct(m, Nx.Tensor) or is_number(m) or is_tuple(m))
  do
    positional = [
      m: Evision.Internal.Structurise.from_struct(m)
    ]
    :evision_nif.cuda_unregisterPageLocked(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  warpAffine

  ##### Positional Arguments
  - **src**: `Evision.Mat`
  - **m**: `Evision.Mat`
  - **dsize**: `Size`

  ##### Keyword Arguments
  - **flags**: `integer()`.
  - **borderMode**: `integer()`.
  - **borderValue**: `Evision.scalar()`.
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  warpAffine(src, M, dsize[, dst[, flags[, borderMode[, borderValue[, stream]]]]]) -> dst
  ```
  #### Variant 2:
  warpAffine

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`
  - **m**: `Evision.Mat`
  - **dsize**: `Size`

  ##### Keyword Arguments
  - **flags**: `integer()`.
  - **borderMode**: `integer()`.
  - **borderValue**: `Evision.scalar()`.
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

  Python prototype (for reference only):
  ```python3
  warpAffine(src, M, dsize[, dst[, flags[, borderMode[, borderValue[, stream]]]]]) -> dst
  ```

  """
  @spec warpAffine(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), {number(), number()}, [{:borderMode, term()} | {:borderValue, term()} | {:flags, term()} | {:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def warpAffine(src, m, dsize, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (is_struct(m, Evision.Mat) or is_struct(m, Nx.Tensor) or is_number(m) or is_tuple(m)) and is_tuple(dsize) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:borderMode, :borderValue, :flags, :stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      m: Evision.Internal.Structurise.from_struct(m),
      dsize: Evision.Internal.Structurise.from_struct(dsize)
    ]
    :evision_nif.cuda_warpAffine(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec warpAffine(Evision.CUDA.GpuMat.t(), Evision.Mat.maybe_mat_in(), {number(), number()}, [{:borderMode, term()} | {:borderValue, term()} | {:flags, term()} | {:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def warpAffine(src, m, dsize, opts) when is_struct(src, Evision.CUDA.GpuMat) and (is_struct(m, Evision.Mat) or is_struct(m, Nx.Tensor) or is_number(m) or is_tuple(m)) and is_tuple(dsize) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:borderMode, :borderValue, :flags, :stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      m: Evision.Internal.Structurise.from_struct(m),
      dsize: Evision.Internal.Structurise.from_struct(dsize)
    ]
    :evision_nif.cuda_warpAffine(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  warpAffine

  ##### Positional Arguments
  - **src**: `Evision.Mat`
  - **m**: `Evision.Mat`
  - **dsize**: `Size`

  ##### Keyword Arguments
  - **flags**: `integer()`.
  - **borderMode**: `integer()`.
  - **borderValue**: `Evision.scalar()`.
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  warpAffine(src, M, dsize[, dst[, flags[, borderMode[, borderValue[, stream]]]]]) -> dst
  ```
  #### Variant 2:
  warpAffine

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`
  - **m**: `Evision.Mat`
  - **dsize**: `Size`

  ##### Keyword Arguments
  - **flags**: `integer()`.
  - **borderMode**: `integer()`.
  - **borderValue**: `Evision.scalar()`.
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

  Python prototype (for reference only):
  ```python3
  warpAffine(src, M, dsize[, dst[, flags[, borderMode[, borderValue[, stream]]]]]) -> dst
  ```

  """
  @spec warpAffine(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), {number(), number()}) :: Evision.Mat.t() | {:error, String.t()}
  def warpAffine(src, m, dsize) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (is_struct(m, Evision.Mat) or is_struct(m, Nx.Tensor) or is_number(m) or is_tuple(m)) and is_tuple(dsize)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      m: Evision.Internal.Structurise.from_struct(m),
      dsize: Evision.Internal.Structurise.from_struct(dsize)
    ]
    :evision_nif.cuda_warpAffine(positional)
    |> to_struct()
  end
  @spec warpAffine(Evision.CUDA.GpuMat.t(), Evision.Mat.maybe_mat_in(), {number(), number()}) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def warpAffine(src, m, dsize) when is_struct(src, Evision.CUDA.GpuMat) and (is_struct(m, Evision.Mat) or is_struct(m, Nx.Tensor) or is_number(m) or is_tuple(m)) and is_tuple(dsize)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      m: Evision.Internal.Structurise.from_struct(m),
      dsize: Evision.Internal.Structurise.from_struct(dsize)
    ]
    :evision_nif.cuda_warpAffine(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  warpPerspective

  ##### Positional Arguments
  - **src**: `Evision.Mat`
  - **m**: `Evision.Mat`
  - **dsize**: `Size`

  ##### Keyword Arguments
  - **flags**: `integer()`.
  - **borderMode**: `integer()`.
  - **borderValue**: `Evision.scalar()`.
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  warpPerspective(src, M, dsize[, dst[, flags[, borderMode[, borderValue[, stream]]]]]) -> dst
  ```
  #### Variant 2:
  warpPerspective

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`
  - **m**: `Evision.Mat`
  - **dsize**: `Size`

  ##### Keyword Arguments
  - **flags**: `integer()`.
  - **borderMode**: `integer()`.
  - **borderValue**: `Evision.scalar()`.
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

  Python prototype (for reference only):
  ```python3
  warpPerspective(src, M, dsize[, dst[, flags[, borderMode[, borderValue[, stream]]]]]) -> dst
  ```

  """
  @spec warpPerspective(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), {number(), number()}, [{:borderMode, term()} | {:borderValue, term()} | {:flags, term()} | {:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def warpPerspective(src, m, dsize, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (is_struct(m, Evision.Mat) or is_struct(m, Nx.Tensor) or is_number(m) or is_tuple(m)) and is_tuple(dsize) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:borderMode, :borderValue, :flags, :stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      m: Evision.Internal.Structurise.from_struct(m),
      dsize: Evision.Internal.Structurise.from_struct(dsize)
    ]
    :evision_nif.cuda_warpPerspective(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec warpPerspective(Evision.CUDA.GpuMat.t(), Evision.Mat.maybe_mat_in(), {number(), number()}, [{:borderMode, term()} | {:borderValue, term()} | {:flags, term()} | {:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def warpPerspective(src, m, dsize, opts) when is_struct(src, Evision.CUDA.GpuMat) and (is_struct(m, Evision.Mat) or is_struct(m, Nx.Tensor) or is_number(m) or is_tuple(m)) and is_tuple(dsize) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:borderMode, :borderValue, :flags, :stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      m: Evision.Internal.Structurise.from_struct(m),
      dsize: Evision.Internal.Structurise.from_struct(dsize)
    ]
    :evision_nif.cuda_warpPerspective(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  warpPerspective

  ##### Positional Arguments
  - **src**: `Evision.Mat`
  - **m**: `Evision.Mat`
  - **dsize**: `Size`

  ##### Keyword Arguments
  - **flags**: `integer()`.
  - **borderMode**: `integer()`.
  - **borderValue**: `Evision.scalar()`.
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  warpPerspective(src, M, dsize[, dst[, flags[, borderMode[, borderValue[, stream]]]]]) -> dst
  ```
  #### Variant 2:
  warpPerspective

  ##### Positional Arguments
  - **src**: `Evision.CUDA.GpuMat.t()`
  - **m**: `Evision.Mat`
  - **dsize**: `Size`

  ##### Keyword Arguments
  - **flags**: `integer()`.
  - **borderMode**: `integer()`.
  - **borderValue**: `Evision.scalar()`.
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

  Python prototype (for reference only):
  ```python3
  warpPerspective(src, M, dsize[, dst[, flags[, borderMode[, borderValue[, stream]]]]]) -> dst
  ```

  """
  @spec warpPerspective(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), {number(), number()}) :: Evision.Mat.t() | {:error, String.t()}
  def warpPerspective(src, m, dsize) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (is_struct(m, Evision.Mat) or is_struct(m, Nx.Tensor) or is_number(m) or is_tuple(m)) and is_tuple(dsize)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      m: Evision.Internal.Structurise.from_struct(m),
      dsize: Evision.Internal.Structurise.from_struct(dsize)
    ]
    :evision_nif.cuda_warpPerspective(positional)
    |> to_struct()
  end
  @spec warpPerspective(Evision.CUDA.GpuMat.t(), Evision.Mat.maybe_mat_in(), {number(), number()}) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def warpPerspective(src, m, dsize) when is_struct(src, Evision.CUDA.GpuMat) and (is_struct(m, Evision.Mat) or is_struct(m, Nx.Tensor) or is_number(m) or is_tuple(m)) and is_tuple(dsize)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      m: Evision.Internal.Structurise.from_struct(m),
      dsize: Evision.Internal.Structurise.from_struct(dsize)
    ]
    :evision_nif.cuda_warpPerspective(positional)
    |> to_struct()
  end

  @doc """
  Bindings overload to create a Stream object from the address stored in an existing CUDA Runtime API stream pointer (cudaStream_t).

  ##### Positional Arguments
  - **cudaStreamMemoryAddress**: `size_t`.

    Memory address stored in a CUDA Runtime API stream pointer (cudaStream_t). The created Stream object does not perform any allocation or deallocation and simply wraps existing raw CUDA Runtime API stream pointer.

  ##### Return
  - **retval**: `Evision.CUDA.Stream.t()`

  **Note**: Overload for generation of bindings only, not exported or intended for use internally from C++.

  Python prototype (for reference only):
  ```python3
  wrapStream(cudaStreamMemoryAddress) -> retval
  ```
  """
  @spec wrapStream(integer()) :: Evision.CUDA.Stream.t() | {:error, String.t()}
  def wrapStream(cudaStreamMemoryAddress) when is_integer(cudaStreamMemoryAddress)
  do
    positional = [
      cudaStreamMemoryAddress: Evision.Internal.Structurise.from_struct(cudaStreamMemoryAddress)
    ]
    :evision_nif.cuda_wrapStream(positional)
    |> to_struct()
  end
end
