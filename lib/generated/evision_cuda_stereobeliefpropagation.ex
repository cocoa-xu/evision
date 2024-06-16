defmodule Evision.CUDA.StereoBeliefPropagation do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `CUDA.StereoBeliefPropagation` struct.

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
  def to_struct({:ok, %{class: Evision.CUDA.StereoBeliefPropagation, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.CUDA.StereoBeliefPropagation, ref: ref}) do
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
  - **self**: `Evision.CUDA.StereoBeliefPropagation.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.CUDA.StereoBeliefPropagation.t()) :: Evision.CUDA.StereoBeliefPropagation.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.cuda_StereoBeliefPropagation_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  compute

  ##### Positional Arguments
  - **self**: `Evision.CUDA.StereoBeliefPropagation.t()`
  - **left**: `Evision.Mat`
  - **right**: `Evision.Mat`
  - **stream**: `Evision.CUDA.Stream.t()`

  ##### Return
  - **disparity**: `Evision.Mat.t()`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  compute(left, right, stream[, disparity]) -> disparity
  ```
  #### Variant 2:
  compute

  ##### Positional Arguments
  - **self**: `Evision.CUDA.StereoBeliefPropagation.t()`
  - **left**: `Evision.CUDA.GpuMat.t()`
  - **right**: `Evision.CUDA.GpuMat.t()`
  - **stream**: `Evision.CUDA.Stream.t()`

  ##### Return
  - **disparity**: `Evision.CUDA.GpuMat.t()`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  compute(left, right, stream[, disparity]) -> disparity
  ```

  """
  @spec compute(Evision.CUDA.StereoBeliefPropagation.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.CUDA.Stream.t(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def compute(self, left, right, stream, opts) when (is_struct(left, Evision.Mat) or is_struct(left, Nx.Tensor) or is_number(left) or is_tuple(left)) and (is_struct(right, Evision.Mat) or is_struct(right, Nx.Tensor) or is_number(right) or is_tuple(right)) and is_struct(stream, Evision.CUDA.Stream) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      left: Evision.Internal.Structurise.from_struct(left),
      right: Evision.Internal.Structurise.from_struct(right),
      stream: Evision.Internal.Structurise.from_struct(stream)
    ]
    :evision_nif.cuda_cuda_StereoBeliefPropagation_compute(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec compute(Evision.CUDA.StereoBeliefPropagation.t(), Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t(), Evision.CUDA.Stream.t(), [{atom(), term()},...] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def compute(self, left, right, stream, opts) when is_struct(left, Evision.CUDA.GpuMat) and is_struct(right, Evision.CUDA.GpuMat) and is_struct(stream, Evision.CUDA.Stream) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      left: Evision.Internal.Structurise.from_struct(left),
      right: Evision.Internal.Structurise.from_struct(right),
      stream: Evision.Internal.Structurise.from_struct(stream)
    ]
    :evision_nif.cuda_cuda_StereoBeliefPropagation_compute(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  compute

  ##### Positional Arguments
  - **self**: `Evision.CUDA.StereoBeliefPropagation.t()`
  - **left**: `Evision.Mat`
  - **right**: `Evision.Mat`
  - **stream**: `Evision.CUDA.Stream.t()`

  ##### Return
  - **disparity**: `Evision.Mat.t()`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  compute(left, right, stream[, disparity]) -> disparity
  ```
  #### Variant 2:
  compute

  ##### Positional Arguments
  - **self**: `Evision.CUDA.StereoBeliefPropagation.t()`
  - **left**: `Evision.CUDA.GpuMat.t()`
  - **right**: `Evision.CUDA.GpuMat.t()`
  - **stream**: `Evision.CUDA.Stream.t()`

  ##### Return
  - **disparity**: `Evision.CUDA.GpuMat.t()`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  compute(left, right, stream[, disparity]) -> disparity
  ```

  """
  @spec compute(Evision.CUDA.StereoBeliefPropagation.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.CUDA.Stream.t()) :: Evision.Mat.t() | {:error, String.t()}
  def compute(self, left, right, stream) when (is_struct(left, Evision.Mat) or is_struct(left, Nx.Tensor) or is_number(left) or is_tuple(left)) and (is_struct(right, Evision.Mat) or is_struct(right, Nx.Tensor) or is_number(right) or is_tuple(right)) and is_struct(stream, Evision.CUDA.Stream)
  do
    positional = [
      left: Evision.Internal.Structurise.from_struct(left),
      right: Evision.Internal.Structurise.from_struct(right),
      stream: Evision.Internal.Structurise.from_struct(stream)
    ]
    :evision_nif.cuda_cuda_StereoBeliefPropagation_compute(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec compute(Evision.CUDA.StereoBeliefPropagation.t(), Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t(), Evision.CUDA.Stream.t()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def compute(self, left, right, stream) when is_struct(left, Evision.CUDA.GpuMat) and is_struct(right, Evision.CUDA.GpuMat) and is_struct(stream, Evision.CUDA.Stream)
  do
    positional = [
      left: Evision.Internal.Structurise.from_struct(left),
      right: Evision.Internal.Structurise.from_struct(right),
      stream: Evision.Internal.Structurise.from_struct(stream)
    ]
    :evision_nif.cuda_cuda_StereoBeliefPropagation_compute(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Enables the stereo correspondence operator that finds the disparity for the specified data cost.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.StereoBeliefPropagation.t()`
  - **data**: `Evision.Mat`.

    User-specified data cost, a matrix of msg_type type and
    Size(\\<image columns\\>\\*ndisp, \\<image rows\\>) size.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **disparity**: `Evision.Mat.t()`.

    Output disparity map. If disparity is empty, the output type is CV_16SC1 .
    Otherwise, the type is retained. In 16-bit signed format, the disparity values do not have
    fractional bits.

  Python prototype (for reference only):
  ```python3
  compute(data[, disparity[, stream]]) -> disparity
  ```
  #### Variant 2:
  Enables the stereo correspondence operator that finds the disparity for the specified data cost.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.StereoBeliefPropagation.t()`
  - **data**: `Evision.CUDA.GpuMat.t()`.

    User-specified data cost, a matrix of msg_type type and
    Size(\\<image columns\\>\\*ndisp, \\<image rows\\>) size.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **disparity**: `Evision.CUDA.GpuMat.t()`.

    Output disparity map. If disparity is empty, the output type is CV_16SC1 .
    Otherwise, the type is retained. In 16-bit signed format, the disparity values do not have
    fractional bits.

  Python prototype (for reference only):
  ```python3
  compute(data[, disparity[, stream]]) -> disparity
  ```

  """
  @spec compute(Evision.CUDA.StereoBeliefPropagation.t(), Evision.Mat.maybe_mat_in(), [{:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def compute(self, data, opts) when (is_struct(data, Evision.Mat) or is_struct(data, Nx.Tensor) or is_number(data) or is_tuple(data)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      data: Evision.Internal.Structurise.from_struct(data)
    ]
    :evision_nif.cuda_cuda_StereoBeliefPropagation_compute(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec compute(Evision.CUDA.StereoBeliefPropagation.t(), Evision.CUDA.GpuMat.t(), [{:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def compute(self, data, opts) when is_struct(data, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      data: Evision.Internal.Structurise.from_struct(data)
    ]
    :evision_nif.cuda_cuda_StereoBeliefPropagation_compute(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Enables the stereo correspondence operator that finds the disparity for the specified data cost.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.StereoBeliefPropagation.t()`
  - **data**: `Evision.Mat`.

    User-specified data cost, a matrix of msg_type type and
    Size(\\<image columns\\>\\*ndisp, \\<image rows\\>) size.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **disparity**: `Evision.Mat.t()`.

    Output disparity map. If disparity is empty, the output type is CV_16SC1 .
    Otherwise, the type is retained. In 16-bit signed format, the disparity values do not have
    fractional bits.

  Python prototype (for reference only):
  ```python3
  compute(data[, disparity[, stream]]) -> disparity
  ```
  #### Variant 2:
  Enables the stereo correspondence operator that finds the disparity for the specified data cost.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.StereoBeliefPropagation.t()`
  - **data**: `Evision.CUDA.GpuMat.t()`.

    User-specified data cost, a matrix of msg_type type and
    Size(\\<image columns\\>\\*ndisp, \\<image rows\\>) size.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **disparity**: `Evision.CUDA.GpuMat.t()`.

    Output disparity map. If disparity is empty, the output type is CV_16SC1 .
    Otherwise, the type is retained. In 16-bit signed format, the disparity values do not have
    fractional bits.

  Python prototype (for reference only):
  ```python3
  compute(data[, disparity[, stream]]) -> disparity
  ```

  """
  @spec compute(Evision.CUDA.StereoBeliefPropagation.t(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def compute(self, data) when (is_struct(data, Evision.Mat) or is_struct(data, Nx.Tensor) or is_number(data) or is_tuple(data))
  do
    positional = [
      data: Evision.Internal.Structurise.from_struct(data)
    ]
    :evision_nif.cuda_cuda_StereoBeliefPropagation_compute(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec compute(Evision.CUDA.StereoBeliefPropagation.t(), Evision.CUDA.GpuMat.t()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def compute(self, data) when is_struct(data, Evision.CUDA.GpuMat)
  do
    positional = [
      data: Evision.Internal.Structurise.from_struct(data)
    ]
    :evision_nif.cuda_cuda_StereoBeliefPropagation_compute(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.CUDA.StereoBeliefPropagation.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.CUDA.StereoBeliefPropagation.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.cuda_StereoBeliefPropagation_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Uses a heuristic method to compute the recommended parameters ( ndisp, iters and levels ) for the
  specified image size ( width and height ).

  ##### Positional Arguments
  - **width**: `integer()`
  - **height**: `integer()`
  - **ndisp**: `integer()`
  - **iters**: `integer()`
  - **levels**: `integer()`

  Python prototype (for reference only):
  ```python3
  estimateRecommendedParams(width, height, ndisp, iters, levels) -> None
  ```
  """
  @spec estimateRecommendedParams(integer(), integer(), integer(), integer(), integer()) :: :ok | {:error, String.t()}
  def estimateRecommendedParams(width, height, ndisp, iters, levels) when is_integer(width) and is_integer(height) and is_integer(ndisp) and is_integer(iters) and is_integer(levels)
  do
    positional = [
      width: Evision.Internal.Structurise.from_struct(width),
      height: Evision.Internal.Structurise.from_struct(height),
      ndisp: Evision.Internal.Structurise.from_struct(ndisp),
      iters: Evision.Internal.Structurise.from_struct(iters),
      levels: Evision.Internal.Structurise.from_struct(levels)
    ]
    :evision_nif.cuda_cuda_StereoBeliefPropagation_estimateRecommendedParams_static(positional)
    |> to_struct()
  end

  @doc """
  getBlockSize

  ##### Positional Arguments
  - **self**: `Evision.CUDA.StereoBeliefPropagation.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getBlockSize() -> retval
  ```
  """
  @spec getBlockSize(Evision.CUDA.StereoBeliefPropagation.t()) :: integer() | {:error, String.t()}
  def getBlockSize(self) do
    positional = [
    ]
    :evision_nif.cuda_StereoBeliefPropagation_getBlockSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDataWeight

  ##### Positional Arguments
  - **self**: `Evision.CUDA.StereoBeliefPropagation.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getDataWeight() -> retval
  ```
  """
  @spec getDataWeight(Evision.CUDA.StereoBeliefPropagation.t()) :: number() | {:error, String.t()}
  def getDataWeight(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_StereoBeliefPropagation_getDataWeight(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.CUDA.StereoBeliefPropagation.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.CUDA.StereoBeliefPropagation.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.cuda_StereoBeliefPropagation_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDiscSingleJump

  ##### Positional Arguments
  - **self**: `Evision.CUDA.StereoBeliefPropagation.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getDiscSingleJump() -> retval
  ```
  """
  @spec getDiscSingleJump(Evision.CUDA.StereoBeliefPropagation.t()) :: number() | {:error, String.t()}
  def getDiscSingleJump(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_StereoBeliefPropagation_getDiscSingleJump(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDisp12MaxDiff

  ##### Positional Arguments
  - **self**: `Evision.CUDA.StereoBeliefPropagation.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getDisp12MaxDiff() -> retval
  ```
  """
  @spec getDisp12MaxDiff(Evision.CUDA.StereoBeliefPropagation.t()) :: integer() | {:error, String.t()}
  def getDisp12MaxDiff(self) do
    positional = [
    ]
    :evision_nif.cuda_StereoBeliefPropagation_getDisp12MaxDiff(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMaxDataTerm

  ##### Positional Arguments
  - **self**: `Evision.CUDA.StereoBeliefPropagation.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getMaxDataTerm() -> retval
  ```
  """
  @spec getMaxDataTerm(Evision.CUDA.StereoBeliefPropagation.t()) :: number() | {:error, String.t()}
  def getMaxDataTerm(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_StereoBeliefPropagation_getMaxDataTerm(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMaxDiscTerm

  ##### Positional Arguments
  - **self**: `Evision.CUDA.StereoBeliefPropagation.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getMaxDiscTerm() -> retval
  ```
  """
  @spec getMaxDiscTerm(Evision.CUDA.StereoBeliefPropagation.t()) :: number() | {:error, String.t()}
  def getMaxDiscTerm(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_StereoBeliefPropagation_getMaxDiscTerm(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMinDisparity

  ##### Positional Arguments
  - **self**: `Evision.CUDA.StereoBeliefPropagation.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getMinDisparity() -> retval
  ```
  """
  @spec getMinDisparity(Evision.CUDA.StereoBeliefPropagation.t()) :: integer() | {:error, String.t()}
  def getMinDisparity(self) do
    positional = [
    ]
    :evision_nif.cuda_StereoBeliefPropagation_getMinDisparity(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMsgType

  ##### Positional Arguments
  - **self**: `Evision.CUDA.StereoBeliefPropagation.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getMsgType() -> retval
  ```
  """
  @spec getMsgType(Evision.CUDA.StereoBeliefPropagation.t()) :: integer() | {:error, String.t()}
  def getMsgType(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_StereoBeliefPropagation_getMsgType(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getNumDisparities

  ##### Positional Arguments
  - **self**: `Evision.CUDA.StereoBeliefPropagation.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getNumDisparities() -> retval
  ```
  """
  @spec getNumDisparities(Evision.CUDA.StereoBeliefPropagation.t()) :: integer() | {:error, String.t()}
  def getNumDisparities(self) do
    positional = [
    ]
    :evision_nif.cuda_StereoBeliefPropagation_getNumDisparities(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getNumIters

  ##### Positional Arguments
  - **self**: `Evision.CUDA.StereoBeliefPropagation.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getNumIters() -> retval
  ```
  """
  @spec getNumIters(Evision.CUDA.StereoBeliefPropagation.t()) :: integer() | {:error, String.t()}
  def getNumIters(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_StereoBeliefPropagation_getNumIters(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getNumLevels

  ##### Positional Arguments
  - **self**: `Evision.CUDA.StereoBeliefPropagation.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getNumLevels() -> retval
  ```
  """
  @spec getNumLevels(Evision.CUDA.StereoBeliefPropagation.t()) :: integer() | {:error, String.t()}
  def getNumLevels(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_StereoBeliefPropagation_getNumLevels(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getSpeckleRange

  ##### Positional Arguments
  - **self**: `Evision.CUDA.StereoBeliefPropagation.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getSpeckleRange() -> retval
  ```
  """
  @spec getSpeckleRange(Evision.CUDA.StereoBeliefPropagation.t()) :: integer() | {:error, String.t()}
  def getSpeckleRange(self) do
    positional = [
    ]
    :evision_nif.cuda_StereoBeliefPropagation_getSpeckleRange(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getSpeckleWindowSize

  ##### Positional Arguments
  - **self**: `Evision.CUDA.StereoBeliefPropagation.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getSpeckleWindowSize() -> retval
  ```
  """
  @spec getSpeckleWindowSize(Evision.CUDA.StereoBeliefPropagation.t()) :: integer() | {:error, String.t()}
  def getSpeckleWindowSize(self) do
    positional = [
    ]
    :evision_nif.cuda_StereoBeliefPropagation_getSpeckleWindowSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.CUDA.StereoBeliefPropagation.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.CUDA.StereoBeliefPropagation.t(), Evision.FileNode.t()) :: Evision.CUDA.StereoBeliefPropagation.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.cuda_StereoBeliefPropagation_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.CUDA.StereoBeliefPropagation.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.CUDA.StereoBeliefPropagation.t(), binary()) :: Evision.CUDA.StereoBeliefPropagation.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.cuda_StereoBeliefPropagation_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setBlockSize

  ##### Positional Arguments
  - **self**: `Evision.CUDA.StereoBeliefPropagation.t()`
  - **blockSize**: `integer()`

  Python prototype (for reference only):
  ```python3
  setBlockSize(blockSize) -> None
  ```
  """
  @spec setBlockSize(Evision.CUDA.StereoBeliefPropagation.t(), integer()) :: Evision.CUDA.StereoBeliefPropagation.t() | {:error, String.t()}
  def setBlockSize(self, blockSize) when is_integer(blockSize)
  do
    positional = [
      blockSize: Evision.Internal.Structurise.from_struct(blockSize)
    ]
    :evision_nif.cuda_StereoBeliefPropagation_setBlockSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setDataWeight

  ##### Positional Arguments
  - **self**: `Evision.CUDA.StereoBeliefPropagation.t()`
  - **data_weight**: `double`

  Python prototype (for reference only):
  ```python3
  setDataWeight(data_weight) -> None
  ```
  """
  @spec setDataWeight(Evision.CUDA.StereoBeliefPropagation.t(), number()) :: Evision.CUDA.StereoBeliefPropagation.t() | {:error, String.t()}
  def setDataWeight(self, data_weight) when is_number(data_weight)
  do
    positional = [
      data_weight: Evision.Internal.Structurise.from_struct(data_weight)
    ]
    :evision_nif.cuda_cuda_StereoBeliefPropagation_setDataWeight(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setDiscSingleJump

  ##### Positional Arguments
  - **self**: `Evision.CUDA.StereoBeliefPropagation.t()`
  - **disc_single_jump**: `double`

  Python prototype (for reference only):
  ```python3
  setDiscSingleJump(disc_single_jump) -> None
  ```
  """
  @spec setDiscSingleJump(Evision.CUDA.StereoBeliefPropagation.t(), number()) :: Evision.CUDA.StereoBeliefPropagation.t() | {:error, String.t()}
  def setDiscSingleJump(self, disc_single_jump) when is_number(disc_single_jump)
  do
    positional = [
      disc_single_jump: Evision.Internal.Structurise.from_struct(disc_single_jump)
    ]
    :evision_nif.cuda_cuda_StereoBeliefPropagation_setDiscSingleJump(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setDisp12MaxDiff

  ##### Positional Arguments
  - **self**: `Evision.CUDA.StereoBeliefPropagation.t()`
  - **disp12MaxDiff**: `integer()`

  Python prototype (for reference only):
  ```python3
  setDisp12MaxDiff(disp12MaxDiff) -> None
  ```
  """
  @spec setDisp12MaxDiff(Evision.CUDA.StereoBeliefPropagation.t(), integer()) :: Evision.CUDA.StereoBeliefPropagation.t() | {:error, String.t()}
  def setDisp12MaxDiff(self, disp12MaxDiff) when is_integer(disp12MaxDiff)
  do
    positional = [
      disp12MaxDiff: Evision.Internal.Structurise.from_struct(disp12MaxDiff)
    ]
    :evision_nif.cuda_StereoBeliefPropagation_setDisp12MaxDiff(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMaxDataTerm

  ##### Positional Arguments
  - **self**: `Evision.CUDA.StereoBeliefPropagation.t()`
  - **max_data_term**: `double`

  Python prototype (for reference only):
  ```python3
  setMaxDataTerm(max_data_term) -> None
  ```
  """
  @spec setMaxDataTerm(Evision.CUDA.StereoBeliefPropagation.t(), number()) :: Evision.CUDA.StereoBeliefPropagation.t() | {:error, String.t()}
  def setMaxDataTerm(self, max_data_term) when is_number(max_data_term)
  do
    positional = [
      max_data_term: Evision.Internal.Structurise.from_struct(max_data_term)
    ]
    :evision_nif.cuda_cuda_StereoBeliefPropagation_setMaxDataTerm(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMaxDiscTerm

  ##### Positional Arguments
  - **self**: `Evision.CUDA.StereoBeliefPropagation.t()`
  - **max_disc_term**: `double`

  Python prototype (for reference only):
  ```python3
  setMaxDiscTerm(max_disc_term) -> None
  ```
  """
  @spec setMaxDiscTerm(Evision.CUDA.StereoBeliefPropagation.t(), number()) :: Evision.CUDA.StereoBeliefPropagation.t() | {:error, String.t()}
  def setMaxDiscTerm(self, max_disc_term) when is_number(max_disc_term)
  do
    positional = [
      max_disc_term: Evision.Internal.Structurise.from_struct(max_disc_term)
    ]
    :evision_nif.cuda_cuda_StereoBeliefPropagation_setMaxDiscTerm(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMinDisparity

  ##### Positional Arguments
  - **self**: `Evision.CUDA.StereoBeliefPropagation.t()`
  - **minDisparity**: `integer()`

  Python prototype (for reference only):
  ```python3
  setMinDisparity(minDisparity) -> None
  ```
  """
  @spec setMinDisparity(Evision.CUDA.StereoBeliefPropagation.t(), integer()) :: Evision.CUDA.StereoBeliefPropagation.t() | {:error, String.t()}
  def setMinDisparity(self, minDisparity) when is_integer(minDisparity)
  do
    positional = [
      minDisparity: Evision.Internal.Structurise.from_struct(minDisparity)
    ]
    :evision_nif.cuda_StereoBeliefPropagation_setMinDisparity(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMsgType

  ##### Positional Arguments
  - **self**: `Evision.CUDA.StereoBeliefPropagation.t()`
  - **msg_type**: `integer()`

  Python prototype (for reference only):
  ```python3
  setMsgType(msg_type) -> None
  ```
  """
  @spec setMsgType(Evision.CUDA.StereoBeliefPropagation.t(), integer()) :: Evision.CUDA.StereoBeliefPropagation.t() | {:error, String.t()}
  def setMsgType(self, msg_type) when is_integer(msg_type)
  do
    positional = [
      msg_type: Evision.Internal.Structurise.from_struct(msg_type)
    ]
    :evision_nif.cuda_cuda_StereoBeliefPropagation_setMsgType(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setNumDisparities

  ##### Positional Arguments
  - **self**: `Evision.CUDA.StereoBeliefPropagation.t()`
  - **numDisparities**: `integer()`

  Python prototype (for reference only):
  ```python3
  setNumDisparities(numDisparities) -> None
  ```
  """
  @spec setNumDisparities(Evision.CUDA.StereoBeliefPropagation.t(), integer()) :: Evision.CUDA.StereoBeliefPropagation.t() | {:error, String.t()}
  def setNumDisparities(self, numDisparities) when is_integer(numDisparities)
  do
    positional = [
      numDisparities: Evision.Internal.Structurise.from_struct(numDisparities)
    ]
    :evision_nif.cuda_StereoBeliefPropagation_setNumDisparities(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setNumIters

  ##### Positional Arguments
  - **self**: `Evision.CUDA.StereoBeliefPropagation.t()`
  - **iters**: `integer()`

  Python prototype (for reference only):
  ```python3
  setNumIters(iters) -> None
  ```
  """
  @spec setNumIters(Evision.CUDA.StereoBeliefPropagation.t(), integer()) :: Evision.CUDA.StereoBeliefPropagation.t() | {:error, String.t()}
  def setNumIters(self, iters) when is_integer(iters)
  do
    positional = [
      iters: Evision.Internal.Structurise.from_struct(iters)
    ]
    :evision_nif.cuda_cuda_StereoBeliefPropagation_setNumIters(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setNumLevels

  ##### Positional Arguments
  - **self**: `Evision.CUDA.StereoBeliefPropagation.t()`
  - **levels**: `integer()`

  Python prototype (for reference only):
  ```python3
  setNumLevels(levels) -> None
  ```
  """
  @spec setNumLevels(Evision.CUDA.StereoBeliefPropagation.t(), integer()) :: Evision.CUDA.StereoBeliefPropagation.t() | {:error, String.t()}
  def setNumLevels(self, levels) when is_integer(levels)
  do
    positional = [
      levels: Evision.Internal.Structurise.from_struct(levels)
    ]
    :evision_nif.cuda_cuda_StereoBeliefPropagation_setNumLevels(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setSpeckleRange

  ##### Positional Arguments
  - **self**: `Evision.CUDA.StereoBeliefPropagation.t()`
  - **speckleRange**: `integer()`

  Python prototype (for reference only):
  ```python3
  setSpeckleRange(speckleRange) -> None
  ```
  """
  @spec setSpeckleRange(Evision.CUDA.StereoBeliefPropagation.t(), integer()) :: Evision.CUDA.StereoBeliefPropagation.t() | {:error, String.t()}
  def setSpeckleRange(self, speckleRange) when is_integer(speckleRange)
  do
    positional = [
      speckleRange: Evision.Internal.Structurise.from_struct(speckleRange)
    ]
    :evision_nif.cuda_StereoBeliefPropagation_setSpeckleRange(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setSpeckleWindowSize

  ##### Positional Arguments
  - **self**: `Evision.CUDA.StereoBeliefPropagation.t()`
  - **speckleWindowSize**: `integer()`

  Python prototype (for reference only):
  ```python3
  setSpeckleWindowSize(speckleWindowSize) -> None
  ```
  """
  @spec setSpeckleWindowSize(Evision.CUDA.StereoBeliefPropagation.t(), integer()) :: Evision.CUDA.StereoBeliefPropagation.t() | {:error, String.t()}
  def setSpeckleWindowSize(self, speckleWindowSize) when is_integer(speckleWindowSize)
  do
    positional = [
      speckleWindowSize: Evision.Internal.Structurise.from_struct(speckleWindowSize)
    ]
    :evision_nif.cuda_StereoBeliefPropagation_setSpeckleWindowSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.CUDA.StereoBeliefPropagation.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.CUDA.StereoBeliefPropagation.t(), Evision.FileStorage.t(), binary()) :: Evision.CUDA.StereoBeliefPropagation.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.cuda_StereoBeliefPropagation_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.CUDA.StereoBeliefPropagation.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.CUDA.StereoBeliefPropagation.t(), Evision.FileStorage.t()) :: Evision.CUDA.StereoBeliefPropagation.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.cuda_StereoBeliefPropagation_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
