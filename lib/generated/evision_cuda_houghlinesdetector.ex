defmodule Evision.CUDA.HoughLinesDetector do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `CUDA.HoughLinesDetector` struct.

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
  def to_struct({:ok, %{class: Evision.CUDA.HoughLinesDetector, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.CUDA.HoughLinesDetector, ref: ref}) do
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
  - **self**: `Evision.CUDA.HoughLinesDetector.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.CUDA.HoughLinesDetector.t()) :: Evision.CUDA.HoughLinesDetector.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.cuda_HoughLinesDetector_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Finds lines in a binary image using the classical Hough transform.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughLinesDetector.t()`
  - **src**: `Evision.Mat`.

    8-bit, single-channel binary source image.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **lines**: `Evision.Mat.t()`.

    Output vector of lines. Each line is represented by a two-element vector
    \\f$(\\rho, \\theta)\\f$ . \\f$\\rho\\f$ is the distance from the coordinate origin \\f$(0,0)\\f$ (top-left corner of
    the image). \\f$\\theta\\f$ is the line rotation angle in radians (
    \\f$0 \\sim \\textrm{vertical line}, \\pi/2 \\sim \\textrm{horizontal line}\\f$ ).

  @sa HoughLines

  Python prototype (for reference only):
  ```python3
  detect(src[, lines[, stream]]) -> lines
  ```
  #### Variant 2:
  Finds lines in a binary image using the classical Hough transform.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughLinesDetector.t()`
  - **src**: `Evision.CUDA.GpuMat.t()`.

    8-bit, single-channel binary source image.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **lines**: `Evision.CUDA.GpuMat.t()`.

    Output vector of lines. Each line is represented by a two-element vector
    \\f$(\\rho, \\theta)\\f$ . \\f$\\rho\\f$ is the distance from the coordinate origin \\f$(0,0)\\f$ (top-left corner of
    the image). \\f$\\theta\\f$ is the line rotation angle in radians (
    \\f$0 \\sim \\textrm{vertical line}, \\pi/2 \\sim \\textrm{horizontal line}\\f$ ).

  @sa HoughLines

  Python prototype (for reference only):
  ```python3
  detect(src[, lines[, stream]]) -> lines
  ```

  """
  @spec detect(Evision.CUDA.HoughLinesDetector.t(), Evision.Mat.maybe_mat_in(), [{:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def detect(self, src, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_cuda_HoughLinesDetector_detect(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec detect(Evision.CUDA.HoughLinesDetector.t(), Evision.CUDA.GpuMat.t(), [{:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def detect(self, src, opts) when is_struct(src, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_cuda_HoughLinesDetector_detect(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Finds lines in a binary image using the classical Hough transform.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughLinesDetector.t()`
  - **src**: `Evision.Mat`.

    8-bit, single-channel binary source image.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **lines**: `Evision.Mat.t()`.

    Output vector of lines. Each line is represented by a two-element vector
    \\f$(\\rho, \\theta)\\f$ . \\f$\\rho\\f$ is the distance from the coordinate origin \\f$(0,0)\\f$ (top-left corner of
    the image). \\f$\\theta\\f$ is the line rotation angle in radians (
    \\f$0 \\sim \\textrm{vertical line}, \\pi/2 \\sim \\textrm{horizontal line}\\f$ ).

  @sa HoughLines

  Python prototype (for reference only):
  ```python3
  detect(src[, lines[, stream]]) -> lines
  ```
  #### Variant 2:
  Finds lines in a binary image using the classical Hough transform.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughLinesDetector.t()`
  - **src**: `Evision.CUDA.GpuMat.t()`.

    8-bit, single-channel binary source image.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **lines**: `Evision.CUDA.GpuMat.t()`.

    Output vector of lines. Each line is represented by a two-element vector
    \\f$(\\rho, \\theta)\\f$ . \\f$\\rho\\f$ is the distance from the coordinate origin \\f$(0,0)\\f$ (top-left corner of
    the image). \\f$\\theta\\f$ is the line rotation angle in radians (
    \\f$0 \\sim \\textrm{vertical line}, \\pi/2 \\sim \\textrm{horizontal line}\\f$ ).

  @sa HoughLines

  Python prototype (for reference only):
  ```python3
  detect(src[, lines[, stream]]) -> lines
  ```

  """
  @spec detect(Evision.CUDA.HoughLinesDetector.t(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def detect(self, src) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_cuda_HoughLinesDetector_detect(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec detect(Evision.CUDA.HoughLinesDetector.t(), Evision.CUDA.GpuMat.t()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def detect(self, src) when is_struct(src, Evision.CUDA.GpuMat)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_cuda_HoughLinesDetector_detect(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Downloads results from cuda::HoughLinesDetector::detect to host memory.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughLinesDetector.t()`
  - **d_lines**: `Evision.Mat`.

    Result of cuda::HoughLinesDetector::detect .

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **h_lines**: `Evision.Mat.t()`.

    Output host array.

  - **h_votes**: `Evision.Mat.t()`.

    Optional output array for line's votes.

  Python prototype (for reference only):
  ```python3
  downloadResults(d_lines[, h_lines[, h_votes[, stream]]]) -> h_lines, h_votes
  ```
  #### Variant 2:
  Downloads results from cuda::HoughLinesDetector::detect to host memory.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughLinesDetector.t()`
  - **d_lines**: `Evision.CUDA.GpuMat.t()`.

    Result of cuda::HoughLinesDetector::detect .

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **h_lines**: `Evision.CUDA.GpuMat.t()`.

    Output host array.

  - **h_votes**: `Evision.CUDA.GpuMat.t()`.

    Optional output array for line's votes.

  Python prototype (for reference only):
  ```python3
  downloadResults(d_lines[, h_lines[, h_votes[, stream]]]) -> h_lines, h_votes
  ```

  """
  @spec downloadResults(Evision.CUDA.HoughLinesDetector.t(), Evision.Mat.maybe_mat_in(), [{:stream, term()}] | nil) :: {Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def downloadResults(self, d_lines, opts) when (is_struct(d_lines, Evision.Mat) or is_struct(d_lines, Nx.Tensor) or is_number(d_lines) or is_tuple(d_lines)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      d_lines: Evision.Internal.Structurise.from_struct(d_lines)
    ]
    :evision_nif.cuda_cuda_HoughLinesDetector_downloadResults(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec downloadResults(Evision.CUDA.HoughLinesDetector.t(), Evision.CUDA.GpuMat.t(), [{:stream, term()}] | nil) :: {Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t()} | {:error, String.t()}
  def downloadResults(self, d_lines, opts) when is_struct(d_lines, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      d_lines: Evision.Internal.Structurise.from_struct(d_lines)
    ]
    :evision_nif.cuda_cuda_HoughLinesDetector_downloadResults(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Downloads results from cuda::HoughLinesDetector::detect to host memory.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughLinesDetector.t()`
  - **d_lines**: `Evision.Mat`.

    Result of cuda::HoughLinesDetector::detect .

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **h_lines**: `Evision.Mat.t()`.

    Output host array.

  - **h_votes**: `Evision.Mat.t()`.

    Optional output array for line's votes.

  Python prototype (for reference only):
  ```python3
  downloadResults(d_lines[, h_lines[, h_votes[, stream]]]) -> h_lines, h_votes
  ```
  #### Variant 2:
  Downloads results from cuda::HoughLinesDetector::detect to host memory.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughLinesDetector.t()`
  - **d_lines**: `Evision.CUDA.GpuMat.t()`.

    Result of cuda::HoughLinesDetector::detect .

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **h_lines**: `Evision.CUDA.GpuMat.t()`.

    Output host array.

  - **h_votes**: `Evision.CUDA.GpuMat.t()`.

    Optional output array for line's votes.

  Python prototype (for reference only):
  ```python3
  downloadResults(d_lines[, h_lines[, h_votes[, stream]]]) -> h_lines, h_votes
  ```

  """
  @spec downloadResults(Evision.CUDA.HoughLinesDetector.t(), Evision.Mat.maybe_mat_in()) :: {Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def downloadResults(self, d_lines) when (is_struct(d_lines, Evision.Mat) or is_struct(d_lines, Nx.Tensor) or is_number(d_lines) or is_tuple(d_lines))
  do
    positional = [
      d_lines: Evision.Internal.Structurise.from_struct(d_lines)
    ]
    :evision_nif.cuda_cuda_HoughLinesDetector_downloadResults(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec downloadResults(Evision.CUDA.HoughLinesDetector.t(), Evision.CUDA.GpuMat.t()) :: {Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t()} | {:error, String.t()}
  def downloadResults(self, d_lines) when is_struct(d_lines, Evision.CUDA.GpuMat)
  do
    positional = [
      d_lines: Evision.Internal.Structurise.from_struct(d_lines)
    ]
    :evision_nif.cuda_cuda_HoughLinesDetector_downloadResults(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughLinesDetector.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.CUDA.HoughLinesDetector.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.cuda_HoughLinesDetector_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughLinesDetector.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.CUDA.HoughLinesDetector.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.cuda_HoughLinesDetector_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDoSort

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughLinesDetector.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  getDoSort() -> retval
  ```
  """
  @spec getDoSort(Evision.CUDA.HoughLinesDetector.t()) :: boolean() | {:error, String.t()}
  def getDoSort(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_HoughLinesDetector_getDoSort(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMaxLines

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughLinesDetector.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getMaxLines() -> retval
  ```
  """
  @spec getMaxLines(Evision.CUDA.HoughLinesDetector.t()) :: integer() | {:error, String.t()}
  def getMaxLines(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_HoughLinesDetector_getMaxLines(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getRho

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughLinesDetector.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  getRho() -> retval
  ```
  """
  @spec getRho(Evision.CUDA.HoughLinesDetector.t()) :: number() | {:error, String.t()}
  def getRho(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_HoughLinesDetector_getRho(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getTheta

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughLinesDetector.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  getTheta() -> retval
  ```
  """
  @spec getTheta(Evision.CUDA.HoughLinesDetector.t()) :: number() | {:error, String.t()}
  def getTheta(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_HoughLinesDetector_getTheta(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getThreshold

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughLinesDetector.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getThreshold() -> retval
  ```
  """
  @spec getThreshold(Evision.CUDA.HoughLinesDetector.t()) :: integer() | {:error, String.t()}
  def getThreshold(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_HoughLinesDetector_getThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughLinesDetector.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.CUDA.HoughLinesDetector.t(), Evision.FileNode.t()) :: Evision.CUDA.HoughLinesDetector.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.cuda_HoughLinesDetector_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughLinesDetector.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.CUDA.HoughLinesDetector.t(), binary()) :: Evision.CUDA.HoughLinesDetector.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.cuda_HoughLinesDetector_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setDoSort

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughLinesDetector.t()`
  - **doSort**: `bool`

  Python prototype (for reference only):
  ```python3
  setDoSort(doSort) -> None
  ```
  """
  @spec setDoSort(Evision.CUDA.HoughLinesDetector.t(), boolean()) :: Evision.CUDA.HoughLinesDetector.t() | {:error, String.t()}
  def setDoSort(self, doSort) when is_boolean(doSort)
  do
    positional = [
      doSort: Evision.Internal.Structurise.from_struct(doSort)
    ]
    :evision_nif.cuda_cuda_HoughLinesDetector_setDoSort(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMaxLines

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughLinesDetector.t()`
  - **maxLines**: `integer()`

  Python prototype (for reference only):
  ```python3
  setMaxLines(maxLines) -> None
  ```
  """
  @spec setMaxLines(Evision.CUDA.HoughLinesDetector.t(), integer()) :: Evision.CUDA.HoughLinesDetector.t() | {:error, String.t()}
  def setMaxLines(self, maxLines) when is_integer(maxLines)
  do
    positional = [
      maxLines: Evision.Internal.Structurise.from_struct(maxLines)
    ]
    :evision_nif.cuda_cuda_HoughLinesDetector_setMaxLines(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setRho

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughLinesDetector.t()`
  - **rho**: `float`

  Python prototype (for reference only):
  ```python3
  setRho(rho) -> None
  ```
  """
  @spec setRho(Evision.CUDA.HoughLinesDetector.t(), number()) :: Evision.CUDA.HoughLinesDetector.t() | {:error, String.t()}
  def setRho(self, rho) when is_float(rho)
  do
    positional = [
      rho: Evision.Internal.Structurise.from_struct(rho)
    ]
    :evision_nif.cuda_cuda_HoughLinesDetector_setRho(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setTheta

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughLinesDetector.t()`
  - **theta**: `float`

  Python prototype (for reference only):
  ```python3
  setTheta(theta) -> None
  ```
  """
  @spec setTheta(Evision.CUDA.HoughLinesDetector.t(), number()) :: Evision.CUDA.HoughLinesDetector.t() | {:error, String.t()}
  def setTheta(self, theta) when is_float(theta)
  do
    positional = [
      theta: Evision.Internal.Structurise.from_struct(theta)
    ]
    :evision_nif.cuda_cuda_HoughLinesDetector_setTheta(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setThreshold

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughLinesDetector.t()`
  - **threshold**: `integer()`

  Python prototype (for reference only):
  ```python3
  setThreshold(threshold) -> None
  ```
  """
  @spec setThreshold(Evision.CUDA.HoughLinesDetector.t(), integer()) :: Evision.CUDA.HoughLinesDetector.t() | {:error, String.t()}
  def setThreshold(self, threshold) when is_integer(threshold)
  do
    positional = [
      threshold: Evision.Internal.Structurise.from_struct(threshold)
    ]
    :evision_nif.cuda_cuda_HoughLinesDetector_setThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughLinesDetector.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.CUDA.HoughLinesDetector.t(), Evision.FileStorage.t(), binary()) :: Evision.CUDA.HoughLinesDetector.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.cuda_HoughLinesDetector_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughLinesDetector.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.CUDA.HoughLinesDetector.t(), Evision.FileStorage.t()) :: Evision.CUDA.HoughLinesDetector.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.cuda_HoughLinesDetector_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
