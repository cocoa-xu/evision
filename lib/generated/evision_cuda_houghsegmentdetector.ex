defmodule Evision.CUDA.HoughSegmentDetector do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `CUDA.HoughSegmentDetector` struct.

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
  def to_struct({:ok, %{class: Evision.CUDA.HoughSegmentDetector, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.CUDA.HoughSegmentDetector, ref: ref}) do
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
  - **self**: `Evision.CUDA.HoughSegmentDetector.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.CUDA.HoughSegmentDetector.t()) :: Evision.CUDA.HoughSegmentDetector.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.cuda_HoughSegmentDetector_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Finds line segments in a binary image using the probabilistic Hough transform.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughSegmentDetector.t()`
  - **src**: `Evision.Mat`.

    8-bit, single-channel binary source image.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **lines**: `Evision.Mat.t()`.

    Output vector of lines. Each line is represented by a 4-element vector
    \\f$(x_1, y_1, x_2, y_2)\\f$ , where \\f$(x_1,y_1)\\f$ and \\f$(x_2, y_2)\\f$ are the ending points of each detected
    line segment.

  @sa HoughLinesP

  Python prototype (for reference only):
  ```python3
  detect(src[, lines[, stream]]) -> lines
  ```
  #### Variant 2:
  Finds line segments in a binary image using the probabilistic Hough transform.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughSegmentDetector.t()`
  - **src**: `Evision.CUDA.GpuMat.t()`.

    8-bit, single-channel binary source image.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **lines**: `Evision.CUDA.GpuMat.t()`.

    Output vector of lines. Each line is represented by a 4-element vector
    \\f$(x_1, y_1, x_2, y_2)\\f$ , where \\f$(x_1,y_1)\\f$ and \\f$(x_2, y_2)\\f$ are the ending points of each detected
    line segment.

  @sa HoughLinesP

  Python prototype (for reference only):
  ```python3
  detect(src[, lines[, stream]]) -> lines
  ```

  """
  @spec detect(Evision.CUDA.HoughSegmentDetector.t(), Evision.Mat.maybe_mat_in(), [{:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def detect(self, src, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_cuda_HoughSegmentDetector_detect(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec detect(Evision.CUDA.HoughSegmentDetector.t(), Evision.CUDA.GpuMat.t(), [{:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def detect(self, src, opts) when is_struct(src, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_cuda_HoughSegmentDetector_detect(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Finds line segments in a binary image using the probabilistic Hough transform.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughSegmentDetector.t()`
  - **src**: `Evision.Mat`.

    8-bit, single-channel binary source image.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **lines**: `Evision.Mat.t()`.

    Output vector of lines. Each line is represented by a 4-element vector
    \\f$(x_1, y_1, x_2, y_2)\\f$ , where \\f$(x_1,y_1)\\f$ and \\f$(x_2, y_2)\\f$ are the ending points of each detected
    line segment.

  @sa HoughLinesP

  Python prototype (for reference only):
  ```python3
  detect(src[, lines[, stream]]) -> lines
  ```
  #### Variant 2:
  Finds line segments in a binary image using the probabilistic Hough transform.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughSegmentDetector.t()`
  - **src**: `Evision.CUDA.GpuMat.t()`.

    8-bit, single-channel binary source image.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **lines**: `Evision.CUDA.GpuMat.t()`.

    Output vector of lines. Each line is represented by a 4-element vector
    \\f$(x_1, y_1, x_2, y_2)\\f$ , where \\f$(x_1,y_1)\\f$ and \\f$(x_2, y_2)\\f$ are the ending points of each detected
    line segment.

  @sa HoughLinesP

  Python prototype (for reference only):
  ```python3
  detect(src[, lines[, stream]]) -> lines
  ```

  """
  @spec detect(Evision.CUDA.HoughSegmentDetector.t(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def detect(self, src) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_cuda_HoughSegmentDetector_detect(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec detect(Evision.CUDA.HoughSegmentDetector.t(), Evision.CUDA.GpuMat.t()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def detect(self, src) when is_struct(src, Evision.CUDA.GpuMat)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_cuda_HoughSegmentDetector_detect(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughSegmentDetector.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.CUDA.HoughSegmentDetector.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.cuda_HoughSegmentDetector_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughSegmentDetector.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.CUDA.HoughSegmentDetector.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.cuda_HoughSegmentDetector_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMaxLineGap

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughSegmentDetector.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getMaxLineGap() -> retval
  ```
  """
  @spec getMaxLineGap(Evision.CUDA.HoughSegmentDetector.t()) :: integer() | {:error, String.t()}
  def getMaxLineGap(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_HoughSegmentDetector_getMaxLineGap(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMaxLines

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughSegmentDetector.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getMaxLines() -> retval
  ```
  """
  @spec getMaxLines(Evision.CUDA.HoughSegmentDetector.t()) :: integer() | {:error, String.t()}
  def getMaxLines(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_HoughSegmentDetector_getMaxLines(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMinLineLength

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughSegmentDetector.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getMinLineLength() -> retval
  ```
  """
  @spec getMinLineLength(Evision.CUDA.HoughSegmentDetector.t()) :: integer() | {:error, String.t()}
  def getMinLineLength(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_HoughSegmentDetector_getMinLineLength(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getRho

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughSegmentDetector.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  getRho() -> retval
  ```
  """
  @spec getRho(Evision.CUDA.HoughSegmentDetector.t()) :: number() | {:error, String.t()}
  def getRho(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_HoughSegmentDetector_getRho(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getTheta

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughSegmentDetector.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  getTheta() -> retval
  ```
  """
  @spec getTheta(Evision.CUDA.HoughSegmentDetector.t()) :: number() | {:error, String.t()}
  def getTheta(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_HoughSegmentDetector_getTheta(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getThreshold

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughSegmentDetector.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getThreshold() -> retval
  ```
  """
  @spec getThreshold(Evision.CUDA.HoughSegmentDetector.t()) :: integer() | {:error, String.t()}
  def getThreshold(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_HoughSegmentDetector_getThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughSegmentDetector.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.CUDA.HoughSegmentDetector.t(), Evision.FileNode.t()) :: Evision.CUDA.HoughSegmentDetector.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.cuda_HoughSegmentDetector_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughSegmentDetector.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.CUDA.HoughSegmentDetector.t(), binary()) :: Evision.CUDA.HoughSegmentDetector.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.cuda_HoughSegmentDetector_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMaxLineGap

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughSegmentDetector.t()`
  - **maxLineGap**: `integer()`

  Python prototype (for reference only):
  ```python3
  setMaxLineGap(maxLineGap) -> None
  ```
  """
  @spec setMaxLineGap(Evision.CUDA.HoughSegmentDetector.t(), integer()) :: Evision.CUDA.HoughSegmentDetector.t() | {:error, String.t()}
  def setMaxLineGap(self, maxLineGap) when is_integer(maxLineGap)
  do
    positional = [
      maxLineGap: Evision.Internal.Structurise.from_struct(maxLineGap)
    ]
    :evision_nif.cuda_cuda_HoughSegmentDetector_setMaxLineGap(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMaxLines

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughSegmentDetector.t()`
  - **maxLines**: `integer()`

  Python prototype (for reference only):
  ```python3
  setMaxLines(maxLines) -> None
  ```
  """
  @spec setMaxLines(Evision.CUDA.HoughSegmentDetector.t(), integer()) :: Evision.CUDA.HoughSegmentDetector.t() | {:error, String.t()}
  def setMaxLines(self, maxLines) when is_integer(maxLines)
  do
    positional = [
      maxLines: Evision.Internal.Structurise.from_struct(maxLines)
    ]
    :evision_nif.cuda_cuda_HoughSegmentDetector_setMaxLines(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMinLineLength

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughSegmentDetector.t()`
  - **minLineLength**: `integer()`

  Python prototype (for reference only):
  ```python3
  setMinLineLength(minLineLength) -> None
  ```
  """
  @spec setMinLineLength(Evision.CUDA.HoughSegmentDetector.t(), integer()) :: Evision.CUDA.HoughSegmentDetector.t() | {:error, String.t()}
  def setMinLineLength(self, minLineLength) when is_integer(minLineLength)
  do
    positional = [
      minLineLength: Evision.Internal.Structurise.from_struct(minLineLength)
    ]
    :evision_nif.cuda_cuda_HoughSegmentDetector_setMinLineLength(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setRho

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughSegmentDetector.t()`
  - **rho**: `float`

  Python prototype (for reference only):
  ```python3
  setRho(rho) -> None
  ```
  """
  @spec setRho(Evision.CUDA.HoughSegmentDetector.t(), number()) :: Evision.CUDA.HoughSegmentDetector.t() | {:error, String.t()}
  def setRho(self, rho) when is_float(rho)
  do
    positional = [
      rho: Evision.Internal.Structurise.from_struct(rho)
    ]
    :evision_nif.cuda_cuda_HoughSegmentDetector_setRho(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setTheta

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughSegmentDetector.t()`
  - **theta**: `float`

  Python prototype (for reference only):
  ```python3
  setTheta(theta) -> None
  ```
  """
  @spec setTheta(Evision.CUDA.HoughSegmentDetector.t(), number()) :: Evision.CUDA.HoughSegmentDetector.t() | {:error, String.t()}
  def setTheta(self, theta) when is_float(theta)
  do
    positional = [
      theta: Evision.Internal.Structurise.from_struct(theta)
    ]
    :evision_nif.cuda_cuda_HoughSegmentDetector_setTheta(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setThreshold

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughSegmentDetector.t()`
  - **threshold**: `integer()`

  Python prototype (for reference only):
  ```python3
  setThreshold(threshold) -> None
  ```
  """
  @spec setThreshold(Evision.CUDA.HoughSegmentDetector.t(), integer()) :: Evision.CUDA.HoughSegmentDetector.t() | {:error, String.t()}
  def setThreshold(self, threshold) when is_integer(threshold)
  do
    positional = [
      threshold: Evision.Internal.Structurise.from_struct(threshold)
    ]
    :evision_nif.cuda_cuda_HoughSegmentDetector_setThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughSegmentDetector.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.CUDA.HoughSegmentDetector.t(), Evision.FileStorage.t(), binary()) :: Evision.CUDA.HoughSegmentDetector.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.cuda_HoughSegmentDetector_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughSegmentDetector.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.CUDA.HoughSegmentDetector.t(), Evision.FileStorage.t()) :: Evision.CUDA.HoughSegmentDetector.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.cuda_HoughSegmentDetector_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
