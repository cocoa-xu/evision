defmodule Evision.CUDA.HoughCirclesDetector do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `CUDA.HoughCirclesDetector` struct.

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
  def to_struct({:ok, %{class: Evision.CUDA.HoughCirclesDetector, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.CUDA.HoughCirclesDetector, ref: ref}) do
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
  - **self**: `Evision.CUDA.HoughCirclesDetector.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.CUDA.HoughCirclesDetector.t()) :: Evision.CUDA.HoughCirclesDetector.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.cuda_HoughCirclesDetector_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Finds circles in a grayscale image using the Hough transform.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughCirclesDetector.t()`
  - **src**: `Evision.Mat`.

    8-bit, single-channel grayscale input image.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **circles**: `Evision.Mat.t()`.

    Output vector of found circles. Each vector is encoded as a 3-element
    floating-point vector \\f$(x, y, radius)\\f$ .

  @sa HoughCircles

  Python prototype (for reference only):
  ```python3
  detect(src[, circles[, stream]]) -> circles
  ```
  #### Variant 2:
  Finds circles in a grayscale image using the Hough transform.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughCirclesDetector.t()`
  - **src**: `Evision.CUDA.GpuMat.t()`.

    8-bit, single-channel grayscale input image.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **circles**: `Evision.CUDA.GpuMat.t()`.

    Output vector of found circles. Each vector is encoded as a 3-element
    floating-point vector \\f$(x, y, radius)\\f$ .

  @sa HoughCircles

  Python prototype (for reference only):
  ```python3
  detect(src[, circles[, stream]]) -> circles
  ```

  """
  @spec detect(Evision.CUDA.HoughCirclesDetector.t(), Evision.Mat.maybe_mat_in(), [{:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def detect(self, src, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_cuda_HoughCirclesDetector_detect(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec detect(Evision.CUDA.HoughCirclesDetector.t(), Evision.CUDA.GpuMat.t(), [{:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def detect(self, src, opts) when is_struct(src, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_cuda_HoughCirclesDetector_detect(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Finds circles in a grayscale image using the Hough transform.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughCirclesDetector.t()`
  - **src**: `Evision.Mat`.

    8-bit, single-channel grayscale input image.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **circles**: `Evision.Mat.t()`.

    Output vector of found circles. Each vector is encoded as a 3-element
    floating-point vector \\f$(x, y, radius)\\f$ .

  @sa HoughCircles

  Python prototype (for reference only):
  ```python3
  detect(src[, circles[, stream]]) -> circles
  ```
  #### Variant 2:
  Finds circles in a grayscale image using the Hough transform.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughCirclesDetector.t()`
  - **src**: `Evision.CUDA.GpuMat.t()`.

    8-bit, single-channel grayscale input image.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **circles**: `Evision.CUDA.GpuMat.t()`.

    Output vector of found circles. Each vector is encoded as a 3-element
    floating-point vector \\f$(x, y, radius)\\f$ .

  @sa HoughCircles

  Python prototype (for reference only):
  ```python3
  detect(src[, circles[, stream]]) -> circles
  ```

  """
  @spec detect(Evision.CUDA.HoughCirclesDetector.t(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def detect(self, src) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_cuda_HoughCirclesDetector_detect(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec detect(Evision.CUDA.HoughCirclesDetector.t(), Evision.CUDA.GpuMat.t()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def detect(self, src) when is_struct(src, Evision.CUDA.GpuMat)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_cuda_HoughCirclesDetector_detect(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughCirclesDetector.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.CUDA.HoughCirclesDetector.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.cuda_HoughCirclesDetector_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getCannyThreshold

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughCirclesDetector.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getCannyThreshold() -> retval
  ```
  """
  @spec getCannyThreshold(Evision.CUDA.HoughCirclesDetector.t()) :: integer() | {:error, String.t()}
  def getCannyThreshold(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_HoughCirclesDetector_getCannyThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughCirclesDetector.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.CUDA.HoughCirclesDetector.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.cuda_HoughCirclesDetector_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDp

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughCirclesDetector.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  getDp() -> retval
  ```
  """
  @spec getDp(Evision.CUDA.HoughCirclesDetector.t()) :: number() | {:error, String.t()}
  def getDp(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_HoughCirclesDetector_getDp(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMaxCircles

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughCirclesDetector.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getMaxCircles() -> retval
  ```
  """
  @spec getMaxCircles(Evision.CUDA.HoughCirclesDetector.t()) :: integer() | {:error, String.t()}
  def getMaxCircles(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_HoughCirclesDetector_getMaxCircles(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMaxRadius

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughCirclesDetector.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getMaxRadius() -> retval
  ```
  """
  @spec getMaxRadius(Evision.CUDA.HoughCirclesDetector.t()) :: integer() | {:error, String.t()}
  def getMaxRadius(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_HoughCirclesDetector_getMaxRadius(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMinDist

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughCirclesDetector.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  getMinDist() -> retval
  ```
  """
  @spec getMinDist(Evision.CUDA.HoughCirclesDetector.t()) :: number() | {:error, String.t()}
  def getMinDist(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_HoughCirclesDetector_getMinDist(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMinRadius

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughCirclesDetector.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getMinRadius() -> retval
  ```
  """
  @spec getMinRadius(Evision.CUDA.HoughCirclesDetector.t()) :: integer() | {:error, String.t()}
  def getMinRadius(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_HoughCirclesDetector_getMinRadius(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getVotesThreshold

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughCirclesDetector.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getVotesThreshold() -> retval
  ```
  """
  @spec getVotesThreshold(Evision.CUDA.HoughCirclesDetector.t()) :: integer() | {:error, String.t()}
  def getVotesThreshold(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_HoughCirclesDetector_getVotesThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughCirclesDetector.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.CUDA.HoughCirclesDetector.t(), Evision.FileNode.t()) :: Evision.CUDA.HoughCirclesDetector.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.cuda_HoughCirclesDetector_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughCirclesDetector.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.CUDA.HoughCirclesDetector.t(), binary()) :: Evision.CUDA.HoughCirclesDetector.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.cuda_HoughCirclesDetector_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setCannyThreshold

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughCirclesDetector.t()`
  - **cannyThreshold**: `integer()`

  Python prototype (for reference only):
  ```python3
  setCannyThreshold(cannyThreshold) -> None
  ```
  """
  @spec setCannyThreshold(Evision.CUDA.HoughCirclesDetector.t(), integer()) :: Evision.CUDA.HoughCirclesDetector.t() | {:error, String.t()}
  def setCannyThreshold(self, cannyThreshold) when is_integer(cannyThreshold)
  do
    positional = [
      cannyThreshold: Evision.Internal.Structurise.from_struct(cannyThreshold)
    ]
    :evision_nif.cuda_cuda_HoughCirclesDetector_setCannyThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setDp

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughCirclesDetector.t()`
  - **dp**: `float`

  Python prototype (for reference only):
  ```python3
  setDp(dp) -> None
  ```
  """
  @spec setDp(Evision.CUDA.HoughCirclesDetector.t(), number()) :: Evision.CUDA.HoughCirclesDetector.t() | {:error, String.t()}
  def setDp(self, dp) when is_float(dp)
  do
    positional = [
      dp: Evision.Internal.Structurise.from_struct(dp)
    ]
    :evision_nif.cuda_cuda_HoughCirclesDetector_setDp(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMaxCircles

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughCirclesDetector.t()`
  - **maxCircles**: `integer()`

  Python prototype (for reference only):
  ```python3
  setMaxCircles(maxCircles) -> None
  ```
  """
  @spec setMaxCircles(Evision.CUDA.HoughCirclesDetector.t(), integer()) :: Evision.CUDA.HoughCirclesDetector.t() | {:error, String.t()}
  def setMaxCircles(self, maxCircles) when is_integer(maxCircles)
  do
    positional = [
      maxCircles: Evision.Internal.Structurise.from_struct(maxCircles)
    ]
    :evision_nif.cuda_cuda_HoughCirclesDetector_setMaxCircles(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMaxRadius

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughCirclesDetector.t()`
  - **maxRadius**: `integer()`

  Python prototype (for reference only):
  ```python3
  setMaxRadius(maxRadius) -> None
  ```
  """
  @spec setMaxRadius(Evision.CUDA.HoughCirclesDetector.t(), integer()) :: Evision.CUDA.HoughCirclesDetector.t() | {:error, String.t()}
  def setMaxRadius(self, maxRadius) when is_integer(maxRadius)
  do
    positional = [
      maxRadius: Evision.Internal.Structurise.from_struct(maxRadius)
    ]
    :evision_nif.cuda_cuda_HoughCirclesDetector_setMaxRadius(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMinDist

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughCirclesDetector.t()`
  - **minDist**: `float`

  Python prototype (for reference only):
  ```python3
  setMinDist(minDist) -> None
  ```
  """
  @spec setMinDist(Evision.CUDA.HoughCirclesDetector.t(), number()) :: Evision.CUDA.HoughCirclesDetector.t() | {:error, String.t()}
  def setMinDist(self, minDist) when is_float(minDist)
  do
    positional = [
      minDist: Evision.Internal.Structurise.from_struct(minDist)
    ]
    :evision_nif.cuda_cuda_HoughCirclesDetector_setMinDist(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMinRadius

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughCirclesDetector.t()`
  - **minRadius**: `integer()`

  Python prototype (for reference only):
  ```python3
  setMinRadius(minRadius) -> None
  ```
  """
  @spec setMinRadius(Evision.CUDA.HoughCirclesDetector.t(), integer()) :: Evision.CUDA.HoughCirclesDetector.t() | {:error, String.t()}
  def setMinRadius(self, minRadius) when is_integer(minRadius)
  do
    positional = [
      minRadius: Evision.Internal.Structurise.from_struct(minRadius)
    ]
    :evision_nif.cuda_cuda_HoughCirclesDetector_setMinRadius(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setVotesThreshold

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughCirclesDetector.t()`
  - **votesThreshold**: `integer()`

  Python prototype (for reference only):
  ```python3
  setVotesThreshold(votesThreshold) -> None
  ```
  """
  @spec setVotesThreshold(Evision.CUDA.HoughCirclesDetector.t(), integer()) :: Evision.CUDA.HoughCirclesDetector.t() | {:error, String.t()}
  def setVotesThreshold(self, votesThreshold) when is_integer(votesThreshold)
  do
    positional = [
      votesThreshold: Evision.Internal.Structurise.from_struct(votesThreshold)
    ]
    :evision_nif.cuda_cuda_HoughCirclesDetector_setVotesThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughCirclesDetector.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.CUDA.HoughCirclesDetector.t(), Evision.FileStorage.t(), binary()) :: Evision.CUDA.HoughCirclesDetector.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.cuda_HoughCirclesDetector_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HoughCirclesDetector.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.CUDA.HoughCirclesDetector.t(), Evision.FileStorage.t()) :: Evision.CUDA.HoughCirclesDetector.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.cuda_HoughCirclesDetector_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
