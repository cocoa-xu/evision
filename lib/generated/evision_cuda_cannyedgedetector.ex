defmodule Evision.CUDA.CannyEdgeDetector do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `CUDA.CannyEdgeDetector` struct.

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
  def to_struct({:ok, %{class: Evision.CUDA.CannyEdgeDetector, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.CUDA.CannyEdgeDetector, ref: ref}) do
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
  - **self**: `Evision.CUDA.CannyEdgeDetector.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.CUDA.CannyEdgeDetector.t()) :: Evision.CUDA.CannyEdgeDetector.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.cuda_CannyEdgeDetector_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  detect

  ##### Positional Arguments
  - **self**: `Evision.CUDA.CannyEdgeDetector.t()`
  - **dx**: `Evision.Mat`.

    First derivative of image in the vertical direction. Support only CV_32S type.

  - **dy**: `Evision.Mat`.

    First derivative of image in the horizontal direction. Support only CV_32S type.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **edges**: `Evision.Mat.t()`.

    Output edge map. It has the same size and type as image.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  detect(dx, dy[, edges[, stream]]) -> edges
  ```
  #### Variant 2:
  detect

  ##### Positional Arguments
  - **self**: `Evision.CUDA.CannyEdgeDetector.t()`
  - **dx**: `Evision.CUDA.GpuMat.t()`.

    First derivative of image in the vertical direction. Support only CV_32S type.

  - **dy**: `Evision.CUDA.GpuMat.t()`.

    First derivative of image in the horizontal direction. Support only CV_32S type.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **edges**: `Evision.CUDA.GpuMat.t()`.

    Output edge map. It has the same size and type as image.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  detect(dx, dy[, edges[, stream]]) -> edges
  ```

  """
  @spec detect(Evision.CUDA.CannyEdgeDetector.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def detect(self, dx, dy, opts) when (is_struct(dx, Evision.Mat) or is_struct(dx, Nx.Tensor) or is_number(dx) or is_tuple(dx)) and (is_struct(dy, Evision.Mat) or is_struct(dy, Nx.Tensor) or is_number(dy) or is_tuple(dy)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      dx: Evision.Internal.Structurise.from_struct(dx),
      dy: Evision.Internal.Structurise.from_struct(dy)
    ]
    :evision_nif.cuda_cuda_CannyEdgeDetector_detect(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec detect(Evision.CUDA.CannyEdgeDetector.t(), Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t(), [{:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def detect(self, dx, dy, opts) when is_struct(dx, Evision.CUDA.GpuMat) and is_struct(dy, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      dx: Evision.Internal.Structurise.from_struct(dx),
      dy: Evision.Internal.Structurise.from_struct(dy)
    ]
    :evision_nif.cuda_cuda_CannyEdgeDetector_detect(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  detect

  ##### Positional Arguments
  - **self**: `Evision.CUDA.CannyEdgeDetector.t()`
  - **dx**: `Evision.Mat`.

    First derivative of image in the vertical direction. Support only CV_32S type.

  - **dy**: `Evision.Mat`.

    First derivative of image in the horizontal direction. Support only CV_32S type.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **edges**: `Evision.Mat.t()`.

    Output edge map. It has the same size and type as image.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  detect(dx, dy[, edges[, stream]]) -> edges
  ```
  #### Variant 2:
  detect

  ##### Positional Arguments
  - **self**: `Evision.CUDA.CannyEdgeDetector.t()`
  - **dx**: `Evision.CUDA.GpuMat.t()`.

    First derivative of image in the vertical direction. Support only CV_32S type.

  - **dy**: `Evision.CUDA.GpuMat.t()`.

    First derivative of image in the horizontal direction. Support only CV_32S type.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **edges**: `Evision.CUDA.GpuMat.t()`.

    Output edge map. It has the same size and type as image.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  detect(dx, dy[, edges[, stream]]) -> edges
  ```
  #### Variant 3:
  Finds edges in an image using the @cite Canny86 algorithm.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.CannyEdgeDetector.t()`
  - **image**: `Evision.Mat`.

    Single-channel 8-bit input image.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **edges**: `Evision.Mat.t()`.

    Output edge map. It has the same size and type as image.

  Python prototype (for reference only):
  ```python3
  detect(image[, edges[, stream]]) -> edges
  ```
  #### Variant 4:
  Finds edges in an image using the @cite Canny86 algorithm.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.CannyEdgeDetector.t()`
  - **image**: `Evision.CUDA.GpuMat.t()`.

    Single-channel 8-bit input image.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **edges**: `Evision.CUDA.GpuMat.t()`.

    Output edge map. It has the same size and type as image.

  Python prototype (for reference only):
  ```python3
  detect(image[, edges[, stream]]) -> edges
  ```

  """
  @spec detect(Evision.CUDA.CannyEdgeDetector.t(), Evision.Mat.maybe_mat_in(), [{:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def detect(self, image, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.cuda_cuda_CannyEdgeDetector_detect(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec detect(Evision.CUDA.CannyEdgeDetector.t(), Evision.CUDA.GpuMat.t(), [{:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def detect(self, image, opts) when is_struct(image, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.cuda_cuda_CannyEdgeDetector_detect(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec detect(Evision.CUDA.CannyEdgeDetector.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def detect(self, dx, dy) when (is_struct(dx, Evision.Mat) or is_struct(dx, Nx.Tensor) or is_number(dx) or is_tuple(dx)) and (is_struct(dy, Evision.Mat) or is_struct(dy, Nx.Tensor) or is_number(dy) or is_tuple(dy))
  do
    positional = [
      dx: Evision.Internal.Structurise.from_struct(dx),
      dy: Evision.Internal.Structurise.from_struct(dy)
    ]
    :evision_nif.cuda_cuda_CannyEdgeDetector_detect(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec detect(Evision.CUDA.CannyEdgeDetector.t(), Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def detect(self, dx, dy) when is_struct(dx, Evision.CUDA.GpuMat) and is_struct(dy, Evision.CUDA.GpuMat)
  do
    positional = [
      dx: Evision.Internal.Structurise.from_struct(dx),
      dy: Evision.Internal.Structurise.from_struct(dy)
    ]
    :evision_nif.cuda_cuda_CannyEdgeDetector_detect(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Finds edges in an image using the @cite Canny86 algorithm.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.CannyEdgeDetector.t()`
  - **image**: `Evision.Mat`.

    Single-channel 8-bit input image.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **edges**: `Evision.Mat.t()`.

    Output edge map. It has the same size and type as image.

  Python prototype (for reference only):
  ```python3
  detect(image[, edges[, stream]]) -> edges
  ```
  #### Variant 2:
  Finds edges in an image using the @cite Canny86 algorithm.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.CannyEdgeDetector.t()`
  - **image**: `Evision.CUDA.GpuMat.t()`.

    Single-channel 8-bit input image.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **edges**: `Evision.CUDA.GpuMat.t()`.

    Output edge map. It has the same size and type as image.

  Python prototype (for reference only):
  ```python3
  detect(image[, edges[, stream]]) -> edges
  ```

  """
  @spec detect(Evision.CUDA.CannyEdgeDetector.t(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def detect(self, image) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.cuda_cuda_CannyEdgeDetector_detect(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec detect(Evision.CUDA.CannyEdgeDetector.t(), Evision.CUDA.GpuMat.t()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def detect(self, image) when is_struct(image, Evision.CUDA.GpuMat)
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.cuda_cuda_CannyEdgeDetector_detect(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.CUDA.CannyEdgeDetector.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.CUDA.CannyEdgeDetector.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.cuda_CannyEdgeDetector_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getAppertureSize

  ##### Positional Arguments
  - **self**: `Evision.CUDA.CannyEdgeDetector.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getAppertureSize() -> retval
  ```
  """
  @spec getAppertureSize(Evision.CUDA.CannyEdgeDetector.t()) :: integer() | {:error, String.t()}
  def getAppertureSize(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_CannyEdgeDetector_getAppertureSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.CUDA.CannyEdgeDetector.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.CUDA.CannyEdgeDetector.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.cuda_CannyEdgeDetector_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getHighThreshold

  ##### Positional Arguments
  - **self**: `Evision.CUDA.CannyEdgeDetector.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getHighThreshold() -> retval
  ```
  """
  @spec getHighThreshold(Evision.CUDA.CannyEdgeDetector.t()) :: number() | {:error, String.t()}
  def getHighThreshold(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_CannyEdgeDetector_getHighThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getL2Gradient

  ##### Positional Arguments
  - **self**: `Evision.CUDA.CannyEdgeDetector.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  getL2Gradient() -> retval
  ```
  """
  @spec getL2Gradient(Evision.CUDA.CannyEdgeDetector.t()) :: boolean() | {:error, String.t()}
  def getL2Gradient(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_CannyEdgeDetector_getL2Gradient(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getLowThreshold

  ##### Positional Arguments
  - **self**: `Evision.CUDA.CannyEdgeDetector.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getLowThreshold() -> retval
  ```
  """
  @spec getLowThreshold(Evision.CUDA.CannyEdgeDetector.t()) :: number() | {:error, String.t()}
  def getLowThreshold(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_CannyEdgeDetector_getLowThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.CUDA.CannyEdgeDetector.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.CUDA.CannyEdgeDetector.t(), Evision.FileNode.t()) :: Evision.CUDA.CannyEdgeDetector.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.cuda_CannyEdgeDetector_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.CUDA.CannyEdgeDetector.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.CUDA.CannyEdgeDetector.t(), binary()) :: Evision.CUDA.CannyEdgeDetector.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.cuda_CannyEdgeDetector_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setAppertureSize

  ##### Positional Arguments
  - **self**: `Evision.CUDA.CannyEdgeDetector.t()`
  - **apperture_size**: `integer()`

  Python prototype (for reference only):
  ```python3
  setAppertureSize(apperture_size) -> None
  ```
  """
  @spec setAppertureSize(Evision.CUDA.CannyEdgeDetector.t(), integer()) :: Evision.CUDA.CannyEdgeDetector.t() | {:error, String.t()}
  def setAppertureSize(self, apperture_size) when is_integer(apperture_size)
  do
    positional = [
      apperture_size: Evision.Internal.Structurise.from_struct(apperture_size)
    ]
    :evision_nif.cuda_cuda_CannyEdgeDetector_setAppertureSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setHighThreshold

  ##### Positional Arguments
  - **self**: `Evision.CUDA.CannyEdgeDetector.t()`
  - **high_thresh**: `double`

  Python prototype (for reference only):
  ```python3
  setHighThreshold(high_thresh) -> None
  ```
  """
  @spec setHighThreshold(Evision.CUDA.CannyEdgeDetector.t(), number()) :: Evision.CUDA.CannyEdgeDetector.t() | {:error, String.t()}
  def setHighThreshold(self, high_thresh) when is_number(high_thresh)
  do
    positional = [
      high_thresh: Evision.Internal.Structurise.from_struct(high_thresh)
    ]
    :evision_nif.cuda_cuda_CannyEdgeDetector_setHighThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setL2Gradient

  ##### Positional Arguments
  - **self**: `Evision.CUDA.CannyEdgeDetector.t()`
  - **l2gradient**: `bool`

  Python prototype (for reference only):
  ```python3
  setL2Gradient(L2gradient) -> None
  ```
  """
  @spec setL2Gradient(Evision.CUDA.CannyEdgeDetector.t(), boolean()) :: Evision.CUDA.CannyEdgeDetector.t() | {:error, String.t()}
  def setL2Gradient(self, l2gradient) when is_boolean(l2gradient)
  do
    positional = [
      l2gradient: Evision.Internal.Structurise.from_struct(l2gradient)
    ]
    :evision_nif.cuda_cuda_CannyEdgeDetector_setL2Gradient(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setLowThreshold

  ##### Positional Arguments
  - **self**: `Evision.CUDA.CannyEdgeDetector.t()`
  - **low_thresh**: `double`

  Python prototype (for reference only):
  ```python3
  setLowThreshold(low_thresh) -> None
  ```
  """
  @spec setLowThreshold(Evision.CUDA.CannyEdgeDetector.t(), number()) :: Evision.CUDA.CannyEdgeDetector.t() | {:error, String.t()}
  def setLowThreshold(self, low_thresh) when is_number(low_thresh)
  do
    positional = [
      low_thresh: Evision.Internal.Structurise.from_struct(low_thresh)
    ]
    :evision_nif.cuda_cuda_CannyEdgeDetector_setLowThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.CUDA.CannyEdgeDetector.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.CUDA.CannyEdgeDetector.t(), Evision.FileStorage.t(), binary()) :: Evision.CUDA.CannyEdgeDetector.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.cuda_CannyEdgeDetector_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.CUDA.CannyEdgeDetector.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.CUDA.CannyEdgeDetector.t(), Evision.FileStorage.t()) :: Evision.CUDA.CannyEdgeDetector.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.cuda_CannyEdgeDetector_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
