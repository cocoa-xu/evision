defmodule Evision.CUDA.CornersDetector do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `CUDA.CornersDetector` struct.

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
  def to_struct({:ok, %{class: Evision.CUDA.CornersDetector, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.CUDA.CornersDetector, ref: ref}) do
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
  - **self**: `Evision.CUDA.CornersDetector.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.CUDA.CornersDetector.t()) :: Evision.CUDA.CornersDetector.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.cuda_CornersDetector_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Determines strong corners on an image.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.CornersDetector.t()`
  - **image**: `Evision.Mat`.

    Input 8-bit or floating-point 32-bit, single-channel image.

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.

    Optional region of interest. If the image is not empty (it needs to have the type
    CV_8UC1 and the same size as image ), it specifies the region in which the corners are detected.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **corners**: `Evision.Mat.t()`.

    Output vector of detected corners (1-row matrix with CV_32FC2 type with corners
    positions).

  Python prototype (for reference only):
  ```python3
  detect(image[, corners[, mask[, stream]]]) -> corners
  ```
  #### Variant 2:
  Determines strong corners on an image.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.CornersDetector.t()`
  - **image**: `Evision.CUDA.GpuMat.t()`.

    Input 8-bit or floating-point 32-bit, single-channel image.

  ##### Keyword Arguments
  - **mask**: `Evision.CUDA.GpuMat.t()`.

    Optional region of interest. If the image is not empty (it needs to have the type
    CV_8UC1 and the same size as image ), it specifies the region in which the corners are detected.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **corners**: `Evision.CUDA.GpuMat.t()`.

    Output vector of detected corners (1-row matrix with CV_32FC2 type with corners
    positions).

  Python prototype (for reference only):
  ```python3
  detect(image[, corners[, mask[, stream]]]) -> corners
  ```

  """
  @spec detect(Evision.CUDA.CornersDetector.t(), Evision.Mat.maybe_mat_in(), [{:mask, term()} | {:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def detect(self, image, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:mask, :stream])
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.cuda_cuda_CornersDetector_detect(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec detect(Evision.CUDA.CornersDetector.t(), Evision.CUDA.GpuMat.t(), [{:mask, term()} | {:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def detect(self, image, opts) when is_struct(image, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:mask, :stream])
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.cuda_cuda_CornersDetector_detect(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Determines strong corners on an image.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.CornersDetector.t()`
  - **image**: `Evision.Mat`.

    Input 8-bit or floating-point 32-bit, single-channel image.

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.

    Optional region of interest. If the image is not empty (it needs to have the type
    CV_8UC1 and the same size as image ), it specifies the region in which the corners are detected.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **corners**: `Evision.Mat.t()`.

    Output vector of detected corners (1-row matrix with CV_32FC2 type with corners
    positions).

  Python prototype (for reference only):
  ```python3
  detect(image[, corners[, mask[, stream]]]) -> corners
  ```
  #### Variant 2:
  Determines strong corners on an image.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.CornersDetector.t()`
  - **image**: `Evision.CUDA.GpuMat.t()`.

    Input 8-bit or floating-point 32-bit, single-channel image.

  ##### Keyword Arguments
  - **mask**: `Evision.CUDA.GpuMat.t()`.

    Optional region of interest. If the image is not empty (it needs to have the type
    CV_8UC1 and the same size as image ), it specifies the region in which the corners are detected.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **corners**: `Evision.CUDA.GpuMat.t()`.

    Output vector of detected corners (1-row matrix with CV_32FC2 type with corners
    positions).

  Python prototype (for reference only):
  ```python3
  detect(image[, corners[, mask[, stream]]]) -> corners
  ```

  """
  @spec detect(Evision.CUDA.CornersDetector.t(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def detect(self, image) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.cuda_cuda_CornersDetector_detect(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec detect(Evision.CUDA.CornersDetector.t(), Evision.CUDA.GpuMat.t()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def detect(self, image) when is_struct(image, Evision.CUDA.GpuMat)
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.cuda_cuda_CornersDetector_detect(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.CUDA.CornersDetector.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.CUDA.CornersDetector.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.cuda_CornersDetector_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.CUDA.CornersDetector.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.CUDA.CornersDetector.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.cuda_CornersDetector_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.CUDA.CornersDetector.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.CUDA.CornersDetector.t(), Evision.FileNode.t()) :: Evision.CUDA.CornersDetector.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.cuda_CornersDetector_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.CUDA.CornersDetector.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.CUDA.CornersDetector.t(), binary()) :: Evision.CUDA.CornersDetector.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.cuda_CornersDetector_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMaxCorners

  ##### Positional Arguments
  - **self**: `Evision.CUDA.CornersDetector.t()`
  - **maxCorners**: `integer()`

  Python prototype (for reference only):
  ```python3
  setMaxCorners(maxCorners) -> None
  ```
  """
  @spec setMaxCorners(Evision.CUDA.CornersDetector.t(), integer()) :: Evision.CUDA.CornersDetector.t() | {:error, String.t()}
  def setMaxCorners(self, maxCorners) when is_integer(maxCorners)
  do
    positional = [
      maxCorners: Evision.Internal.Structurise.from_struct(maxCorners)
    ]
    :evision_nif.cuda_cuda_CornersDetector_setMaxCorners(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMinDistance

  ##### Positional Arguments
  - **self**: `Evision.CUDA.CornersDetector.t()`
  - **minDistance**: `double`

  Python prototype (for reference only):
  ```python3
  setMinDistance(minDistance) -> None
  ```
  """
  @spec setMinDistance(Evision.CUDA.CornersDetector.t(), number()) :: Evision.CUDA.CornersDetector.t() | {:error, String.t()}
  def setMinDistance(self, minDistance) when is_number(minDistance)
  do
    positional = [
      minDistance: Evision.Internal.Structurise.from_struct(minDistance)
    ]
    :evision_nif.cuda_cuda_CornersDetector_setMinDistance(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.CUDA.CornersDetector.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.CUDA.CornersDetector.t(), Evision.FileStorage.t(), binary()) :: Evision.CUDA.CornersDetector.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.cuda_CornersDetector_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.CUDA.CornersDetector.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.CUDA.CornersDetector.t(), Evision.FileStorage.t()) :: Evision.CUDA.CornersDetector.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.cuda_CornersDetector_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
