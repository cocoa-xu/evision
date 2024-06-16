defmodule Evision.CUDA.DisparityBilateralFilter do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `CUDA.DisparityBilateralFilter` struct.

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
  def to_struct({:ok, %{class: Evision.CUDA.DisparityBilateralFilter, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.CUDA.DisparityBilateralFilter, ref: ref}) do
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
  Refines a disparity map using joint bilateral filtering.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DisparityBilateralFilter.t()`
  - **disparity**: `Evision.Mat`.

    Input disparity map. CV_8UC1 and CV_16SC1 types are supported.

  - **image**: `Evision.Mat`.

    Input image. CV_8UC1 and CV_8UC3 types are supported.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination disparity map. It has the same size and type as disparity .

  Python prototype (for reference only):
  ```python3
  apply(disparity, image[, dst[, stream]]) -> dst
  ```
  #### Variant 2:
  Refines a disparity map using joint bilateral filtering.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DisparityBilateralFilter.t()`
  - **disparity**: `Evision.CUDA.GpuMat.t()`.

    Input disparity map. CV_8UC1 and CV_16SC1 types are supported.

  - **image**: `Evision.CUDA.GpuMat.t()`.

    Input image. CV_8UC1 and CV_8UC3 types are supported.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination disparity map. It has the same size and type as disparity .

  Python prototype (for reference only):
  ```python3
  apply(disparity, image[, dst[, stream]]) -> dst
  ```

  """
  @spec apply(Evision.CUDA.DisparityBilateralFilter.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def apply(self, disparity, image, opts) when (is_struct(disparity, Evision.Mat) or is_struct(disparity, Nx.Tensor) or is_number(disparity) or is_tuple(disparity)) and (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      disparity: Evision.Internal.Structurise.from_struct(disparity),
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.cuda_cuda_DisparityBilateralFilter_apply(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec apply(Evision.CUDA.DisparityBilateralFilter.t(), Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t(), [{:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def apply(self, disparity, image, opts) when is_struct(disparity, Evision.CUDA.GpuMat) and is_struct(image, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      disparity: Evision.Internal.Structurise.from_struct(disparity),
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.cuda_cuda_DisparityBilateralFilter_apply(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Refines a disparity map using joint bilateral filtering.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DisparityBilateralFilter.t()`
  - **disparity**: `Evision.Mat`.

    Input disparity map. CV_8UC1 and CV_16SC1 types are supported.

  - **image**: `Evision.Mat`.

    Input image. CV_8UC1 and CV_8UC3 types are supported.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination disparity map. It has the same size and type as disparity .

  Python prototype (for reference only):
  ```python3
  apply(disparity, image[, dst[, stream]]) -> dst
  ```
  #### Variant 2:
  Refines a disparity map using joint bilateral filtering.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DisparityBilateralFilter.t()`
  - **disparity**: `Evision.CUDA.GpuMat.t()`.

    Input disparity map. CV_8UC1 and CV_16SC1 types are supported.

  - **image**: `Evision.CUDA.GpuMat.t()`.

    Input image. CV_8UC1 and CV_8UC3 types are supported.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination disparity map. It has the same size and type as disparity .

  Python prototype (for reference only):
  ```python3
  apply(disparity, image[, dst[, stream]]) -> dst
  ```

  """
  @spec apply(Evision.CUDA.DisparityBilateralFilter.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def apply(self, disparity, image) when (is_struct(disparity, Evision.Mat) or is_struct(disparity, Nx.Tensor) or is_number(disparity) or is_tuple(disparity)) and (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image))
  do
    positional = [
      disparity: Evision.Internal.Structurise.from_struct(disparity),
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.cuda_cuda_DisparityBilateralFilter_apply(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec apply(Evision.CUDA.DisparityBilateralFilter.t(), Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def apply(self, disparity, image) when is_struct(disparity, Evision.CUDA.GpuMat) and is_struct(image, Evision.CUDA.GpuMat)
  do
    positional = [
      disparity: Evision.Internal.Structurise.from_struct(disparity),
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.cuda_cuda_DisparityBilateralFilter_apply(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Clears the algorithm state

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DisparityBilateralFilter.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.CUDA.DisparityBilateralFilter.t()) :: Evision.CUDA.DisparityBilateralFilter.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.cuda_DisparityBilateralFilter_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DisparityBilateralFilter.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.CUDA.DisparityBilateralFilter.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.cuda_DisparityBilateralFilter_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DisparityBilateralFilter.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.CUDA.DisparityBilateralFilter.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.cuda_DisparityBilateralFilter_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getEdgeThreshold

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DisparityBilateralFilter.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getEdgeThreshold() -> retval
  ```
  """
  @spec getEdgeThreshold(Evision.CUDA.DisparityBilateralFilter.t()) :: number() | {:error, String.t()}
  def getEdgeThreshold(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DisparityBilateralFilter_getEdgeThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMaxDiscThreshold

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DisparityBilateralFilter.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getMaxDiscThreshold() -> retval
  ```
  """
  @spec getMaxDiscThreshold(Evision.CUDA.DisparityBilateralFilter.t()) :: number() | {:error, String.t()}
  def getMaxDiscThreshold(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DisparityBilateralFilter_getMaxDiscThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getNumDisparities

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DisparityBilateralFilter.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getNumDisparities() -> retval
  ```
  """
  @spec getNumDisparities(Evision.CUDA.DisparityBilateralFilter.t()) :: integer() | {:error, String.t()}
  def getNumDisparities(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DisparityBilateralFilter_getNumDisparities(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getNumIters

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DisparityBilateralFilter.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getNumIters() -> retval
  ```
  """
  @spec getNumIters(Evision.CUDA.DisparityBilateralFilter.t()) :: integer() | {:error, String.t()}
  def getNumIters(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DisparityBilateralFilter_getNumIters(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getRadius

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DisparityBilateralFilter.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getRadius() -> retval
  ```
  """
  @spec getRadius(Evision.CUDA.DisparityBilateralFilter.t()) :: integer() | {:error, String.t()}
  def getRadius(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DisparityBilateralFilter_getRadius(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getSigmaRange

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DisparityBilateralFilter.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getSigmaRange() -> retval
  ```
  """
  @spec getSigmaRange(Evision.CUDA.DisparityBilateralFilter.t()) :: number() | {:error, String.t()}
  def getSigmaRange(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DisparityBilateralFilter_getSigmaRange(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DisparityBilateralFilter.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.CUDA.DisparityBilateralFilter.t(), Evision.FileNode.t()) :: Evision.CUDA.DisparityBilateralFilter.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.cuda_DisparityBilateralFilter_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DisparityBilateralFilter.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.CUDA.DisparityBilateralFilter.t(), binary()) :: Evision.CUDA.DisparityBilateralFilter.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.cuda_DisparityBilateralFilter_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setEdgeThreshold

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DisparityBilateralFilter.t()`
  - **edge_threshold**: `double`

  Python prototype (for reference only):
  ```python3
  setEdgeThreshold(edge_threshold) -> None
  ```
  """
  @spec setEdgeThreshold(Evision.CUDA.DisparityBilateralFilter.t(), number()) :: Evision.CUDA.DisparityBilateralFilter.t() | {:error, String.t()}
  def setEdgeThreshold(self, edge_threshold) when is_number(edge_threshold)
  do
    positional = [
      edge_threshold: Evision.Internal.Structurise.from_struct(edge_threshold)
    ]
    :evision_nif.cuda_cuda_DisparityBilateralFilter_setEdgeThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMaxDiscThreshold

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DisparityBilateralFilter.t()`
  - **max_disc_threshold**: `double`

  Python prototype (for reference only):
  ```python3
  setMaxDiscThreshold(max_disc_threshold) -> None
  ```
  """
  @spec setMaxDiscThreshold(Evision.CUDA.DisparityBilateralFilter.t(), number()) :: Evision.CUDA.DisparityBilateralFilter.t() | {:error, String.t()}
  def setMaxDiscThreshold(self, max_disc_threshold) when is_number(max_disc_threshold)
  do
    positional = [
      max_disc_threshold: Evision.Internal.Structurise.from_struct(max_disc_threshold)
    ]
    :evision_nif.cuda_cuda_DisparityBilateralFilter_setMaxDiscThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setNumDisparities

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DisparityBilateralFilter.t()`
  - **numDisparities**: `integer()`

  Python prototype (for reference only):
  ```python3
  setNumDisparities(numDisparities) -> None
  ```
  """
  @spec setNumDisparities(Evision.CUDA.DisparityBilateralFilter.t(), integer()) :: Evision.CUDA.DisparityBilateralFilter.t() | {:error, String.t()}
  def setNumDisparities(self, numDisparities) when is_integer(numDisparities)
  do
    positional = [
      numDisparities: Evision.Internal.Structurise.from_struct(numDisparities)
    ]
    :evision_nif.cuda_cuda_DisparityBilateralFilter_setNumDisparities(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setNumIters

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DisparityBilateralFilter.t()`
  - **iters**: `integer()`

  Python prototype (for reference only):
  ```python3
  setNumIters(iters) -> None
  ```
  """
  @spec setNumIters(Evision.CUDA.DisparityBilateralFilter.t(), integer()) :: Evision.CUDA.DisparityBilateralFilter.t() | {:error, String.t()}
  def setNumIters(self, iters) when is_integer(iters)
  do
    positional = [
      iters: Evision.Internal.Structurise.from_struct(iters)
    ]
    :evision_nif.cuda_cuda_DisparityBilateralFilter_setNumIters(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setRadius

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DisparityBilateralFilter.t()`
  - **radius**: `integer()`

  Python prototype (for reference only):
  ```python3
  setRadius(radius) -> None
  ```
  """
  @spec setRadius(Evision.CUDA.DisparityBilateralFilter.t(), integer()) :: Evision.CUDA.DisparityBilateralFilter.t() | {:error, String.t()}
  def setRadius(self, radius) when is_integer(radius)
  do
    positional = [
      radius: Evision.Internal.Structurise.from_struct(radius)
    ]
    :evision_nif.cuda_cuda_DisparityBilateralFilter_setRadius(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setSigmaRange

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DisparityBilateralFilter.t()`
  - **sigma_range**: `double`

  Python prototype (for reference only):
  ```python3
  setSigmaRange(sigma_range) -> None
  ```
  """
  @spec setSigmaRange(Evision.CUDA.DisparityBilateralFilter.t(), number()) :: Evision.CUDA.DisparityBilateralFilter.t() | {:error, String.t()}
  def setSigmaRange(self, sigma_range) when is_number(sigma_range)
  do
    positional = [
      sigma_range: Evision.Internal.Structurise.from_struct(sigma_range)
    ]
    :evision_nif.cuda_cuda_DisparityBilateralFilter_setSigmaRange(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DisparityBilateralFilter.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.CUDA.DisparityBilateralFilter.t(), Evision.FileStorage.t(), binary()) :: Evision.CUDA.DisparityBilateralFilter.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.cuda_DisparityBilateralFilter_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DisparityBilateralFilter.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.CUDA.DisparityBilateralFilter.t(), Evision.FileStorage.t()) :: Evision.CUDA.DisparityBilateralFilter.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.cuda_DisparityBilateralFilter_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
