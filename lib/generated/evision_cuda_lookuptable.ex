defmodule Evision.CUDA.LookUpTable do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `CUDA.LookUpTable` struct.

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
  def to_struct({:ok, %{class: Evision.CUDA.LookUpTable, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.CUDA.LookUpTable, ref: ref}) do
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
  - **self**: `Evision.CUDA.LookUpTable.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.CUDA.LookUpTable.t()) :: Evision.CUDA.LookUpTable.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.cuda_LookUpTable_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.CUDA.LookUpTable.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.CUDA.LookUpTable.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.cuda_LookUpTable_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.CUDA.LookUpTable.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.CUDA.LookUpTable.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.cuda_LookUpTable_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.CUDA.LookUpTable.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.CUDA.LookUpTable.t(), Evision.FileNode.t()) :: Evision.CUDA.LookUpTable.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.cuda_LookUpTable_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.CUDA.LookUpTable.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.CUDA.LookUpTable.t(), binary()) :: Evision.CUDA.LookUpTable.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.cuda_LookUpTable_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Transforms the source matrix into the destination matrix using the given look-up table:
  dst(I) = lut(src(I)) .

  ##### Positional Arguments
  - **self**: `Evision.CUDA.LookUpTable.t()`
  - **src**: `Evision.Mat`.

    Source matrix. CV_8UC1 and CV_8UC3 matrices are supported for now.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination matrix.

  Python prototype (for reference only):
  ```python3
  transform(src[, dst[, stream]]) -> dst
  ```
  #### Variant 2:
  Transforms the source matrix into the destination matrix using the given look-up table:
  dst(I) = lut(src(I)) .

  ##### Positional Arguments
  - **self**: `Evision.CUDA.LookUpTable.t()`
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source matrix. CV_8UC1 and CV_8UC3 matrices are supported for now.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination matrix.

  Python prototype (for reference only):
  ```python3
  transform(src[, dst[, stream]]) -> dst
  ```

  """
  @spec transform(Evision.CUDA.LookUpTable.t(), Evision.Mat.maybe_mat_in(), [{:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def transform(self, src, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_cuda_LookUpTable_transform(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec transform(Evision.CUDA.LookUpTable.t(), Evision.CUDA.GpuMat.t(), [{:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def transform(self, src, opts) when is_struct(src, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_cuda_LookUpTable_transform(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Transforms the source matrix into the destination matrix using the given look-up table:
  dst(I) = lut(src(I)) .

  ##### Positional Arguments
  - **self**: `Evision.CUDA.LookUpTable.t()`
  - **src**: `Evision.Mat`.

    Source matrix. CV_8UC1 and CV_8UC3 matrices are supported for now.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination matrix.

  Python prototype (for reference only):
  ```python3
  transform(src[, dst[, stream]]) -> dst
  ```
  #### Variant 2:
  Transforms the source matrix into the destination matrix using the given look-up table:
  dst(I) = lut(src(I)) .

  ##### Positional Arguments
  - **self**: `Evision.CUDA.LookUpTable.t()`
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source matrix. CV_8UC1 and CV_8UC3 matrices are supported for now.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination matrix.

  Python prototype (for reference only):
  ```python3
  transform(src[, dst[, stream]]) -> dst
  ```

  """
  @spec transform(Evision.CUDA.LookUpTable.t(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def transform(self, src) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_cuda_LookUpTable_transform(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec transform(Evision.CUDA.LookUpTable.t(), Evision.CUDA.GpuMat.t()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def transform(self, src) when is_struct(src, Evision.CUDA.GpuMat)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.cuda_cuda_LookUpTable_transform(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.CUDA.LookUpTable.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.CUDA.LookUpTable.t(), Evision.FileStorage.t(), binary()) :: Evision.CUDA.LookUpTable.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.cuda_LookUpTable_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.CUDA.LookUpTable.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.CUDA.LookUpTable.t(), Evision.FileStorage.t()) :: Evision.CUDA.LookUpTable.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.cuda_LookUpTable_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
