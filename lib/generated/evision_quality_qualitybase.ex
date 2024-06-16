defmodule Evision.Quality.QualityBase do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Quality.QualityBase` struct.

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
  def to_struct({:ok, %{class: Evision.Quality.QualityBase, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Quality.QualityBase, ref: ref}) do
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
  Implements Algorithm::clear()

  ##### Positional Arguments
  - **self**: `Evision.Quality.QualityBase.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.Quality.QualityBase.t()) :: Evision.Quality.QualityBase.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.quality_quality_QualityBase_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Compute quality score per channel with the per-channel score in each element of the resulting cv::Scalar.  See specific algorithm for interpreting result scores

  ##### Positional Arguments
  - **self**: `Evision.Quality.QualityBase.t()`
  - **img**: `Evision.Mat`.

    comparison image, or image to evalute for no-reference quality algorithms

  ##### Return
  - **retval**: `Evision.scalar().t()`

  Python prototype (for reference only):
  ```python3
  compute(img) -> retval
  ```
  """
  @spec compute(Evision.Quality.QualityBase.t(), Evision.Mat.maybe_mat_in()) :: Evision.scalar() | {:error, String.t()}
  def compute(self, img) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img)
    ]
    :evision_nif.quality_quality_QualityBase_compute(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Implements Algorithm::empty()

  ##### Positional Arguments
  - **self**: `Evision.Quality.QualityBase.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.Quality.QualityBase.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.quality_quality_QualityBase_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.Quality.QualityBase.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.Quality.QualityBase.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.quality_QualityBase_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns output quality map that was generated during computation, if supported by the algorithm

  ##### Positional Arguments
  - **self**: `Evision.Quality.QualityBase.t()`

  ##### Return
  - **dst**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  getQualityMap([, dst]) -> dst
  ```
  """
  @spec getQualityMap(Evision.Quality.QualityBase.t(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def getQualityMap(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    positional = [
    ]
    :evision_nif.quality_quality_QualityBase_getQualityMap(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Returns output quality map that was generated during computation, if supported by the algorithm

  ##### Positional Arguments
  - **self**: `Evision.Quality.QualityBase.t()`

  ##### Return
  - **dst**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  getQualityMap([, dst]) -> dst
  ```
  """
  @spec getQualityMap(Evision.Quality.QualityBase.t()) :: Evision.Mat.t() | {:error, String.t()}
  def getQualityMap(self) do
    positional = [
    ]
    :evision_nif.quality_quality_QualityBase_getQualityMap(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.Quality.QualityBase.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.Quality.QualityBase.t(), Evision.FileNode.t()) :: Evision.Quality.QualityBase.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.quality_QualityBase_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.Quality.QualityBase.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.Quality.QualityBase.t(), binary()) :: Evision.Quality.QualityBase.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.quality_QualityBase_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.Quality.QualityBase.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.Quality.QualityBase.t(), Evision.FileStorage.t(), binary()) :: Evision.Quality.QualityBase.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.quality_QualityBase_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.Quality.QualityBase.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.Quality.QualityBase.t(), Evision.FileStorage.t()) :: Evision.Quality.QualityBase.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.quality_QualityBase_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
