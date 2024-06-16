defmodule Evision.XImgProc.RFFeatureGetter do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `XImgProc.RFFeatureGetter` struct.

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
  def to_struct({:ok, %{class: Evision.XImgProc.RFFeatureGetter, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.XImgProc.RFFeatureGetter, ref: ref}) do
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
  - **self**: `Evision.XImgProc.RFFeatureGetter.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.XImgProc.RFFeatureGetter.t()) :: Evision.XImgProc.RFFeatureGetter.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.ximgproc_RFFeatureGetter_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.RFFeatureGetter.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.XImgProc.RFFeatureGetter.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.ximgproc_RFFeatureGetter_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.RFFeatureGetter.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.XImgProc.RFFeatureGetter.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.ximgproc_RFFeatureGetter_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getFeatures

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.RFFeatureGetter.t()`
  - **src**: `Evision.Mat`
  - **features**: `Evision.Mat`
  - **gnrmRad**: `integer()`
  - **gsmthRad**: `integer()`
  - **shrink**: `integer()`
  - **outNum**: `integer()`
  - **gradNum**: `integer()`

  Python prototype (for reference only):
  ```python3
  getFeatures(src, features, gnrmRad, gsmthRad, shrink, outNum, gradNum) -> None
  ```
  """
  @spec getFeatures(Evision.XImgProc.RFFeatureGetter.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), integer(), integer(), integer(), integer(), integer()) :: Evision.XImgProc.RFFeatureGetter.t() | {:error, String.t()}
  def getFeatures(self, src, features, gnrmRad, gsmthRad, shrink, outNum, gradNum) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (is_struct(features, Evision.Mat) or is_struct(features, Nx.Tensor) or is_number(features) or is_tuple(features)) and is_integer(gnrmRad) and is_integer(gsmthRad) and is_integer(shrink) and is_integer(outNum) and is_integer(gradNum)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      features: Evision.Internal.Structurise.from_struct(features),
      gnrmRad: Evision.Internal.Structurise.from_struct(gnrmRad),
      gsmthRad: Evision.Internal.Structurise.from_struct(gsmthRad),
      shrink: Evision.Internal.Structurise.from_struct(shrink),
      outNum: Evision.Internal.Structurise.from_struct(outNum),
      gradNum: Evision.Internal.Structurise.from_struct(gradNum)
    ]
    :evision_nif.ximgproc_ximgproc_RFFeatureGetter_getFeatures(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.RFFeatureGetter.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.XImgProc.RFFeatureGetter.t(), Evision.FileNode.t()) :: Evision.XImgProc.RFFeatureGetter.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.ximgproc_RFFeatureGetter_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.RFFeatureGetter.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.XImgProc.RFFeatureGetter.t(), binary()) :: Evision.XImgProc.RFFeatureGetter.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.ximgproc_RFFeatureGetter_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.RFFeatureGetter.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.XImgProc.RFFeatureGetter.t(), Evision.FileStorage.t(), binary()) :: Evision.XImgProc.RFFeatureGetter.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.ximgproc_RFFeatureGetter_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.RFFeatureGetter.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.XImgProc.RFFeatureGetter.t(), Evision.FileStorage.t()) :: Evision.XImgProc.RFFeatureGetter.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.ximgproc_RFFeatureGetter_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
