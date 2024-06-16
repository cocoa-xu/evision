defmodule Evision.LineDescriptor.BinaryDescriptor do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `LineDescriptor.BinaryDescriptor` struct.

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
  def to_struct({:ok, %{class: Evision.LineDescriptor.BinaryDescriptor, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.LineDescriptor.BinaryDescriptor, ref: ref}) do
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
  - **self**: `Evision.LineDescriptor.BinaryDescriptor.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.LineDescriptor.BinaryDescriptor.t()) :: Evision.LineDescriptor.BinaryDescriptor.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.line_descriptor_BinaryDescriptor_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Requires descriptors computation

  ##### Positional Arguments
  - **self**: `Evision.LineDescriptor.BinaryDescriptor.t()`
  - **image**: `Evision.Mat`.

    input image

  ##### Keyword Arguments
  - **returnFloatDescr**: `bool`.

    flag (when set to true, original non-binary descriptors are returned)

  ##### Return
  - **keylines**: `[Evision.LineDescriptor.KeyLine]`.

    vector containing lines for which descriptors must be computed

  - **descriptors**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  compute(image, keylines[, descriptors[, returnFloatDescr]]) -> keylines, descriptors
  ```
  """
  @spec compute(Evision.LineDescriptor.BinaryDescriptor.t(), Evision.Mat.maybe_mat_in(), list(Evision.LineDescriptor.KeyLine.t()), [{:returnFloatDescr, term()}] | nil) :: {list(Evision.LineDescriptor.KeyLine.t()), Evision.Mat.t()} | {:error, String.t()}
  def compute(self, image, keylines, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and is_list(keylines) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:returnFloatDescr])
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      keylines: Evision.Internal.Structurise.from_struct(keylines)
    ]
    :evision_nif.line_descriptor_line_descriptor_BinaryDescriptor_compute(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Requires descriptors computation

  ##### Positional Arguments
  - **self**: `Evision.LineDescriptor.BinaryDescriptor.t()`
  - **image**: `Evision.Mat`.

    input image

  ##### Keyword Arguments
  - **returnFloatDescr**: `bool`.

    flag (when set to true, original non-binary descriptors are returned)

  ##### Return
  - **keylines**: `[Evision.LineDescriptor.KeyLine]`.

    vector containing lines for which descriptors must be computed

  - **descriptors**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  compute(image, keylines[, descriptors[, returnFloatDescr]]) -> keylines, descriptors
  ```
  """
  @spec compute(Evision.LineDescriptor.BinaryDescriptor.t(), Evision.Mat.maybe_mat_in(), list(Evision.LineDescriptor.KeyLine.t())) :: {list(Evision.LineDescriptor.KeyLine.t()), Evision.Mat.t()} | {:error, String.t()}
  def compute(self, image, keylines) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and is_list(keylines)
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      keylines: Evision.Internal.Structurise.from_struct(keylines)
    ]
    :evision_nif.line_descriptor_line_descriptor_BinaryDescriptor_compute(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Create a BinaryDescriptor object with default parameters (or with the ones provided)
  and return a smart pointer to it

  ##### Return
  - **retval**: `Evision.LineDescriptor.BinaryDescriptor.t()`

  Python prototype (for reference only):
  ```python3
  createBinaryDescriptor() -> retval
  ```
  """
  @spec createBinaryDescriptor() :: Evision.LineDescriptor.BinaryDescriptor.t() | {:error, String.t()}
  def createBinaryDescriptor() do
    positional = [
    ]
    :evision_nif.line_descriptor_line_descriptor_BinaryDescriptor_createBinaryDescriptor_static(positional)
    |> to_struct()
  end

  @doc """
  Requires line detection

  ##### Positional Arguments
  - **self**: `Evision.LineDescriptor.BinaryDescriptor.t()`
  - **image**: `Evision.Mat`.

    input image

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.

    mask matrix to detect only KeyLines of interest

  ##### Return
  - **keypoints**: `[Evision.LineDescriptor.KeyLine]`.

    vector that will store extracted lines for one or more images

  Python prototype (for reference only):
  ```python3
  detect(image[, mask]) -> keypoints
  ```
  """
  @spec detect(Evision.LineDescriptor.BinaryDescriptor.t(), Evision.Mat.maybe_mat_in(), [{:mask, term()}] | nil) :: list(Evision.LineDescriptor.KeyLine.t()) | {:error, String.t()}
  def detect(self, image, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:mask])
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.line_descriptor_line_descriptor_BinaryDescriptor_detect(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Requires line detection

  ##### Positional Arguments
  - **self**: `Evision.LineDescriptor.BinaryDescriptor.t()`
  - **image**: `Evision.Mat`.

    input image

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.

    mask matrix to detect only KeyLines of interest

  ##### Return
  - **keypoints**: `[Evision.LineDescriptor.KeyLine]`.

    vector that will store extracted lines for one or more images

  Python prototype (for reference only):
  ```python3
  detect(image[, mask]) -> keypoints
  ```
  """
  @spec detect(Evision.LineDescriptor.BinaryDescriptor.t(), Evision.Mat.maybe_mat_in()) :: list(Evision.LineDescriptor.KeyLine.t()) | {:error, String.t()}
  def detect(self, image) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.line_descriptor_line_descriptor_BinaryDescriptor_detect(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.LineDescriptor.BinaryDescriptor.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.LineDescriptor.BinaryDescriptor.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.line_descriptor_BinaryDescriptor_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.LineDescriptor.BinaryDescriptor.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.LineDescriptor.BinaryDescriptor.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.line_descriptor_BinaryDescriptor_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Get current number of octaves

  ##### Positional Arguments
  - **self**: `Evision.LineDescriptor.BinaryDescriptor.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getNumOfOctaves() -> retval
  ```
  """
  @spec getNumOfOctaves(Evision.LineDescriptor.BinaryDescriptor.t()) :: integer() | {:error, String.t()}
  def getNumOfOctaves(self) do
    positional = [
    ]
    :evision_nif.line_descriptor_line_descriptor_BinaryDescriptor_getNumOfOctaves(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Get current reduction ratio (used in Gaussian pyramids)

  ##### Positional Arguments
  - **self**: `Evision.LineDescriptor.BinaryDescriptor.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getReductionRatio() -> retval
  ```
  """
  @spec getReductionRatio(Evision.LineDescriptor.BinaryDescriptor.t()) :: integer() | {:error, String.t()}
  def getReductionRatio(self) do
    positional = [
    ]
    :evision_nif.line_descriptor_line_descriptor_BinaryDescriptor_getReductionRatio(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Get current width of bands

  ##### Positional Arguments
  - **self**: `Evision.LineDescriptor.BinaryDescriptor.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getWidthOfBand() -> retval
  ```
  """
  @spec getWidthOfBand(Evision.LineDescriptor.BinaryDescriptor.t()) :: integer() | {:error, String.t()}
  def getWidthOfBand(self) do
    positional = [
    ]
    :evision_nif.line_descriptor_line_descriptor_BinaryDescriptor_getWidthOfBand(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.LineDescriptor.BinaryDescriptor.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.LineDescriptor.BinaryDescriptor.t(), Evision.FileNode.t()) :: Evision.LineDescriptor.BinaryDescriptor.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.line_descriptor_BinaryDescriptor_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.LineDescriptor.BinaryDescriptor.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.LineDescriptor.BinaryDescriptor.t(), binary()) :: Evision.LineDescriptor.BinaryDescriptor.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.line_descriptor_BinaryDescriptor_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Set number of octaves

  ##### Positional Arguments
  - **self**: `Evision.LineDescriptor.BinaryDescriptor.t()`
  - **octaves**: `integer()`.

    number of octaves

  Python prototype (for reference only):
  ```python3
  setNumOfOctaves(octaves) -> None
  ```
  """
  @spec setNumOfOctaves(Evision.LineDescriptor.BinaryDescriptor.t(), integer()) :: Evision.LineDescriptor.BinaryDescriptor.t() | {:error, String.t()}
  def setNumOfOctaves(self, octaves) when is_integer(octaves)
  do
    positional = [
      octaves: Evision.Internal.Structurise.from_struct(octaves)
    ]
    :evision_nif.line_descriptor_line_descriptor_BinaryDescriptor_setNumOfOctaves(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Set reduction ratio (used in Gaussian pyramids)

  ##### Positional Arguments
  - **self**: `Evision.LineDescriptor.BinaryDescriptor.t()`
  - **rRatio**: `integer()`.

    reduction ratio

  Python prototype (for reference only):
  ```python3
  setReductionRatio(rRatio) -> None
  ```
  """
  @spec setReductionRatio(Evision.LineDescriptor.BinaryDescriptor.t(), integer()) :: Evision.LineDescriptor.BinaryDescriptor.t() | {:error, String.t()}
  def setReductionRatio(self, rRatio) when is_integer(rRatio)
  do
    positional = [
      rRatio: Evision.Internal.Structurise.from_struct(rRatio)
    ]
    :evision_nif.line_descriptor_line_descriptor_BinaryDescriptor_setReductionRatio(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Set width of bands

  ##### Positional Arguments
  - **self**: `Evision.LineDescriptor.BinaryDescriptor.t()`
  - **width**: `integer()`.

    width of bands

  Python prototype (for reference only):
  ```python3
  setWidthOfBand(width) -> None
  ```
  """
  @spec setWidthOfBand(Evision.LineDescriptor.BinaryDescriptor.t(), integer()) :: Evision.LineDescriptor.BinaryDescriptor.t() | {:error, String.t()}
  def setWidthOfBand(self, width) when is_integer(width)
  do
    positional = [
      width: Evision.Internal.Structurise.from_struct(width)
    ]
    :evision_nif.line_descriptor_line_descriptor_BinaryDescriptor_setWidthOfBand(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.LineDescriptor.BinaryDescriptor.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.LineDescriptor.BinaryDescriptor.t(), Evision.FileStorage.t(), binary()) :: Evision.LineDescriptor.BinaryDescriptor.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.line_descriptor_BinaryDescriptor_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.LineDescriptor.BinaryDescriptor.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.LineDescriptor.BinaryDescriptor.t(), Evision.FileStorage.t()) :: Evision.LineDescriptor.BinaryDescriptor.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.line_descriptor_BinaryDescriptor_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
