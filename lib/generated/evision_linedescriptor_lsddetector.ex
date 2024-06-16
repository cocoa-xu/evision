defmodule Evision.LineDescriptor.LSDDetector do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `LineDescriptor.LSDDetector` struct.

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
  def to_struct({:ok, %{class: Evision.LineDescriptor.LSDDetector, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.LineDescriptor.LSDDetector, ref: ref}) do
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
  LSDDetectorWithParams

  ##### Positional Arguments
  - **params**: `Evision.LineDescriptor.LSDParam`

  ##### Return
  - **self**: `Evision.LineDescriptor.LSDDetector.t()`

  Python prototype (for reference only):
  ```python3
  LSDDetectorWithParams(_params) -> <line_descriptor_LSDDetector object>
  ```
  """
  @spec lSDDetector(Evision.LineDescriptor.LSDParam.t()) :: Evision.LineDescriptor.LSDDetector.t() | {:error, String.t()}
  def lSDDetector(params) when is_struct(params, Evision.LineDescriptor.LSDParam)
  do
    positional = [
      params: Evision.Internal.Structurise.from_struct(params)
    ]
    :evision_nif.line_descriptor_line_descriptor_LSDDetector_LSDDetectorWithParams(positional)
    |> to_struct()
  end

  @doc """
  Clears the algorithm state

  ##### Positional Arguments
  - **self**: `Evision.LineDescriptor.LSDDetector.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.LineDescriptor.LSDDetector.t()) :: Evision.LineDescriptor.LSDDetector.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.line_descriptor_LSDDetector_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Creates ad LSDDetector object, using smart pointers.
  ##### Return
  - **retval**: `Evision.LineDescriptor.LSDDetector.t()`

  Python prototype (for reference only):
  ```python3
  createLSDDetector() -> retval
  ```
  """
  @spec createLSDDetector() :: Evision.LineDescriptor.LSDDetector.t() | {:error, String.t()}
  def createLSDDetector() do
    positional = [
    ]
    :evision_nif.line_descriptor_line_descriptor_LSDDetector_createLSDDetector_static(positional)
    |> to_struct()
  end

  @doc """
  createLSDDetectorWithParams

  ##### Positional Arguments
  - **params**: `Evision.LineDescriptor.LSDParam`

  ##### Return
  - **retval**: `Evision.LineDescriptor.LSDDetector.t()`

  Python prototype (for reference only):
  ```python3
  createLSDDetectorWithParams(params) -> retval
  ```
  """
  @spec createLSDDetectorWithParams(Evision.LineDescriptor.LSDParam.t()) :: Evision.LineDescriptor.LSDDetector.t() | {:error, String.t()}
  def createLSDDetectorWithParams(params) when is_struct(params, Evision.LineDescriptor.LSDParam)
  do
    positional = [
      params: Evision.Internal.Structurise.from_struct(params)
    ]
    :evision_nif.line_descriptor_line_descriptor_LSDDetector_createLSDDetectorWithParams_static(positional)
    |> to_struct()
  end

  @doc """
  detect

  ##### Positional Arguments
  - **self**: `Evision.LineDescriptor.LSDDetector.t()`
  - **images**: `[Evision.Mat]`.

    input images

  - **keylines**: `[[Evision.LineDescriptor.KeyLine]]`.

    set of vectors that will store extracted lines for one or more images

  - **scale**: `integer()`.

    scale factor used in pyramids generation

  - **numOctaves**: `integer()`.

    number of octaves inside pyramid

  ##### Keyword Arguments
  - **masks**: `[Evision.Mat]`.

    vector of mask matrices to detect only KeyLines of interest from each input image

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  detect(images, keylines, scale, numOctaves[, masks]) -> None
  ```
  """
  @spec detect(Evision.LineDescriptor.LSDDetector.t(), list(Evision.Mat.maybe_mat_in()), list(list(Evision.LineDescriptor.KeyLine.t())), integer(), integer(), [{:masks, term()}] | nil) :: Evision.LineDescriptor.LSDDetector.t() | {:error, String.t()}
  def detect(self, images, keylines, scale, numOctaves, opts) when is_list(images) and is_list(keylines) and is_integer(scale) and is_integer(numOctaves) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:masks])
    positional = [
      images: Evision.Internal.Structurise.from_struct(images),
      keylines: Evision.Internal.Structurise.from_struct(keylines),
      scale: Evision.Internal.Structurise.from_struct(scale),
      numOctaves: Evision.Internal.Structurise.from_struct(numOctaves)
    ]
    :evision_nif.line_descriptor_line_descriptor_LSDDetector_detect(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  detect

  ##### Positional Arguments
  - **self**: `Evision.LineDescriptor.LSDDetector.t()`
  - **images**: `[Evision.Mat]`.

    input images

  - **keylines**: `[[Evision.LineDescriptor.KeyLine]]`.

    set of vectors that will store extracted lines for one or more images

  - **scale**: `integer()`.

    scale factor used in pyramids generation

  - **numOctaves**: `integer()`.

    number of octaves inside pyramid

  ##### Keyword Arguments
  - **masks**: `[Evision.Mat]`.

    vector of mask matrices to detect only KeyLines of interest from each input image

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  detect(images, keylines, scale, numOctaves[, masks]) -> None
  ```
  #### Variant 2:
  Detect lines inside an image.

  ##### Positional Arguments
  - **self**: `Evision.LineDescriptor.LSDDetector.t()`
  - **image**: `Evision.Mat`.

    input image

  - **scale**: `integer()`.

    scale factor used in pyramids generation

  - **numOctaves**: `integer()`.

    number of octaves inside pyramid

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.

    mask matrix to detect only KeyLines of interest

  ##### Return
  - **keypoints**: `[Evision.LineDescriptor.KeyLine]`.

    vector that will store extracted lines for one or more images

  Python prototype (for reference only):
  ```python3
  detect(image, scale, numOctaves[, mask]) -> keypoints
  ```

  """
  @spec detect(Evision.LineDescriptor.LSDDetector.t(), Evision.Mat.maybe_mat_in(), integer(), integer(), [{:mask, term()}] | nil) :: list(Evision.LineDescriptor.KeyLine.t()) | {:error, String.t()}
  def detect(self, image, scale, numOctaves, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and is_integer(scale) and is_integer(numOctaves) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:mask])
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      scale: Evision.Internal.Structurise.from_struct(scale),
      numOctaves: Evision.Internal.Structurise.from_struct(numOctaves)
    ]
    :evision_nif.line_descriptor_line_descriptor_LSDDetector_detect(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec detect(Evision.LineDescriptor.LSDDetector.t(), list(Evision.Mat.maybe_mat_in()), list(list(Evision.LineDescriptor.KeyLine.t())), integer(), integer()) :: Evision.LineDescriptor.LSDDetector.t() | {:error, String.t()}
  def detect(self, images, keylines, scale, numOctaves) when is_list(images) and is_list(keylines) and is_integer(scale) and is_integer(numOctaves)
  do
    positional = [
      images: Evision.Internal.Structurise.from_struct(images),
      keylines: Evision.Internal.Structurise.from_struct(keylines),
      scale: Evision.Internal.Structurise.from_struct(scale),
      numOctaves: Evision.Internal.Structurise.from_struct(numOctaves)
    ]
    :evision_nif.line_descriptor_line_descriptor_LSDDetector_detect(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Detect lines inside an image.

  ##### Positional Arguments
  - **self**: `Evision.LineDescriptor.LSDDetector.t()`
  - **image**: `Evision.Mat`.

    input image

  - **scale**: `integer()`.

    scale factor used in pyramids generation

  - **numOctaves**: `integer()`.

    number of octaves inside pyramid

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.

    mask matrix to detect only KeyLines of interest

  ##### Return
  - **keypoints**: `[Evision.LineDescriptor.KeyLine]`.

    vector that will store extracted lines for one or more images

  Python prototype (for reference only):
  ```python3
  detect(image, scale, numOctaves[, mask]) -> keypoints
  ```
  """
  @spec detect(Evision.LineDescriptor.LSDDetector.t(), Evision.Mat.maybe_mat_in(), integer(), integer()) :: list(Evision.LineDescriptor.KeyLine.t()) | {:error, String.t()}
  def detect(self, image, scale, numOctaves) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and is_integer(scale) and is_integer(numOctaves)
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      scale: Evision.Internal.Structurise.from_struct(scale),
      numOctaves: Evision.Internal.Structurise.from_struct(numOctaves)
    ]
    :evision_nif.line_descriptor_line_descriptor_LSDDetector_detect(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.LineDescriptor.LSDDetector.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.LineDescriptor.LSDDetector.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.line_descriptor_LSDDetector_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.LineDescriptor.LSDDetector.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.LineDescriptor.LSDDetector.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.line_descriptor_LSDDetector_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.LineDescriptor.LSDDetector.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.LineDescriptor.LSDDetector.t(), Evision.FileNode.t()) :: Evision.LineDescriptor.LSDDetector.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.line_descriptor_LSDDetector_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.LineDescriptor.LSDDetector.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.LineDescriptor.LSDDetector.t(), binary()) :: Evision.LineDescriptor.LSDDetector.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.line_descriptor_LSDDetector_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.LineDescriptor.LSDDetector.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.LineDescriptor.LSDDetector.t(), Evision.FileStorage.t(), binary()) :: Evision.LineDescriptor.LSDDetector.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.line_descriptor_LSDDetector_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.LineDescriptor.LSDDetector.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.LineDescriptor.LSDDetector.t(), Evision.FileStorage.t()) :: Evision.LineDescriptor.LSDDetector.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.line_descriptor_LSDDetector_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
