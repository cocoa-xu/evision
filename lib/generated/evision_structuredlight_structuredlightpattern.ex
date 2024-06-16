defmodule Evision.StructuredLight.StructuredLightPattern do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `StructuredLight.StructuredLightPattern` struct.

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
  def to_struct({:ok, %{class: Evision.StructuredLight.StructuredLightPattern, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.StructuredLight.StructuredLightPattern, ref: ref}) do
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
  - **self**: `Evision.StructuredLight.StructuredLightPattern.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.StructuredLight.StructuredLightPattern.t()) :: Evision.StructuredLight.StructuredLightPattern.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.structured_light_StructuredLightPattern_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Decodes the structured light pattern, generating a disparity map

  ##### Positional Arguments
  - **self**: `Evision.StructuredLight.StructuredLightPattern.t()`
  - **patternImages**: `[[Evision.Mat]]`.

    The acquired pattern images to decode (vector<vector<Mat>>), loaded as grayscale and previously rectified.

  ##### Keyword Arguments
  - **blackImages**: `[Evision.Mat]`.

    The all-black images needed for shadowMasks computation.

  - **whiteImages**: `[Evision.Mat]`.

    The all-white images needed for shadowMasks computation.

  - **flags**: `integer()`.

    Flags setting decoding algorithms. Default: DECODE_3D_UNDERWORLD.

  ##### Return
  - **retval**: `bool`
  - **disparityMap**: `Evision.Mat.t()`.

    The decoding result: a CV_64F Mat at image resolution, storing the computed disparity map.

  **Note**: All the images must be at the same resolution.

  Python prototype (for reference only):
  ```python3
  decode(patternImages[, disparityMap[, blackImages[, whiteImages[, flags]]]]) -> retval, disparityMap
  ```
  """
  @spec decode(Evision.StructuredLight.StructuredLightPattern.t(), list(list(Evision.Mat.maybe_mat_in())), [{:blackImages, term()} | {:flags, term()} | {:whiteImages, term()}] | nil) :: Evision.Mat.t() | false | {:error, String.t()}
  def decode(self, patternImages, opts) when is_list(patternImages) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:blackImages, :flags, :whiteImages])
    positional = [
      patternImages: Evision.Internal.Structurise.from_struct(patternImages)
    ]
    :evision_nif.structured_light_structured_light_StructuredLightPattern_decode(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Decodes the structured light pattern, generating a disparity map

  ##### Positional Arguments
  - **self**: `Evision.StructuredLight.StructuredLightPattern.t()`
  - **patternImages**: `[[Evision.Mat]]`.

    The acquired pattern images to decode (vector<vector<Mat>>), loaded as grayscale and previously rectified.

  ##### Keyword Arguments
  - **blackImages**: `[Evision.Mat]`.

    The all-black images needed for shadowMasks computation.

  - **whiteImages**: `[Evision.Mat]`.

    The all-white images needed for shadowMasks computation.

  - **flags**: `integer()`.

    Flags setting decoding algorithms. Default: DECODE_3D_UNDERWORLD.

  ##### Return
  - **retval**: `bool`
  - **disparityMap**: `Evision.Mat.t()`.

    The decoding result: a CV_64F Mat at image resolution, storing the computed disparity map.

  **Note**: All the images must be at the same resolution.

  Python prototype (for reference only):
  ```python3
  decode(patternImages[, disparityMap[, blackImages[, whiteImages[, flags]]]]) -> retval, disparityMap
  ```
  """
  @spec decode(Evision.StructuredLight.StructuredLightPattern.t(), list(list(Evision.Mat.maybe_mat_in()))) :: Evision.Mat.t() | false | {:error, String.t()}
  def decode(self, patternImages) when is_list(patternImages)
  do
    positional = [
      patternImages: Evision.Internal.Structurise.from_struct(patternImages)
    ]
    :evision_nif.structured_light_structured_light_StructuredLightPattern_decode(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.StructuredLight.StructuredLightPattern.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.StructuredLight.StructuredLightPattern.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.structured_light_StructuredLightPattern_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Generates the structured light pattern to project.

  ##### Positional Arguments
  - **self**: `Evision.StructuredLight.StructuredLightPattern.t()`

  ##### Return
  - **retval**: `bool`
  - **patternImages**: `[Evision.Mat]`.

    The generated pattern: a vector<Mat>, in which each image is a CV_8U Mat at projector's resolution.

  Python prototype (for reference only):
  ```python3
  generate([, patternImages]) -> retval, patternImages
  ```
  """
  @spec generate(Evision.StructuredLight.StructuredLightPattern.t(), [{atom(), term()},...] | nil) :: list(Evision.Mat.t()) | false | {:error, String.t()}
  def generate(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    positional = [
    ]
    :evision_nif.structured_light_structured_light_StructuredLightPattern_generate(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Generates the structured light pattern to project.

  ##### Positional Arguments
  - **self**: `Evision.StructuredLight.StructuredLightPattern.t()`

  ##### Return
  - **retval**: `bool`
  - **patternImages**: `[Evision.Mat]`.

    The generated pattern: a vector<Mat>, in which each image is a CV_8U Mat at projector's resolution.

  Python prototype (for reference only):
  ```python3
  generate([, patternImages]) -> retval, patternImages
  ```
  """
  @spec generate(Evision.StructuredLight.StructuredLightPattern.t()) :: list(Evision.Mat.t()) | false | {:error, String.t()}
  def generate(self) do
    positional = [
    ]
    :evision_nif.structured_light_structured_light_StructuredLightPattern_generate(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.StructuredLight.StructuredLightPattern.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.StructuredLight.StructuredLightPattern.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.structured_light_StructuredLightPattern_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.StructuredLight.StructuredLightPattern.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.StructuredLight.StructuredLightPattern.t(), Evision.FileNode.t()) :: Evision.StructuredLight.StructuredLightPattern.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.structured_light_StructuredLightPattern_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.StructuredLight.StructuredLightPattern.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.StructuredLight.StructuredLightPattern.t(), binary()) :: Evision.StructuredLight.StructuredLightPattern.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.structured_light_StructuredLightPattern_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.StructuredLight.StructuredLightPattern.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.StructuredLight.StructuredLightPattern.t(), Evision.FileStorage.t(), binary()) :: Evision.StructuredLight.StructuredLightPattern.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.structured_light_StructuredLightPattern_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.StructuredLight.StructuredLightPattern.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.StructuredLight.StructuredLightPattern.t(), Evision.FileStorage.t()) :: Evision.StructuredLight.StructuredLightPattern.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.structured_light_StructuredLightPattern_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
