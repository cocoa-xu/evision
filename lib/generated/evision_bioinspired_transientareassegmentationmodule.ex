defmodule Evision.Bioinspired.TransientAreasSegmentationModule do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Bioinspired.TransientAreasSegmentationModule` struct.

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
  def to_struct({:ok, %{class: Evision.Bioinspired.TransientAreasSegmentationModule, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Bioinspired.TransientAreasSegmentationModule, ref: ref}) do
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
  - **self**: `Evision.Bioinspired.TransientAreasSegmentationModule.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.Bioinspired.TransientAreasSegmentationModule.t()) :: Evision.Bioinspired.TransientAreasSegmentationModule.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.bioinspired_TransientAreasSegmentationModule_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  cleans all the buffers of the instance

  ##### Positional Arguments
  - **self**: `Evision.Bioinspired.TransientAreasSegmentationModule.t()`

  Python prototype (for reference only):
  ```python3
  clearAllBuffers() -> None
  ```
  """
  @spec clearAllBuffers(Evision.Bioinspired.TransientAreasSegmentationModule.t()) :: Evision.Bioinspired.TransientAreasSegmentationModule.t() | {:error, String.t()}
  def clearAllBuffers(self) do
    positional = [
    ]
    :evision_nif.bioinspired_bioinspired_TransientAreasSegmentationModule_clearAllBuffers(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  allocator

  ##### Positional Arguments
  - **inputSize**: `Size`.

    : size of the images input to segment (output will be the same size)

  ##### Return
  - **retval**: `TransientAreasSegmentationModule`

  Python prototype (for reference only):
  ```python3
  create(inputSize) -> retval
  ```
  """
  @spec create({number(), number()}) :: Evision.Bioinspired.TransientAreasSegmentationModule.t() | {:error, String.t()}
  def create(inputSize) when is_tuple(inputSize)
  do
    positional = [
      inputSize: Evision.Internal.Structurise.from_struct(inputSize)
    ]
    :evision_nif.bioinspired_bioinspired_TransientAreasSegmentationModule_create_static(positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.Bioinspired.TransientAreasSegmentationModule.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.Bioinspired.TransientAreasSegmentationModule.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.bioinspired_TransientAreasSegmentationModule_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.Bioinspired.TransientAreasSegmentationModule.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.Bioinspired.TransientAreasSegmentationModule.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.bioinspired_TransientAreasSegmentationModule_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  access function
  return the last segmentation result: a boolean picture which is resampled between 0 and 255 for a display purpose

  ##### Positional Arguments
  - **self**: `Evision.Bioinspired.TransientAreasSegmentationModule.t()`

  ##### Return
  - **transientAreas**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  getSegmentationPicture([, transientAreas]) -> transientAreas
  ```
  """
  @spec getSegmentationPicture(Evision.Bioinspired.TransientAreasSegmentationModule.t(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def getSegmentationPicture(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    positional = [
    ]
    :evision_nif.bioinspired_bioinspired_TransientAreasSegmentationModule_getSegmentationPicture(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  access function
  return the last segmentation result: a boolean picture which is resampled between 0 and 255 for a display purpose

  ##### Positional Arguments
  - **self**: `Evision.Bioinspired.TransientAreasSegmentationModule.t()`

  ##### Return
  - **transientAreas**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  getSegmentationPicture([, transientAreas]) -> transientAreas
  ```
  """
  @spec getSegmentationPicture(Evision.Bioinspired.TransientAreasSegmentationModule.t()) :: Evision.Mat.t() | {:error, String.t()}
  def getSegmentationPicture(self) do
    positional = [
    ]
    :evision_nif.bioinspired_bioinspired_TransientAreasSegmentationModule_getSegmentationPicture(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  return the sze of the manage input and output images

  ##### Positional Arguments
  - **self**: `Evision.Bioinspired.TransientAreasSegmentationModule.t()`

  ##### Return
  - **retval**: `Size`

  Python prototype (for reference only):
  ```python3
  getSize() -> retval
  ```
  """
  @spec getSize(Evision.Bioinspired.TransientAreasSegmentationModule.t()) :: {number(), number()} | {:error, String.t()}
  def getSize(self) do
    positional = [
    ]
    :evision_nif.bioinspired_bioinspired_TransientAreasSegmentationModule_getSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  parameters setup display method

  ##### Positional Arguments
  - **self**: `Evision.Bioinspired.TransientAreasSegmentationModule.t()`

  ##### Return
  - **retval**: `String`

  @return a string which contains formatted parameters information

  Python prototype (for reference only):
  ```python3
  printSetup() -> retval
  ```
  """
  @spec printSetup(Evision.Bioinspired.TransientAreasSegmentationModule.t()) :: binary() | {:error, String.t()}
  def printSetup(self) do
    positional = [
    ]
    :evision_nif.bioinspired_bioinspired_TransientAreasSegmentationModule_printSetup(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.Bioinspired.TransientAreasSegmentationModule.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.Bioinspired.TransientAreasSegmentationModule.t(), Evision.FileNode.t()) :: Evision.Bioinspired.TransientAreasSegmentationModule.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.bioinspired_TransientAreasSegmentationModule_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  main processing method, get result using methods getSegmentationPicture()

  ##### Positional Arguments
  - **self**: `Evision.Bioinspired.TransientAreasSegmentationModule.t()`
  - **inputToSegment**: `Evision.Mat`.

    : the image to process, it must match the instance buffer size !

  ##### Keyword Arguments
  - **channelIndex**: `integer()`.

    : the channel to process in case of multichannel images

  Python prototype (for reference only):
  ```python3
  run(inputToSegment[, channelIndex]) -> None
  ```
  """
  @spec run(Evision.Bioinspired.TransientAreasSegmentationModule.t(), Evision.Mat.maybe_mat_in(), [{:channelIndex, term()}] | nil) :: Evision.Bioinspired.TransientAreasSegmentationModule.t() | {:error, String.t()}
  def run(self, inputToSegment, opts) when (is_struct(inputToSegment, Evision.Mat) or is_struct(inputToSegment, Nx.Tensor) or is_number(inputToSegment) or is_tuple(inputToSegment)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:channelIndex])
    positional = [
      inputToSegment: Evision.Internal.Structurise.from_struct(inputToSegment)
    ]
    :evision_nif.bioinspired_bioinspired_TransientAreasSegmentationModule_run(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  main processing method, get result using methods getSegmentationPicture()

  ##### Positional Arguments
  - **self**: `Evision.Bioinspired.TransientAreasSegmentationModule.t()`
  - **inputToSegment**: `Evision.Mat`.

    : the image to process, it must match the instance buffer size !

  ##### Keyword Arguments
  - **channelIndex**: `integer()`.

    : the channel to process in case of multichannel images

  Python prototype (for reference only):
  ```python3
  run(inputToSegment[, channelIndex]) -> None
  ```
  """
  @spec run(Evision.Bioinspired.TransientAreasSegmentationModule.t(), Evision.Mat.maybe_mat_in()) :: Evision.Bioinspired.TransientAreasSegmentationModule.t() | {:error, String.t()}
  def run(self, inputToSegment) when (is_struct(inputToSegment, Evision.Mat) or is_struct(inputToSegment, Nx.Tensor) or is_number(inputToSegment) or is_tuple(inputToSegment))
  do
    positional = [
      inputToSegment: Evision.Internal.Structurise.from_struct(inputToSegment)
    ]
    :evision_nif.bioinspired_bioinspired_TransientAreasSegmentationModule_run(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.Bioinspired.TransientAreasSegmentationModule.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.Bioinspired.TransientAreasSegmentationModule.t(), binary()) :: Evision.Bioinspired.TransientAreasSegmentationModule.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.bioinspired_TransientAreasSegmentationModule_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  try to open an XML segmentation parameters file to adjust current segmentation instance setup

  ##### Positional Arguments
  - **self**: `Evision.Bioinspired.TransientAreasSegmentationModule.t()`

  ##### Keyword Arguments
  - **segmentationParameterFile**: `String`.

    : the parameters filename

  - **applyDefaultSetupOnFailure**: `bool`.

    : set to true if an error must be thrown on error

  - if the xml file does not exist, then default setup is applied
  - warning, Exceptions are thrown if read XML file is not valid

  Python prototype (for reference only):
  ```python3
  setup([, segmentationParameterFile[, applyDefaultSetupOnFailure]]) -> None
  ```
  """
  @spec setup(Evision.Bioinspired.TransientAreasSegmentationModule.t(), [{:applyDefaultSetupOnFailure, term()} | {:segmentationParameterFile, term()}] | nil) :: Evision.Bioinspired.TransientAreasSegmentationModule.t() | {:error, String.t()}
  def setup(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:applyDefaultSetupOnFailure, :segmentationParameterFile])
    positional = [
    ]
    :evision_nif.bioinspired_bioinspired_TransientAreasSegmentationModule_setup(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  try to open an XML segmentation parameters file to adjust current segmentation instance setup

  ##### Positional Arguments
  - **self**: `Evision.Bioinspired.TransientAreasSegmentationModule.t()`

  ##### Keyword Arguments
  - **segmentationParameterFile**: `String`.

    : the parameters filename

  - **applyDefaultSetupOnFailure**: `bool`.

    : set to true if an error must be thrown on error

  - if the xml file does not exist, then default setup is applied
  - warning, Exceptions are thrown if read XML file is not valid

  Python prototype (for reference only):
  ```python3
  setup([, segmentationParameterFile[, applyDefaultSetupOnFailure]]) -> None
  ```
  """
  @spec setup(Evision.Bioinspired.TransientAreasSegmentationModule.t()) :: Evision.Bioinspired.TransientAreasSegmentationModule.t() | {:error, String.t()}
  def setup(self) do
    positional = [
    ]
    :evision_nif.bioinspired_bioinspired_TransientAreasSegmentationModule_setup(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write xml/yml formated parameters information

  ##### Positional Arguments
  - **self**: `Evision.Bioinspired.TransientAreasSegmentationModule.t()`
  - **fs**: `String`.

    : the filename of the xml file that will be open and writen with formatted parameters information

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.Bioinspired.TransientAreasSegmentationModule.t(), binary()) :: Evision.Bioinspired.TransientAreasSegmentationModule.t() | {:error, String.t()}
  def write(self, fs) when is_binary(fs)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.bioinspired_bioinspired_TransientAreasSegmentationModule_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
