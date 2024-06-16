defmodule Evision.BgSegm.SyntheticSequenceGenerator do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `BgSegm.SyntheticSequenceGenerator` struct.

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
  def to_struct({:ok, %{class: Evision.BgSegm.SyntheticSequenceGenerator, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.BgSegm.SyntheticSequenceGenerator, ref: ref}) do
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
  Creates an instance of SyntheticSequenceGenerator.

  ##### Positional Arguments
  - **background**: `Evision.Mat`.

    Background image for object.

  - **object**: `Evision.Mat`.

    Object image which will move slowly over the background.

  - **amplitude**: `double`.

    Amplitude of wave distortion applied to background.

  - **wavelength**: `double`.

    Length of waves in distortion applied to background.

  - **wavespeed**: `double`.

    How fast waves will move.

  - **objspeed**: `double`.

    How fast object will fly over background.

  ##### Return
  - **self**: `Evision.BgSegm.SyntheticSequenceGenerator.t()`

  Python prototype (for reference only):
  ```python3
  SyntheticSequenceGenerator(background, object, amplitude, wavelength, wavespeed, objspeed) -> <bgsegm_SyntheticSequenceGenerator object>
  ```
  """
  @spec syntheticSequenceGenerator(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), number(), number(), number(), number()) :: Evision.BgSegm.SyntheticSequenceGenerator.t() | {:error, String.t()}
  def syntheticSequenceGenerator(background, object, amplitude, wavelength, wavespeed, objspeed) when (is_struct(background, Evision.Mat) or is_struct(background, Nx.Tensor) or is_number(background) or is_tuple(background)) and (is_struct(object, Evision.Mat) or is_struct(object, Nx.Tensor) or is_number(object) or is_tuple(object)) and is_number(amplitude) and is_number(wavelength) and is_number(wavespeed) and is_number(objspeed)
  do
    positional = [
      background: Evision.Internal.Structurise.from_struct(background),
      object: Evision.Internal.Structurise.from_struct(object),
      amplitude: Evision.Internal.Structurise.from_struct(amplitude),
      wavelength: Evision.Internal.Structurise.from_struct(wavelength),
      wavespeed: Evision.Internal.Structurise.from_struct(wavespeed),
      objspeed: Evision.Internal.Structurise.from_struct(objspeed)
    ]
    :evision_nif.bgsegm_bgsegm_SyntheticSequenceGenerator_SyntheticSequenceGenerator(positional)
    |> to_struct()
  end

  @doc """
  Clears the algorithm state

  ##### Positional Arguments
  - **self**: `Evision.BgSegm.SyntheticSequenceGenerator.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.BgSegm.SyntheticSequenceGenerator.t()) :: Evision.BgSegm.SyntheticSequenceGenerator.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.bgsegm_SyntheticSequenceGenerator_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.BgSegm.SyntheticSequenceGenerator.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.BgSegm.SyntheticSequenceGenerator.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.bgsegm_SyntheticSequenceGenerator_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.BgSegm.SyntheticSequenceGenerator.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.BgSegm.SyntheticSequenceGenerator.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.bgsegm_SyntheticSequenceGenerator_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Obtain the next frame in the sequence.

  ##### Positional Arguments
  - **self**: `Evision.BgSegm.SyntheticSequenceGenerator.t()`

  ##### Return
  - **frame**: `Evision.Mat.t()`.

    Output frame.

  - **gtMask**: `Evision.Mat.t()`.

    Output ground-truth (reference) segmentation mask object/background.

  Python prototype (for reference only):
  ```python3
  getNextFrame([, frame[, gtMask]]) -> frame, gtMask
  ```
  """
  @spec getNextFrame(Evision.BgSegm.SyntheticSequenceGenerator.t(), [{atom(), term()},...] | nil) :: {Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def getNextFrame(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    positional = [
    ]
    :evision_nif.bgsegm_bgsegm_SyntheticSequenceGenerator_getNextFrame(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Obtain the next frame in the sequence.

  ##### Positional Arguments
  - **self**: `Evision.BgSegm.SyntheticSequenceGenerator.t()`

  ##### Return
  - **frame**: `Evision.Mat.t()`.

    Output frame.

  - **gtMask**: `Evision.Mat.t()`.

    Output ground-truth (reference) segmentation mask object/background.

  Python prototype (for reference only):
  ```python3
  getNextFrame([, frame[, gtMask]]) -> frame, gtMask
  ```
  """
  @spec getNextFrame(Evision.BgSegm.SyntheticSequenceGenerator.t()) :: {Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def getNextFrame(self) do
    positional = [
    ]
    :evision_nif.bgsegm_bgsegm_SyntheticSequenceGenerator_getNextFrame(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.BgSegm.SyntheticSequenceGenerator.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.BgSegm.SyntheticSequenceGenerator.t(), Evision.FileNode.t()) :: Evision.BgSegm.SyntheticSequenceGenerator.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.bgsegm_SyntheticSequenceGenerator_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.BgSegm.SyntheticSequenceGenerator.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.BgSegm.SyntheticSequenceGenerator.t(), binary()) :: Evision.BgSegm.SyntheticSequenceGenerator.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.bgsegm_SyntheticSequenceGenerator_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.BgSegm.SyntheticSequenceGenerator.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.BgSegm.SyntheticSequenceGenerator.t(), Evision.FileStorage.t(), binary()) :: Evision.BgSegm.SyntheticSequenceGenerator.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.bgsegm_SyntheticSequenceGenerator_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.BgSegm.SyntheticSequenceGenerator.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.BgSegm.SyntheticSequenceGenerator.t(), Evision.FileStorage.t()) :: Evision.BgSegm.SyntheticSequenceGenerator.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.bgsegm_SyntheticSequenceGenerator_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
