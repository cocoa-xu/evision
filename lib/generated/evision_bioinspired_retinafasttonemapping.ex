defmodule Evision.Bioinspired.RetinaFastToneMapping do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Bioinspired.RetinaFastToneMapping` struct.

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
  def to_struct({:ok, %{class: Evision.Bioinspired.RetinaFastToneMapping, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Bioinspired.RetinaFastToneMapping, ref: ref}) do
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
  applies a luminance correction (initially High Dynamic Range (HDR) tone mapping)

  ##### Positional Arguments
  - **self**: `Evision.Bioinspired.RetinaFastToneMapping.t()`
  - **inputImage**: `Evision.Mat`.

    the input image to process RGB or gray levels

  ##### Return
  - **outputToneMappedImage**: `Evision.Mat.t()`.

    the output tone mapped image

  using only the 2 local adaptation stages of the retina parvocellular channel : photoreceptors
  level and ganlion cells level. Spatio temporal filtering is applied but limited to temporal
  smoothing and eventually high frequencies attenuation. This is a lighter method than the one
  available using the regular retina::run method. It is then faster but it does not include
  complete temporal filtering nor retina spectral whitening. Then, it can have a more limited
  effect on images with a very high dynamic range. This is an adptation of the original still
  image HDR tone mapping algorithm of David Alleyson, Sabine Susstruck and Laurence Meylan's
  work, please cite: -> Meylan L., Alleysson D., and Susstrunk S., A Model of Retinal Local
  Adaptation for the Tone Mapping of Color Filter Array Images, Journal of Optical Society of
  America, A, Vol. 24, N 9, September, 1st, 2007, pp. 2807-2816

  Python prototype (for reference only):
  ```python3
  applyFastToneMapping(inputImage[, outputToneMappedImage]) -> outputToneMappedImage
  ```
  """
  @spec applyFastToneMapping(Evision.Bioinspired.RetinaFastToneMapping.t(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def applyFastToneMapping(self, inputImage, opts) when (is_struct(inputImage, Evision.Mat) or is_struct(inputImage, Nx.Tensor) or is_number(inputImage) or is_tuple(inputImage)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      inputImage: Evision.Internal.Structurise.from_struct(inputImage)
    ]
    :evision_nif.bioinspired_bioinspired_RetinaFastToneMapping_applyFastToneMapping(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  applies a luminance correction (initially High Dynamic Range (HDR) tone mapping)

  ##### Positional Arguments
  - **self**: `Evision.Bioinspired.RetinaFastToneMapping.t()`
  - **inputImage**: `Evision.Mat`.

    the input image to process RGB or gray levels

  ##### Return
  - **outputToneMappedImage**: `Evision.Mat.t()`.

    the output tone mapped image

  using only the 2 local adaptation stages of the retina parvocellular channel : photoreceptors
  level and ganlion cells level. Spatio temporal filtering is applied but limited to temporal
  smoothing and eventually high frequencies attenuation. This is a lighter method than the one
  available using the regular retina::run method. It is then faster but it does not include
  complete temporal filtering nor retina spectral whitening. Then, it can have a more limited
  effect on images with a very high dynamic range. This is an adptation of the original still
  image HDR tone mapping algorithm of David Alleyson, Sabine Susstruck and Laurence Meylan's
  work, please cite: -> Meylan L., Alleysson D., and Susstrunk S., A Model of Retinal Local
  Adaptation for the Tone Mapping of Color Filter Array Images, Journal of Optical Society of
  America, A, Vol. 24, N 9, September, 1st, 2007, pp. 2807-2816

  Python prototype (for reference only):
  ```python3
  applyFastToneMapping(inputImage[, outputToneMappedImage]) -> outputToneMappedImage
  ```
  """
  @spec applyFastToneMapping(Evision.Bioinspired.RetinaFastToneMapping.t(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def applyFastToneMapping(self, inputImage) when (is_struct(inputImage, Evision.Mat) or is_struct(inputImage, Nx.Tensor) or is_number(inputImage) or is_tuple(inputImage))
  do
    positional = [
      inputImage: Evision.Internal.Structurise.from_struct(inputImage)
    ]
    :evision_nif.bioinspired_bioinspired_RetinaFastToneMapping_applyFastToneMapping(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Clears the algorithm state

  ##### Positional Arguments
  - **self**: `Evision.Bioinspired.RetinaFastToneMapping.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.Bioinspired.RetinaFastToneMapping.t()) :: Evision.Bioinspired.RetinaFastToneMapping.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.bioinspired_RetinaFastToneMapping_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  create

  ##### Positional Arguments
  - **inputSize**: `Size`

  ##### Return
  - **retval**: `RetinaFastToneMapping`

  Python prototype (for reference only):
  ```python3
  create(inputSize) -> retval
  ```
  """
  @spec create({number(), number()}) :: Evision.Bioinspired.RetinaFastToneMapping.t() | {:error, String.t()}
  def create(inputSize) when is_tuple(inputSize)
  do
    positional = [
      inputSize: Evision.Internal.Structurise.from_struct(inputSize)
    ]
    :evision_nif.bioinspired_bioinspired_RetinaFastToneMapping_create_static(positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.Bioinspired.RetinaFastToneMapping.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.Bioinspired.RetinaFastToneMapping.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.bioinspired_RetinaFastToneMapping_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.Bioinspired.RetinaFastToneMapping.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.Bioinspired.RetinaFastToneMapping.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.bioinspired_RetinaFastToneMapping_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.Bioinspired.RetinaFastToneMapping.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.Bioinspired.RetinaFastToneMapping.t(), Evision.FileNode.t()) :: Evision.Bioinspired.RetinaFastToneMapping.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.bioinspired_RetinaFastToneMapping_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.Bioinspired.RetinaFastToneMapping.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.Bioinspired.RetinaFastToneMapping.t(), binary()) :: Evision.Bioinspired.RetinaFastToneMapping.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.bioinspired_RetinaFastToneMapping_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  updates tone mapping behaviors by adjusing the local luminance computation area

  ##### Positional Arguments
  - **self**: `Evision.Bioinspired.RetinaFastToneMapping.t()`

  ##### Keyword Arguments
  - **photoreceptorsNeighborhoodRadius**: `float`.

    the first stage local adaptation area

  - **ganglioncellsNeighborhoodRadius**: `float`.

    the second stage local adaptation area

  - **meanLuminanceModulatorK**: `float`.

    the factor applied to modulate the meanLuminance information
    (default is 1, see reference paper)

  Python prototype (for reference only):
  ```python3
  setup([, photoreceptorsNeighborhoodRadius[, ganglioncellsNeighborhoodRadius[, meanLuminanceModulatorK]]]) -> None
  ```
  """
  @spec setup(Evision.Bioinspired.RetinaFastToneMapping.t(), [{:ganglioncellsNeighborhoodRadius, term()} | {:meanLuminanceModulatorK, term()} | {:photoreceptorsNeighborhoodRadius, term()}] | nil) :: Evision.Bioinspired.RetinaFastToneMapping.t() | {:error, String.t()}
  def setup(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:ganglioncellsNeighborhoodRadius, :meanLuminanceModulatorK, :photoreceptorsNeighborhoodRadius])
    positional = [
    ]
    :evision_nif.bioinspired_bioinspired_RetinaFastToneMapping_setup(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  updates tone mapping behaviors by adjusing the local luminance computation area

  ##### Positional Arguments
  - **self**: `Evision.Bioinspired.RetinaFastToneMapping.t()`

  ##### Keyword Arguments
  - **photoreceptorsNeighborhoodRadius**: `float`.

    the first stage local adaptation area

  - **ganglioncellsNeighborhoodRadius**: `float`.

    the second stage local adaptation area

  - **meanLuminanceModulatorK**: `float`.

    the factor applied to modulate the meanLuminance information
    (default is 1, see reference paper)

  Python prototype (for reference only):
  ```python3
  setup([, photoreceptorsNeighborhoodRadius[, ganglioncellsNeighborhoodRadius[, meanLuminanceModulatorK]]]) -> None
  ```
  """
  @spec setup(Evision.Bioinspired.RetinaFastToneMapping.t()) :: Evision.Bioinspired.RetinaFastToneMapping.t() | {:error, String.t()}
  def setup(self) do
    positional = [
    ]
    :evision_nif.bioinspired_bioinspired_RetinaFastToneMapping_setup(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.Bioinspired.RetinaFastToneMapping.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.Bioinspired.RetinaFastToneMapping.t(), Evision.FileStorage.t(), binary()) :: Evision.Bioinspired.RetinaFastToneMapping.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.bioinspired_RetinaFastToneMapping_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.Bioinspired.RetinaFastToneMapping.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.Bioinspired.RetinaFastToneMapping.t(), Evision.FileStorage.t()) :: Evision.Bioinspired.RetinaFastToneMapping.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.bioinspired_RetinaFastToneMapping_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
