defmodule Evision.Bioinspired.Retina do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Bioinspired.Retina` struct.

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
  def to_struct({:ok, %{class: Evision.Bioinspired.Retina, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Bioinspired.Retina, ref: ref}) do
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
  Activate/desactivate the Parvocellular pathway processing (contours information extraction), by
  default, it is activated

  ##### Positional Arguments
  - **self**: `Evision.Bioinspired.Retina.t()`
  - **activate**: `bool`.

    true if Parvocellular (contours information extraction) output should be
    activated, false if not... if activated, the Parvocellular output can be retrieved using the
    Retina::getParvo methods

  Python prototype (for reference only):
  ```python3
  activateContoursProcessing(activate) -> None
  ```
  """
  @spec activateContoursProcessing(Evision.Bioinspired.Retina.t(), boolean()) :: Evision.Bioinspired.Retina.t() | {:error, String.t()}
  def activateContoursProcessing(self, activate) when is_boolean(activate)
  do
    positional = [
      activate: Evision.Internal.Structurise.from_struct(activate)
    ]
    :evision_nif.bioinspired_bioinspired_Retina_activateContoursProcessing(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Activate/desactivate the Magnocellular pathway processing (motion information extraction), by
  default, it is activated

  ##### Positional Arguments
  - **self**: `Evision.Bioinspired.Retina.t()`
  - **activate**: `bool`.

    true if Magnocellular output should be activated, false if not... if activated,
    the Magnocellular output can be retrieved using the **getMagno** methods

  Python prototype (for reference only):
  ```python3
  activateMovingContoursProcessing(activate) -> None
  ```
  """
  @spec activateMovingContoursProcessing(Evision.Bioinspired.Retina.t(), boolean()) :: Evision.Bioinspired.Retina.t() | {:error, String.t()}
  def activateMovingContoursProcessing(self, activate) when is_boolean(activate)
  do
    positional = [
      activate: Evision.Internal.Structurise.from_struct(activate)
    ]
    :evision_nif.bioinspired_bioinspired_Retina_activateMovingContoursProcessing(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Method which processes an image in the aim to correct its luminance correct
  backlight problems, enhance details in shadows.

  ##### Positional Arguments
  - **self**: `Evision.Bioinspired.Retina.t()`
  - **inputImage**: `Evision.Mat`.

    the input image to process (should be coded in float format : CV_32F,
    CV_32FC1, CV_32F_C3, CV_32F_C4, the 4th channel won't be considered).

  ##### Return
  - **outputToneMappedImage**: `Evision.Mat.t()`.

    the output 8bit/channel tone mapped image (CV_8U or CV_8UC3 format).

  This method is designed to perform High Dynamic Range image tone mapping (compress \\>8bit/pixel
  images to 8bit/pixel). This is a simplified version of the Retina Parvocellular model
  (simplified version of the run/getParvo methods call) since it does not include the
  spatio-temporal filter modelling the Outer Plexiform Layer of the retina that performs spectral
  whitening and many other stuff. However, it works great for tone mapping and in a faster way.
  Check the demos and experiments section to see examples and the way to perform tone mapping
  using the original retina model and the method.

  Python prototype (for reference only):
  ```python3
  applyFastToneMapping(inputImage[, outputToneMappedImage]) -> outputToneMappedImage
  ```
  """
  @spec applyFastToneMapping(Evision.Bioinspired.Retina.t(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def applyFastToneMapping(self, inputImage, opts) when (is_struct(inputImage, Evision.Mat) or is_struct(inputImage, Nx.Tensor) or is_number(inputImage) or is_tuple(inputImage)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      inputImage: Evision.Internal.Structurise.from_struct(inputImage)
    ]
    :evision_nif.bioinspired_bioinspired_Retina_applyFastToneMapping(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Method which processes an image in the aim to correct its luminance correct
  backlight problems, enhance details in shadows.

  ##### Positional Arguments
  - **self**: `Evision.Bioinspired.Retina.t()`
  - **inputImage**: `Evision.Mat`.

    the input image to process (should be coded in float format : CV_32F,
    CV_32FC1, CV_32F_C3, CV_32F_C4, the 4th channel won't be considered).

  ##### Return
  - **outputToneMappedImage**: `Evision.Mat.t()`.

    the output 8bit/channel tone mapped image (CV_8U or CV_8UC3 format).

  This method is designed to perform High Dynamic Range image tone mapping (compress \\>8bit/pixel
  images to 8bit/pixel). This is a simplified version of the Retina Parvocellular model
  (simplified version of the run/getParvo methods call) since it does not include the
  spatio-temporal filter modelling the Outer Plexiform Layer of the retina that performs spectral
  whitening and many other stuff. However, it works great for tone mapping and in a faster way.
  Check the demos and experiments section to see examples and the way to perform tone mapping
  using the original retina model and the method.

  Python prototype (for reference only):
  ```python3
  applyFastToneMapping(inputImage[, outputToneMappedImage]) -> outputToneMappedImage
  ```
  """
  @spec applyFastToneMapping(Evision.Bioinspired.Retina.t(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def applyFastToneMapping(self, inputImage) when (is_struct(inputImage, Evision.Mat) or is_struct(inputImage, Nx.Tensor) or is_number(inputImage) or is_tuple(inputImage))
  do
    positional = [
      inputImage: Evision.Internal.Structurise.from_struct(inputImage)
    ]
    :evision_nif.bioinspired_bioinspired_Retina_applyFastToneMapping(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Clears the algorithm state

  ##### Positional Arguments
  - **self**: `Evision.Bioinspired.Retina.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.Bioinspired.Retina.t()) :: Evision.Bioinspired.Retina.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.bioinspired_Retina_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Clears all retina buffers

  ##### Positional Arguments
  - **self**: `Evision.Bioinspired.Retina.t()`

  (equivalent to opening the eyes after a long period of eye close ;o) whatchout the temporal
  transition occuring just after this method call.

  Python prototype (for reference only):
  ```python3
  clearBuffers() -> None
  ```
  """
  @spec clearBuffers(Evision.Bioinspired.Retina.t()) :: Evision.Bioinspired.Retina.t() | {:error, String.t()}
  def clearBuffers(self) do
    positional = [
    ]
    :evision_nif.bioinspired_bioinspired_Retina_clearBuffers(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Constructors from standardized interfaces : retreive a smart pointer to a Retina instance

  ##### Positional Arguments
  - **inputSize**: `Size`.

    the input frame size

  - **colorMode**: `bool`.

    the chosen processing mode : with or without color processing

  ##### Keyword Arguments
  - **colorSamplingMethod**: `integer()`.

    specifies which kind of color sampling will be used :
    - cv::bioinspired::RETINA_COLOR_RANDOM: each pixel position is either R, G or B in a random choice
    - cv::bioinspired::RETINA_COLOR_DIAGONAL: color sampling is RGBRGBRGB..., line 2 BRGBRGBRG..., line 3, GBRGBRGBR...
    - cv::bioinspired::RETINA_COLOR_BAYER: standard bayer sampling

  - **useRetinaLogSampling**: `bool`.

    activate retina log sampling, if true, the 2 following parameters can
    be used

  - **reductionFactor**: `float`.

    only usefull if param useRetinaLogSampling=true, specifies the reduction
    factor of the output frame (as the center (fovea) is high resolution and corners can be
    underscaled, then a reduction of the output is allowed without precision leak

  - **samplingStrength**: `float`.

    only usefull if param useRetinaLogSampling=true, specifies the strength of
    the log scale that is applied

  ##### Return
  - **retval**: `Retina`

  Python prototype (for reference only):
  ```python3
  create(inputSize, colorMode[, colorSamplingMethod[, useRetinaLogSampling[, reductionFactor[, samplingStrength]]]]) -> retval
  ```
  """
  @spec create({number(), number()}, boolean(), [{:colorSamplingMethod, term()} | {:reductionFactor, term()} | {:samplingStrength, term()} | {:useRetinaLogSampling, term()}] | nil) :: Evision.Bioinspired.Retina.t() | {:error, String.t()}
  def create(inputSize, colorMode, opts) when is_tuple(inputSize) and is_boolean(colorMode) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:colorSamplingMethod, :reductionFactor, :samplingStrength, :useRetinaLogSampling])
    positional = [
      inputSize: Evision.Internal.Structurise.from_struct(inputSize),
      colorMode: Evision.Internal.Structurise.from_struct(colorMode)
    ]
    :evision_nif.bioinspired_bioinspired_Retina_create_static(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Constructors from standardized interfaces : retreive a smart pointer to a Retina instance

  ##### Positional Arguments
  - **inputSize**: `Size`.

    the input frame size

  - **colorMode**: `bool`.

    the chosen processing mode : with or without color processing

  ##### Keyword Arguments
  - **colorSamplingMethod**: `integer()`.

    specifies which kind of color sampling will be used :
    - cv::bioinspired::RETINA_COLOR_RANDOM: each pixel position is either R, G or B in a random choice
    - cv::bioinspired::RETINA_COLOR_DIAGONAL: color sampling is RGBRGBRGB..., line 2 BRGBRGBRG..., line 3, GBRGBRGBR...
    - cv::bioinspired::RETINA_COLOR_BAYER: standard bayer sampling

  - **useRetinaLogSampling**: `bool`.

    activate retina log sampling, if true, the 2 following parameters can
    be used

  - **reductionFactor**: `float`.

    only usefull if param useRetinaLogSampling=true, specifies the reduction
    factor of the output frame (as the center (fovea) is high resolution and corners can be
    underscaled, then a reduction of the output is allowed without precision leak

  - **samplingStrength**: `float`.

    only usefull if param useRetinaLogSampling=true, specifies the strength of
    the log scale that is applied

  ##### Return
  - **retval**: `Retina`

  Python prototype (for reference only):
  ```python3
  create(inputSize, colorMode[, colorSamplingMethod[, useRetinaLogSampling[, reductionFactor[, samplingStrength]]]]) -> retval
  ```
  """
  @spec create({number(), number()}, boolean()) :: Evision.Bioinspired.Retina.t() | {:error, String.t()}
  def create(inputSize, colorMode) when is_tuple(inputSize) and is_boolean(colorMode)
  do
    positional = [
      inputSize: Evision.Internal.Structurise.from_struct(inputSize),
      colorMode: Evision.Internal.Structurise.from_struct(colorMode)
    ]
    :evision_nif.bioinspired_bioinspired_Retina_create_static(positional)
    |> to_struct()
  end

  @doc """
  create

  ##### Positional Arguments
  - **inputSize**: `Size`

  ##### Return
  - **retval**: `Retina`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  create(inputSize) -> retval
  ```
  """
  @spec create({number(), number()}) :: Evision.Bioinspired.Retina.t() | {:error, String.t()}
  def create(inputSize) when is_tuple(inputSize)
  do
    positional = [
      inputSize: Evision.Internal.Structurise.from_struct(inputSize)
    ]
    :evision_nif.bioinspired_bioinspired_Retina_create_static(positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.Bioinspired.Retina.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.Bioinspired.Retina.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.bioinspired_Retina_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.Bioinspired.Retina.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.Bioinspired.Retina.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.bioinspired_Retina_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Retreive retina input buffer size

  ##### Positional Arguments
  - **self**: `Evision.Bioinspired.Retina.t()`

  ##### Return
  - **retval**: `Size`

  @return the retina input buffer size

  Python prototype (for reference only):
  ```python3
  getInputSize() -> retval
  ```
  """
  @spec getInputSize(Evision.Bioinspired.Retina.t()) :: {number(), number()} | {:error, String.t()}
  def getInputSize(self) do
    positional = [
    ]
    :evision_nif.bioinspired_bioinspired_Retina_getInputSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Accessor of the motion channel of the retina (models peripheral vision).

  ##### Positional Arguments
  - **self**: `Evision.Bioinspired.Retina.t()`

  ##### Return
  - **retinaOutput_magno**: `Evision.Mat.t()`.

    the output buffer (reallocated if necessary), format can be :
    - a Mat, this output is rescaled for standard 8bits image processing use in OpenCV
    - RAW methods actually return a 1D matrix (encoding is M1, M2,... Mn), this output is the
      original retina filter model output, without any quantification or rescaling.

  Warning, getMagnoRAW methods return buffers that are not rescaled within range [0;255] while
  the non RAW method allows a normalized matrix to be retrieved.
  @see getMagnoRAW

  Python prototype (for reference only):
  ```python3
  getMagno([, retinaOutput_magno]) -> retinaOutput_magno
  ```
  """
  @spec getMagno(Evision.Bioinspired.Retina.t(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def getMagno(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    positional = [
    ]
    :evision_nif.bioinspired_bioinspired_Retina_getMagno(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Accessor of the motion channel of the retina (models peripheral vision).

  ##### Positional Arguments
  - **self**: `Evision.Bioinspired.Retina.t()`

  ##### Return
  - **retinaOutput_magno**: `Evision.Mat.t()`.

    the output buffer (reallocated if necessary), format can be :
    - a Mat, this output is rescaled for standard 8bits image processing use in OpenCV
    - RAW methods actually return a 1D matrix (encoding is M1, M2,... Mn), this output is the
      original retina filter model output, without any quantification or rescaling.

  Warning, getMagnoRAW methods return buffers that are not rescaled within range [0;255] while
  the non RAW method allows a normalized matrix to be retrieved.
  @see getMagnoRAW

  Python prototype (for reference only):
  ```python3
  getMagno([, retinaOutput_magno]) -> retinaOutput_magno
  ```
  """
  @spec getMagno(Evision.Bioinspired.Retina.t()) :: Evision.Mat.t() | {:error, String.t()}
  def getMagno(self) do
    positional = [
    ]
    :evision_nif.bioinspired_bioinspired_Retina_getMagno(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMagnoRAW

  ##### Positional Arguments
  - **self**: `Evision.Bioinspired.Retina.t()`

  ##### Return
  - **retval**: `Evision.Mat.t()`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  getMagnoRAW() -> retval
  ```
  """
  @spec getMagnoRAW(Evision.Bioinspired.Retina.t()) :: Evision.Mat.t() | {:error, String.t()}
  def getMagnoRAW(self) do
    positional = [
    ]
    :evision_nif.bioinspired_bioinspired_Retina_getMagnoRAW(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Retreive retina output buffer size that can be different from the input if a spatial log
  transformation is applied

  ##### Positional Arguments
  - **self**: `Evision.Bioinspired.Retina.t()`

  ##### Return
  - **retval**: `Size`

  @return the retina output buffer size

  Python prototype (for reference only):
  ```python3
  getOutputSize() -> retval
  ```
  """
  @spec getOutputSize(Evision.Bioinspired.Retina.t()) :: {number(), number()} | {:error, String.t()}
  def getOutputSize(self) do
    positional = [
    ]
    :evision_nif.bioinspired_bioinspired_Retina_getOutputSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Accessor of the details channel of the retina (models foveal vision).

  ##### Positional Arguments
  - **self**: `Evision.Bioinspired.Retina.t()`

  ##### Return
  - **retinaOutput_parvo**: `Evision.Mat.t()`.

    the output buffer (reallocated if necessary), format can be :
    - a Mat, this output is rescaled for standard 8bits image processing use in OpenCV
    - RAW methods actually return a 1D matrix (encoding is R1, R2, ... Rn, G1, G2, ..., Gn, B1,
      B2, ...Bn), this output is the original retina filter model output, without any
      quantification or rescaling.

  Warning, getParvoRAW methods return buffers that are not rescaled within range [0;255] while
  the non RAW method allows a normalized matrix to be retrieved.
  @see getParvoRAW

  Python prototype (for reference only):
  ```python3
  getParvo([, retinaOutput_parvo]) -> retinaOutput_parvo
  ```
  """
  @spec getParvo(Evision.Bioinspired.Retina.t(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def getParvo(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    positional = [
    ]
    :evision_nif.bioinspired_bioinspired_Retina_getParvo(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Accessor of the details channel of the retina (models foveal vision).

  ##### Positional Arguments
  - **self**: `Evision.Bioinspired.Retina.t()`

  ##### Return
  - **retinaOutput_parvo**: `Evision.Mat.t()`.

    the output buffer (reallocated if necessary), format can be :
    - a Mat, this output is rescaled for standard 8bits image processing use in OpenCV
    - RAW methods actually return a 1D matrix (encoding is R1, R2, ... Rn, G1, G2, ..., Gn, B1,
      B2, ...Bn), this output is the original retina filter model output, without any
      quantification or rescaling.

  Warning, getParvoRAW methods return buffers that are not rescaled within range [0;255] while
  the non RAW method allows a normalized matrix to be retrieved.
  @see getParvoRAW

  Python prototype (for reference only):
  ```python3
  getParvo([, retinaOutput_parvo]) -> retinaOutput_parvo
  ```
  """
  @spec getParvo(Evision.Bioinspired.Retina.t()) :: Evision.Mat.t() | {:error, String.t()}
  def getParvo(self) do
    positional = [
    ]
    :evision_nif.bioinspired_bioinspired_Retina_getParvo(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getParvoRAW

  ##### Positional Arguments
  - **self**: `Evision.Bioinspired.Retina.t()`

  ##### Return
  - **retval**: `Evision.Mat.t()`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  getParvoRAW() -> retval
  ```
  """
  @spec getParvoRAW(Evision.Bioinspired.Retina.t()) :: Evision.Mat.t() | {:error, String.t()}
  def getParvoRAW(self) do
    positional = [
    ]
    :evision_nif.bioinspired_bioinspired_Retina_getParvoRAW(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Outputs a string showing the used parameters setup

  ##### Positional Arguments
  - **self**: `Evision.Bioinspired.Retina.t()`

  ##### Return
  - **retval**: `String`

  @return a string which contains formated parameters information

  Python prototype (for reference only):
  ```python3
  printSetup() -> retval
  ```
  """
  @spec printSetup(Evision.Bioinspired.Retina.t()) :: binary() | {:error, String.t()}
  def printSetup(self) do
    positional = [
    ]
    :evision_nif.bioinspired_bioinspired_Retina_printSetup(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.Bioinspired.Retina.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.Bioinspired.Retina.t(), Evision.FileNode.t()) :: Evision.Bioinspired.Retina.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.bioinspired_Retina_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Method which allows retina to be applied on an input image,

  ##### Positional Arguments
  - **self**: `Evision.Bioinspired.Retina.t()`
  - **inputImage**: `Evision.Mat`.

    the input Mat image to be processed, can be gray level or BGR coded in any
    format (from 8bit to 16bits)

  after run, encapsulated retina module is ready to deliver its outputs using dedicated
  acccessors, see getParvo and getMagno methods

  Python prototype (for reference only):
  ```python3
  run(inputImage) -> None
  ```
  """
  @spec run(Evision.Bioinspired.Retina.t(), Evision.Mat.maybe_mat_in()) :: Evision.Bioinspired.Retina.t() | {:error, String.t()}
  def run(self, inputImage) when (is_struct(inputImage, Evision.Mat) or is_struct(inputImage, Nx.Tensor) or is_number(inputImage) or is_tuple(inputImage))
  do
    positional = [
      inputImage: Evision.Internal.Structurise.from_struct(inputImage)
    ]
    :evision_nif.bioinspired_bioinspired_Retina_run(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.Bioinspired.Retina.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.Bioinspired.Retina.t(), binary()) :: Evision.Bioinspired.Retina.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.bioinspired_Retina_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Activate color saturation as the final step of the color demultiplexing process -\\> this
  saturation is a sigmoide function applied to each channel of the demultiplexed image.

  ##### Positional Arguments
  - **self**: `Evision.Bioinspired.Retina.t()`

  ##### Keyword Arguments
  - **saturateColors**: `bool`.

    boolean that activates color saturation (if true) or desactivate (if false)

  - **colorSaturationValue**: `float`.

    the saturation factor : a simple factor applied on the chrominance
    buffers

  Python prototype (for reference only):
  ```python3
  setColorSaturation([, saturateColors[, colorSaturationValue]]) -> None
  ```
  """
  @spec setColorSaturation(Evision.Bioinspired.Retina.t(), [{:colorSaturationValue, term()} | {:saturateColors, term()}] | nil) :: Evision.Bioinspired.Retina.t() | {:error, String.t()}
  def setColorSaturation(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:colorSaturationValue, :saturateColors])
    positional = [
    ]
    :evision_nif.bioinspired_bioinspired_Retina_setColorSaturation(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Activate color saturation as the final step of the color demultiplexing process -\\> this
  saturation is a sigmoide function applied to each channel of the demultiplexed image.

  ##### Positional Arguments
  - **self**: `Evision.Bioinspired.Retina.t()`

  ##### Keyword Arguments
  - **saturateColors**: `bool`.

    boolean that activates color saturation (if true) or desactivate (if false)

  - **colorSaturationValue**: `float`.

    the saturation factor : a simple factor applied on the chrominance
    buffers

  Python prototype (for reference only):
  ```python3
  setColorSaturation([, saturateColors[, colorSaturationValue]]) -> None
  ```
  """
  @spec setColorSaturation(Evision.Bioinspired.Retina.t()) :: Evision.Bioinspired.Retina.t() | {:error, String.t()}
  def setColorSaturation(self) do
    positional = [
    ]
    :evision_nif.bioinspired_bioinspired_Retina_setColorSaturation(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Try to open an XML retina parameters file to adjust current retina instance setup

  ##### Positional Arguments
  - **self**: `Evision.Bioinspired.Retina.t()`

  ##### Keyword Arguments
  - **retinaParameterFile**: `String`.

    the parameters filename

  - **applyDefaultSetupOnFailure**: `bool`.

    set to true if an error must be thrown on error

  - if the xml file does not exist, then default setup is applied
  - warning, Exceptions are thrown if read XML file is not valid

  You can retrieve the current parameters structure using the method Retina::getParameters and update
  it before running method Retina::setup.

  Python prototype (for reference only):
  ```python3
  setup([, retinaParameterFile[, applyDefaultSetupOnFailure]]) -> None
  ```
  """
  @spec setup(Evision.Bioinspired.Retina.t(), [{:applyDefaultSetupOnFailure, term()} | {:retinaParameterFile, term()}] | nil) :: Evision.Bioinspired.Retina.t() | {:error, String.t()}
  def setup(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:applyDefaultSetupOnFailure, :retinaParameterFile])
    positional = [
    ]
    :evision_nif.bioinspired_bioinspired_Retina_setup(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Try to open an XML retina parameters file to adjust current retina instance setup

  ##### Positional Arguments
  - **self**: `Evision.Bioinspired.Retina.t()`

  ##### Keyword Arguments
  - **retinaParameterFile**: `String`.

    the parameters filename

  - **applyDefaultSetupOnFailure**: `bool`.

    set to true if an error must be thrown on error

  - if the xml file does not exist, then default setup is applied
  - warning, Exceptions are thrown if read XML file is not valid

  You can retrieve the current parameters structure using the method Retina::getParameters and update
  it before running method Retina::setup.

  Python prototype (for reference only):
  ```python3
  setup([, retinaParameterFile[, applyDefaultSetupOnFailure]]) -> None
  ```
  """
  @spec setup(Evision.Bioinspired.Retina.t()) :: Evision.Bioinspired.Retina.t() | {:error, String.t()}
  def setup(self) do
    positional = [
    ]
    :evision_nif.bioinspired_bioinspired_Retina_setup(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Set parameters values for the Inner Plexiform Layer (IPL) magnocellular channel

  ##### Positional Arguments
  - **self**: `Evision.Bioinspired.Retina.t()`

  ##### Keyword Arguments
  - **normaliseOutput**: `bool`.

    specifies if (true) output is rescaled between 0 and 255 of not (false)

  - **parasolCells_beta**: `float`.

    the low pass filter gain used for local contrast adaptation at the
    IPL level of the retina (for ganglion cells local adaptation), typical value is 0

  - **parasolCells_tau**: `float`.

    the low pass filter time constant used for local contrast adaptation
    at the IPL level of the retina (for ganglion cells local adaptation), unit is frame, typical
    value is 0 (immediate response)

  - **parasolCells_k**: `float`.

    the low pass filter spatial constant used for local contrast adaptation
    at the IPL level of the retina (for ganglion cells local adaptation), unit is pixels, typical
    value is 5

  - **amacrinCellsTemporalCutFrequency**: `float`.

    the time constant of the first order high pass fiter of
    the magnocellular way (motion information channel), unit is frames, typical value is 1.2

  - **v0CompressionParameter**: `float`.

    the compression strengh of the ganglion cells local adaptation
    output, set a value between 0.6 and 1 for best results, a high value increases more the low
    value sensitivity... and the output saturates faster, recommended value: 0.95

  - **localAdaptintegration_tau**: `float`.

    specifies the temporal constant of the low pas filter
    involved in the computation of the local "motion mean" for the local adaptation computation

  - **localAdaptintegration_k**: `float`.

    specifies the spatial constant of the low pas filter involved
    in the computation of the local "motion mean" for the local adaptation computation

  this channel processes signals output from OPL processing stage in peripheral vision, it allows
  motion information enhancement. It is decorrelated from the details channel. See reference
  papers for more details.

  Python prototype (for reference only):
  ```python3
  setupIPLMagnoChannel([, normaliseOutput[, parasolCells_beta[, parasolCells_tau[, parasolCells_k[, amacrinCellsTemporalCutFrequency[, V0CompressionParameter[, localAdaptintegration_tau[, localAdaptintegration_k]]]]]]]]) -> None
  ```
  """
  @spec setupIPLMagnoChannel(Evision.Bioinspired.Retina.t(), [{:amacrinCellsTemporalCutFrequency, term()} | {:localAdaptintegration_k, term()} | {:localAdaptintegration_tau, term()} | {:normaliseOutput, term()} | {:parasolCells_beta, term()} | {:parasolCells_k, term()} | {:parasolCells_tau, term()} | {:v0CompressionParameter, term()}] | nil) :: Evision.Bioinspired.Retina.t() | {:error, String.t()}
  def setupIPLMagnoChannel(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:amacrinCellsTemporalCutFrequency, :localAdaptintegration_k, :localAdaptintegration_tau, :normaliseOutput, :parasolCells_beta, :parasolCells_k, :parasolCells_tau, :v0CompressionParameter])
    positional = [
    ]
    :evision_nif.bioinspired_bioinspired_Retina_setupIPLMagnoChannel(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Set parameters values for the Inner Plexiform Layer (IPL) magnocellular channel

  ##### Positional Arguments
  - **self**: `Evision.Bioinspired.Retina.t()`

  ##### Keyword Arguments
  - **normaliseOutput**: `bool`.

    specifies if (true) output is rescaled between 0 and 255 of not (false)

  - **parasolCells_beta**: `float`.

    the low pass filter gain used for local contrast adaptation at the
    IPL level of the retina (for ganglion cells local adaptation), typical value is 0

  - **parasolCells_tau**: `float`.

    the low pass filter time constant used for local contrast adaptation
    at the IPL level of the retina (for ganglion cells local adaptation), unit is frame, typical
    value is 0 (immediate response)

  - **parasolCells_k**: `float`.

    the low pass filter spatial constant used for local contrast adaptation
    at the IPL level of the retina (for ganglion cells local adaptation), unit is pixels, typical
    value is 5

  - **amacrinCellsTemporalCutFrequency**: `float`.

    the time constant of the first order high pass fiter of
    the magnocellular way (motion information channel), unit is frames, typical value is 1.2

  - **v0CompressionParameter**: `float`.

    the compression strengh of the ganglion cells local adaptation
    output, set a value between 0.6 and 1 for best results, a high value increases more the low
    value sensitivity... and the output saturates faster, recommended value: 0.95

  - **localAdaptintegration_tau**: `float`.

    specifies the temporal constant of the low pas filter
    involved in the computation of the local "motion mean" for the local adaptation computation

  - **localAdaptintegration_k**: `float`.

    specifies the spatial constant of the low pas filter involved
    in the computation of the local "motion mean" for the local adaptation computation

  this channel processes signals output from OPL processing stage in peripheral vision, it allows
  motion information enhancement. It is decorrelated from the details channel. See reference
  papers for more details.

  Python prototype (for reference only):
  ```python3
  setupIPLMagnoChannel([, normaliseOutput[, parasolCells_beta[, parasolCells_tau[, parasolCells_k[, amacrinCellsTemporalCutFrequency[, V0CompressionParameter[, localAdaptintegration_tau[, localAdaptintegration_k]]]]]]]]) -> None
  ```
  """
  @spec setupIPLMagnoChannel(Evision.Bioinspired.Retina.t()) :: Evision.Bioinspired.Retina.t() | {:error, String.t()}
  def setupIPLMagnoChannel(self) do
    positional = [
    ]
    :evision_nif.bioinspired_bioinspired_Retina_setupIPLMagnoChannel(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Setup the OPL and IPL parvo channels (see biologocal model)

  ##### Positional Arguments
  - **self**: `Evision.Bioinspired.Retina.t()`

  ##### Keyword Arguments
  - **colorMode**: `bool`.

    specifies if (true) color is processed of not (false) to then processing gray
    level image

  - **normaliseOutput**: `bool`.

    specifies if (true) output is rescaled between 0 and 255 of not (false)

  - **photoreceptorsLocalAdaptationSensitivity**: `float`.

    the photoreceptors sensitivity renage is 0-1
    (more log compression effect when value increases)

  - **photoreceptorsTemporalConstant**: `float`.

    the time constant of the first order low pass filter of
    the photoreceptors, use it to cut high temporal frequencies (noise or fast motion), unit is
    frames, typical value is 1 frame

  - **photoreceptorsSpatialConstant**: `float`.

    the spatial constant of the first order low pass filter of
    the photoreceptors, use it to cut high spatial frequencies (noise or thick contours), unit is
    pixels, typical value is 1 pixel

  - **horizontalCellsGain**: `float`.

    gain of the horizontal cells network, if 0, then the mean value of
    the output is zero, if the parameter is near 1, then, the luminance is not filtered and is
    still reachable at the output, typicall value is 0

  - **hcellsTemporalConstant**: `float`.

    the time constant of the first order low pass filter of the
    horizontal cells, use it to cut low temporal frequencies (local luminance variations), unit is
    frames, typical value is 1 frame, as the photoreceptors

  - **hcellsSpatialConstant**: `float`.

    the spatial constant of the first order low pass filter of the
    horizontal cells, use it to cut low spatial frequencies (local luminance), unit is pixels,
    typical value is 5 pixel, this value is also used for local contrast computing when computing
    the local contrast adaptation at the ganglion cells level (Inner Plexiform Layer parvocellular
    channel model)

  - **ganglionCellsSensitivity**: `float`.

    the compression strengh of the ganglion cells local adaptation
    output, set a value between 0.6 and 1 for best results, a high value increases more the low
    value sensitivity... and the output saturates faster, recommended value: 0.7

  OPL is referred as Outer Plexiform Layer of the retina, it allows the spatio-temporal filtering
  which withens the spectrum and reduces spatio-temporal noise while attenuating global luminance
  (low frequency energy) IPL parvo is the OPL next processing stage, it refers to a part of the
  Inner Plexiform layer of the retina, it allows high contours sensitivity in foveal vision. See
  reference papers for more informations.
  for more informations, please have a look at the paper Benoit A., Caplier A., Durette B., Herault, J., "USING HUMAN VISUAL SYSTEM MODELING FOR BIO-INSPIRED LOW LEVEL IMAGE PROCESSING", Elsevier, Computer Vision and Image Understanding 114 (2010), pp. 758-773, DOI: http://dx.doi.org/10.1016/j.cviu.2010.01.011

  Python prototype (for reference only):
  ```python3
  setupOPLandIPLParvoChannel([, colorMode[, normaliseOutput[, photoreceptorsLocalAdaptationSensitivity[, photoreceptorsTemporalConstant[, photoreceptorsSpatialConstant[, horizontalCellsGain[, HcellsTemporalConstant[, HcellsSpatialConstant[, ganglionCellsSensitivity]]]]]]]]]) -> None
  ```
  """
  @spec setupOPLandIPLParvoChannel(Evision.Bioinspired.Retina.t(), [{:colorMode, term()} | {:ganglionCellsSensitivity, term()} | {:hcellsSpatialConstant, term()} | {:hcellsTemporalConstant, term()} | {:horizontalCellsGain, term()} | {:normaliseOutput, term()} | {:photoreceptorsLocalAdaptationSensitivity, term()} | {:photoreceptorsSpatialConstant, term()} | {:photoreceptorsTemporalConstant, term()}] | nil) :: Evision.Bioinspired.Retina.t() | {:error, String.t()}
  def setupOPLandIPLParvoChannel(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:colorMode, :ganglionCellsSensitivity, :hcellsSpatialConstant, :hcellsTemporalConstant, :horizontalCellsGain, :normaliseOutput, :photoreceptorsLocalAdaptationSensitivity, :photoreceptorsSpatialConstant, :photoreceptorsTemporalConstant])
    positional = [
    ]
    :evision_nif.bioinspired_bioinspired_Retina_setupOPLandIPLParvoChannel(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Setup the OPL and IPL parvo channels (see biologocal model)

  ##### Positional Arguments
  - **self**: `Evision.Bioinspired.Retina.t()`

  ##### Keyword Arguments
  - **colorMode**: `bool`.

    specifies if (true) color is processed of not (false) to then processing gray
    level image

  - **normaliseOutput**: `bool`.

    specifies if (true) output is rescaled between 0 and 255 of not (false)

  - **photoreceptorsLocalAdaptationSensitivity**: `float`.

    the photoreceptors sensitivity renage is 0-1
    (more log compression effect when value increases)

  - **photoreceptorsTemporalConstant**: `float`.

    the time constant of the first order low pass filter of
    the photoreceptors, use it to cut high temporal frequencies (noise or fast motion), unit is
    frames, typical value is 1 frame

  - **photoreceptorsSpatialConstant**: `float`.

    the spatial constant of the first order low pass filter of
    the photoreceptors, use it to cut high spatial frequencies (noise or thick contours), unit is
    pixels, typical value is 1 pixel

  - **horizontalCellsGain**: `float`.

    gain of the horizontal cells network, if 0, then the mean value of
    the output is zero, if the parameter is near 1, then, the luminance is not filtered and is
    still reachable at the output, typicall value is 0

  - **hcellsTemporalConstant**: `float`.

    the time constant of the first order low pass filter of the
    horizontal cells, use it to cut low temporal frequencies (local luminance variations), unit is
    frames, typical value is 1 frame, as the photoreceptors

  - **hcellsSpatialConstant**: `float`.

    the spatial constant of the first order low pass filter of the
    horizontal cells, use it to cut low spatial frequencies (local luminance), unit is pixels,
    typical value is 5 pixel, this value is also used for local contrast computing when computing
    the local contrast adaptation at the ganglion cells level (Inner Plexiform Layer parvocellular
    channel model)

  - **ganglionCellsSensitivity**: `float`.

    the compression strengh of the ganglion cells local adaptation
    output, set a value between 0.6 and 1 for best results, a high value increases more the low
    value sensitivity... and the output saturates faster, recommended value: 0.7

  OPL is referred as Outer Plexiform Layer of the retina, it allows the spatio-temporal filtering
  which withens the spectrum and reduces spatio-temporal noise while attenuating global luminance
  (low frequency energy) IPL parvo is the OPL next processing stage, it refers to a part of the
  Inner Plexiform layer of the retina, it allows high contours sensitivity in foveal vision. See
  reference papers for more informations.
  for more informations, please have a look at the paper Benoit A., Caplier A., Durette B., Herault, J., "USING HUMAN VISUAL SYSTEM MODELING FOR BIO-INSPIRED LOW LEVEL IMAGE PROCESSING", Elsevier, Computer Vision and Image Understanding 114 (2010), pp. 758-773, DOI: http://dx.doi.org/10.1016/j.cviu.2010.01.011

  Python prototype (for reference only):
  ```python3
  setupOPLandIPLParvoChannel([, colorMode[, normaliseOutput[, photoreceptorsLocalAdaptationSensitivity[, photoreceptorsTemporalConstant[, photoreceptorsSpatialConstant[, horizontalCellsGain[, HcellsTemporalConstant[, HcellsSpatialConstant[, ganglionCellsSensitivity]]]]]]]]]) -> None
  ```
  """
  @spec setupOPLandIPLParvoChannel(Evision.Bioinspired.Retina.t()) :: Evision.Bioinspired.Retina.t() | {:error, String.t()}
  def setupOPLandIPLParvoChannel(self) do
    positional = [
    ]
    :evision_nif.bioinspired_bioinspired_Retina_setupOPLandIPLParvoChannel(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Write xml/yml formated parameters information

  ##### Positional Arguments
  - **self**: `Evision.Bioinspired.Retina.t()`
  - **fs**: `String`.

    the filename of the xml file that will be open and writen with formatted parameters
    information

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.Bioinspired.Retina.t(), binary()) :: Evision.Bioinspired.Retina.t() | {:error, String.t()}
  def write(self, fs) when is_binary(fs)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.bioinspired_bioinspired_Retina_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
