defmodule Evision.StructuredLight.GrayCodePattern do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `StructuredLight.GrayCodePattern` struct.

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
  def to_struct({:ok, %{class: Evision.StructuredLight.GrayCodePattern, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.StructuredLight.GrayCodePattern, ref: ref}) do
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
  Constructor

  ##### Positional Arguments
  - **width**: `integer()`
  - **height**: `integer()`

  ##### Return
  - **retval**: `Evision.StructuredLight.GrayCodePattern.t()`

  Python prototype (for reference only):
  ```python3
  create(width, height) -> retval
  ```
  """
  @spec create(integer(), integer()) :: Evision.StructuredLight.GrayCodePattern.t() | {:error, String.t()}
  def create(width, height) when is_integer(width) and is_integer(height)
  do
    positional = [
      width: Evision.Internal.Structurise.from_struct(width),
      height: Evision.Internal.Structurise.from_struct(height)
    ]
    :evision_nif.structured_light_structured_light_GrayCodePattern_create_static(positional)
    |> to_struct()
  end

  @doc """
  Generates the all-black and all-white images needed for shadowMasks computation.

  ##### Positional Arguments
  - **self**: `Evision.StructuredLight.GrayCodePattern.t()`

  ##### Return
  - **blackImage**: `Evision.Mat.t()`.

    The generated all-black CV_8U image, at projector's resolution.

  - **whiteImage**: `Evision.Mat.t()`.

    The generated all-white CV_8U image, at projector's resolution.

    To identify shadow regions, the regions of two images where the pixels are not lit by projector's light and thus where there is not coded information,
    the 3DUNDERWORLD algorithm computes a shadow mask for the two cameras views, starting from a white and a black images captured by each camera.
    This method generates these two additional images to project.

  Python prototype (for reference only):
  ```python3
  getImagesForShadowMasks(blackImage, whiteImage) -> blackImage, whiteImage
  ```
  """
  @spec getImagesForShadowMasks(Evision.StructuredLight.GrayCodePattern.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: {Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def getImagesForShadowMasks(self, blackImage, whiteImage) when (is_struct(blackImage, Evision.Mat) or is_struct(blackImage, Nx.Tensor) or is_number(blackImage) or is_tuple(blackImage)) and (is_struct(whiteImage, Evision.Mat) or is_struct(whiteImage, Nx.Tensor) or is_number(whiteImage) or is_tuple(whiteImage))
  do
    positional = [
      blackImage: Evision.Internal.Structurise.from_struct(blackImage),
      whiteImage: Evision.Internal.Structurise.from_struct(whiteImage)
    ]
    :evision_nif.structured_light_structured_light_GrayCodePattern_getImagesForShadowMasks(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Get the number of pattern images needed for the graycode pattern.

  ##### Positional Arguments
  - **self**: `Evision.StructuredLight.GrayCodePattern.t()`

  ##### Return
  - **retval**: `size_t`

  @return The number of pattern images needed for the graycode pattern.

  Python prototype (for reference only):
  ```python3
  getNumberOfPatternImages() -> retval
  ```
  """
  @spec getNumberOfPatternImages(Evision.StructuredLight.GrayCodePattern.t()) :: integer() | {:error, String.t()}
  def getNumberOfPatternImages(self) do
    positional = [
    ]
    :evision_nif.structured_light_structured_light_GrayCodePattern_getNumberOfPatternImages(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  For a (x,y) pixel of a camera returns the corresponding projector pixel.

  ##### Positional Arguments
  - **self**: `Evision.StructuredLight.GrayCodePattern.t()`
  - **patternImages**: `[Evision.Mat]`.

    The pattern images acquired by the camera, stored in a grayscale vector < Mat >.

  - **x**: `integer()`.

    x coordinate of the image pixel.

  - **y**: `integer()`.

    y coordinate of the image pixel.

  ##### Return
  - **retval**: `bool`
  - **projPix**: `Point`.

    Projector's pixel corresponding to the camera's pixel: projPix.x and projPix.y are the image coordinates of the projector's pixel corresponding to the pixel being decoded in a camera.

    The function decodes each pixel in the pattern images acquired by a camera into their corresponding decimal numbers representing the projector's column and row,
    providing a mapping between camera's and projector's pixel.

  Python prototype (for reference only):
  ```python3
  getProjPixel(patternImages, x, y) -> retval, projPix
  ```
  """
  @spec getProjPixel(Evision.StructuredLight.GrayCodePattern.t(), list(Evision.Mat.maybe_mat_in()), integer(), integer()) :: {number(), number()} | false | {:error, String.t()}
  def getProjPixel(self, patternImages, x, y) when is_list(patternImages) and is_integer(x) and is_integer(y)
  do
    positional = [
      patternImages: Evision.Internal.Structurise.from_struct(patternImages),
      x: Evision.Internal.Structurise.from_struct(x),
      y: Evision.Internal.Structurise.from_struct(y)
    ]
    :evision_nif.structured_light_structured_light_GrayCodePattern_getProjPixel(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Sets the value for black threshold, needed for decoding (shadowsmasks computation).

  ##### Positional Arguments
  - **self**: `Evision.StructuredLight.GrayCodePattern.t()`
  - **value**: `size_t`.

    The desired black threshold value.

    Black threshold is a number between 0-255 that represents the minimum brightness difference required for valid pixels, between the fully illuminated (white) and the not illuminated images (black); used in computeShadowMasks method.

  Python prototype (for reference only):
  ```python3
  setBlackThreshold(value) -> None
  ```
  """
  @spec setBlackThreshold(Evision.StructuredLight.GrayCodePattern.t(), integer()) :: Evision.StructuredLight.GrayCodePattern.t() | {:error, String.t()}
  def setBlackThreshold(self, value) when is_integer(value)
  do
    positional = [
      value: Evision.Internal.Structurise.from_struct(value)
    ]
    :evision_nif.structured_light_structured_light_GrayCodePattern_setBlackThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Sets the value for white threshold, needed for decoding.

  ##### Positional Arguments
  - **self**: `Evision.StructuredLight.GrayCodePattern.t()`
  - **value**: `size_t`.

    The desired white threshold value.

    White threshold is a number between 0-255 that represents the minimum brightness difference required for valid pixels, between the graycode pattern and its inverse images; used in getProjPixel method.

  Python prototype (for reference only):
  ```python3
  setWhiteThreshold(value) -> None
  ```
  """
  @spec setWhiteThreshold(Evision.StructuredLight.GrayCodePattern.t(), integer()) :: Evision.StructuredLight.GrayCodePattern.t() | {:error, String.t()}
  def setWhiteThreshold(self, value) when is_integer(value)
  do
    positional = [
      value: Evision.Internal.Structurise.from_struct(value)
    ]
    :evision_nif.structured_light_structured_light_GrayCodePattern_setWhiteThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
