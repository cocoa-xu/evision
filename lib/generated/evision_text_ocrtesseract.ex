defmodule Evision.Text.OCRTesseract do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Text.OCRTesseract` struct.

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
  def to_struct({:ok, %{class: Evision.Text.OCRTesseract, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Text.OCRTesseract, ref: ref}) do
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
  Creates an instance of the OCRTesseract class. Initializes Tesseract.
  ##### Keyword Arguments
  - **datapath**: `c_string`.

    the name of the parent directory of tessdata ended with "/", or NULL to use the
    system's default directory.

  - **language**: `c_string`.

    an ISO 639-3 code or NULL will default to "eng".

  - **char_whitelist**: `c_string`.

    specifies the list of characters used for recognition. NULL defaults to ""
    (All characters will be used for recognition).

  - **oem**: `integer()`.

    tesseract-ocr offers different OCR Engine Modes (OEM), by default
    tesseract::OEM_DEFAULT is used. See the tesseract-ocr API documentation for other possible
    values.

  - **psmode**: `integer()`.

    tesseract-ocr offers different Page Segmentation Modes (PSM) tesseract::PSM_AUTO
    (fully automatic layout analysis) is used. See the tesseract-ocr API documentation for other
    possible values.

  ##### Return
  - **retval**: `OCRTesseract`

  **Note**: The char_whitelist default is changed after OpenCV 4.7.0/3.19.0 from "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" to "".

  Python prototype (for reference only):
  ```python3
  create([, datapath[, language[, char_whitelist[, oem[, psmode]]]]]) -> retval
  ```
  """
  @spec create([{:char_whitelist, term()} | {:datapath, term()} | {:language, term()} | {:oem, term()} | {:psmode, term()}] | nil) :: Evision.Text.OCRTesseract.t() | {:error, String.t()}
  def create(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:char_whitelist, :datapath, :language, :oem, :psmode])
    positional = [
    ]
    :evision_nif.text_text_OCRTesseract_create_static(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Creates an instance of the OCRTesseract class. Initializes Tesseract.
  ##### Keyword Arguments
  - **datapath**: `c_string`.

    the name of the parent directory of tessdata ended with "/", or NULL to use the
    system's default directory.

  - **language**: `c_string`.

    an ISO 639-3 code or NULL will default to "eng".

  - **char_whitelist**: `c_string`.

    specifies the list of characters used for recognition. NULL defaults to ""
    (All characters will be used for recognition).

  - **oem**: `integer()`.

    tesseract-ocr offers different OCR Engine Modes (OEM), by default
    tesseract::OEM_DEFAULT is used. See the tesseract-ocr API documentation for other possible
    values.

  - **psmode**: `integer()`.

    tesseract-ocr offers different Page Segmentation Modes (PSM) tesseract::PSM_AUTO
    (fully automatic layout analysis) is used. See the tesseract-ocr API documentation for other
    possible values.

  ##### Return
  - **retval**: `OCRTesseract`

  **Note**: The char_whitelist default is changed after OpenCV 4.7.0/3.19.0 from "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" to "".

  Python prototype (for reference only):
  ```python3
  create([, datapath[, language[, char_whitelist[, oem[, psmode]]]]]) -> retval
  ```
  """
  @spec create() :: Evision.Text.OCRTesseract.t() | {:error, String.t()}
  def create() do
    positional = [
    ]
    :evision_nif.text_text_OCRTesseract_create_static(positional)
    |> to_struct()
  end

  @doc """
  run

  ##### Positional Arguments
  - **self**: `Evision.Text.OCRTesseract.t()`
  - **image**: `Evision.Mat`
  - **mask**: `Evision.Mat`
  - **min_confidence**: `integer()`

  ##### Keyword Arguments
  - **component_level**: `integer()`.

  ##### Return
  - **retval**: `String`

  Python prototype (for reference only):
  ```python3
  run(image, mask, min_confidence[, component_level]) -> retval
  ```
  """
  @spec run(Evision.Text.OCRTesseract.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), integer(), [{:component_level, term()}] | nil) :: binary() | {:error, String.t()}
  def run(self, image, mask, min_confidence, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (is_struct(mask, Evision.Mat) or is_struct(mask, Nx.Tensor) or is_number(mask) or is_tuple(mask)) and is_integer(min_confidence) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:component_level])
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      mask: Evision.Internal.Structurise.from_struct(mask),
      min_confidence: Evision.Internal.Structurise.from_struct(min_confidence)
    ]
    :evision_nif.text_text_OCRTesseract_run(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  run

  ##### Positional Arguments
  - **self**: `Evision.Text.OCRTesseract.t()`
  - **image**: `Evision.Mat`
  - **mask**: `Evision.Mat`
  - **min_confidence**: `integer()`

  ##### Keyword Arguments
  - **component_level**: `integer()`.

  ##### Return
  - **retval**: `String`

  Python prototype (for reference only):
  ```python3
  run(image, mask, min_confidence[, component_level]) -> retval
  ```
  #### Variant 2:
  Recognize text using the tesseract-ocr API.

  ##### Positional Arguments
  - **self**: `Evision.Text.OCRTesseract.t()`
  - **image**: `Evision.Mat`.

    Input image CV_8UC1 or CV_8UC3

  - **min_confidence**: `integer()`

  ##### Keyword Arguments
  - **component_level**: `integer()`.

    OCR_LEVEL_WORD (by default), or OCR_LEVEL_TEXTLINE.

  ##### Return
  - **retval**: `String`

  Takes image on input and returns recognized text in the output_text parameter. Optionally
  provides also the Rects for individual text elements found (e.g. words), and the list of those
  text elements with their confidence values.

  Python prototype (for reference only):
  ```python3
  run(image, min_confidence[, component_level]) -> retval
  ```

  """
  @spec run(Evision.Text.OCRTesseract.t(), Evision.Mat.maybe_mat_in(), integer(), [{:component_level, term()}] | nil) :: binary() | {:error, String.t()}
  def run(self, image, min_confidence, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and is_integer(min_confidence) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:component_level])
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      min_confidence: Evision.Internal.Structurise.from_struct(min_confidence)
    ]
    :evision_nif.text_text_OCRTesseract_run(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec run(Evision.Text.OCRTesseract.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), integer()) :: binary() | {:error, String.t()}
  def run(self, image, mask, min_confidence) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (is_struct(mask, Evision.Mat) or is_struct(mask, Nx.Tensor) or is_number(mask) or is_tuple(mask)) and is_integer(min_confidence)
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      mask: Evision.Internal.Structurise.from_struct(mask),
      min_confidence: Evision.Internal.Structurise.from_struct(min_confidence)
    ]
    :evision_nif.text_text_OCRTesseract_run(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Recognize text using the tesseract-ocr API.

  ##### Positional Arguments
  - **self**: `Evision.Text.OCRTesseract.t()`
  - **image**: `Evision.Mat`.

    Input image CV_8UC1 or CV_8UC3

  - **min_confidence**: `integer()`

  ##### Keyword Arguments
  - **component_level**: `integer()`.

    OCR_LEVEL_WORD (by default), or OCR_LEVEL_TEXTLINE.

  ##### Return
  - **retval**: `String`

  Takes image on input and returns recognized text in the output_text parameter. Optionally
  provides also the Rects for individual text elements found (e.g. words), and the list of those
  text elements with their confidence values.

  Python prototype (for reference only):
  ```python3
  run(image, min_confidence[, component_level]) -> retval
  ```
  """
  @spec run(Evision.Text.OCRTesseract.t(), Evision.Mat.maybe_mat_in(), integer()) :: binary() | {:error, String.t()}
  def run(self, image, min_confidence) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and is_integer(min_confidence)
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      min_confidence: Evision.Internal.Structurise.from_struct(min_confidence)
    ]
    :evision_nif.text_text_OCRTesseract_run(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setWhiteList

  ##### Positional Arguments
  - **self**: `Evision.Text.OCRTesseract.t()`
  - **char_whitelist**: `String`

  Python prototype (for reference only):
  ```python3
  setWhiteList(char_whitelist) -> None
  ```
  """
  @spec setWhiteList(Evision.Text.OCRTesseract.t(), binary()) :: Evision.Text.OCRTesseract.t() | {:error, String.t()}
  def setWhiteList(self, char_whitelist) when is_binary(char_whitelist)
  do
    positional = [
      char_whitelist: Evision.Internal.Structurise.from_struct(char_whitelist)
    ]
    :evision_nif.text_text_OCRTesseract_setWhiteList(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
