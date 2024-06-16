defmodule Evision.Text do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Text` struct.

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
  def to_struct({:ok, %{class: Evision.Text, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Text, ref: ref}) do
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
  @type enum :: integer()
  @doc enum: true
  def cv_ERFILTER_NM_RGBLGrad, do: 0
  @doc enum: true
  def cv_ERFILTER_NM_IHSGrad, do: 1
  @doc enum: true
  def cv_OCR_LEVEL_WORD, do: 0
  @doc enum: true
  def cv_OCR_LEVEL_TEXTLINE, do: 1


  @doc """
  Compute the different channels to be processed independently in the N&M algorithm @cite Neumann12.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source image. Must be RGB CV_8UC3.

  ##### Keyword Arguments
  - **mode**: `integer()`.

    Mode of operation. Currently the only available options are:
    * ERFILTER_NM_RGBLGrad** (used by default) and **ERFILTER_NM_IHSGrad**.

  ##### Return
  - **channels**: `[Evision.Mat]`.

    Output vector\\<Mat\\> where computed channels are stored.

  In N&M algorithm, the combination of intensity (I), hue (H), saturation (S), and gradient magnitude
  channels (Grad) are used in order to obtain high localization recall. This implementation also
  provides an alternative combination of red (R), green (G), blue (B), lightness (L), and gradient
  magnitude (Grad).

  Python prototype (for reference only):
  ```python3
  computeNMChannels(_src[, _channels[, _mode]]) -> _channels
  ```
  """
  @spec computeNMChannels(Evision.Mat.maybe_mat_in(), [{:mode, term()}] | nil) :: list(Evision.Mat.t()) | {:error, String.t()}
  def computeNMChannels(src, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:mode])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.text_computeNMChannels(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Compute the different channels to be processed independently in the N&M algorithm @cite Neumann12.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source image. Must be RGB CV_8UC3.

  ##### Keyword Arguments
  - **mode**: `integer()`.

    Mode of operation. Currently the only available options are:
    * ERFILTER_NM_RGBLGrad** (used by default) and **ERFILTER_NM_IHSGrad**.

  ##### Return
  - **channels**: `[Evision.Mat]`.

    Output vector\\<Mat\\> where computed channels are stored.

  In N&M algorithm, the combination of intensity (I), hue (H), saturation (S), and gradient magnitude
  channels (Grad) are used in order to obtain high localization recall. This implementation also
  provides an alternative combination of red (R), green (G), blue (B), lightness (L), and gradient
  magnitude (Grad).

  Python prototype (for reference only):
  ```python3
  computeNMChannels(_src[, _channels[, _mode]]) -> _channels
  ```
  """
  @spec computeNMChannels(Evision.Mat.maybe_mat_in()) :: list(Evision.Mat.t()) | {:error, String.t()}
  def computeNMChannels(src) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.text_computeNMChannels(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Reads an Extremal Region Filter for the 1st stage classifier of N&M algorithm
  from the provided path e.g. /path/to/cpp/trained_classifierNM1.xml

  ##### Positional Arguments
  - **filename**: `String`

  ##### Keyword Arguments
  - **thresholdDelta**: `integer()`.
  - **minArea**: `float`.
  - **maxArea**: `float`.
  - **minProbability**: `float`.
  - **nonMaxSuppression**: `bool`.
  - **minProbabilityDiff**: `float`.

  ##### Return
  - **retval**: `Evision.Text.ERFilter.t()`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  createERFilterNM1(filename[, thresholdDelta[, minArea[, maxArea[, minProbability[, nonMaxSuppression[, minProbabilityDiff]]]]]]) -> retval
  ```
  #### Variant 2:
  Create an Extremal Region Filter for the 1st stage classifier of N&M algorithm @cite Neumann12.

  ##### Positional Arguments
  - **cb**: `Evision.Text.ERFilter.Callback`

  ##### Keyword Arguments
  - **thresholdDelta**: `integer()`.
  - **minArea**: `float`.
  - **maxArea**: `float`.
  - **minProbability**: `float`.
  - **nonMaxSuppression**: `bool`.
  - **minProbabilityDiff**: `float`.

  ##### Return
  - **retval**: `Evision.Text.ERFilter.t()`

  The component tree of the image is extracted by a threshold increased step by step from 0 to 255,
  incrementally computable descriptors (aspect_ratio, compactness, number of holes, and number of
  horizontal crossings) are computed for each ER and used as features for a classifier which estimates
  the class-conditional probability P(er|character). The value of P(er|character) is tracked using the
  inclusion relation of ER across all thresholds and only the ERs which correspond to local maximum of
  the probability P(er|character) are selected (if the local maximum of the probability is above a
  global limit pmin and the difference between local maximum and local minimum is greater than
  minProbabilityDiff).

  Python prototype (for reference only):
  ```python3
  createERFilterNM1(cb[, thresholdDelta[, minArea[, maxArea[, minProbability[, nonMaxSuppression[, minProbabilityDiff]]]]]]) -> retval
  ```

  """
  @spec createERFilterNM1(binary(), [{:maxArea, term()} | {:minArea, term()} | {:minProbability, term()} | {:minProbabilityDiff, term()} | {:nonMaxSuppression, term()} | {:thresholdDelta, term()}] | nil) :: Evision.Text.ERFilter.t() | {:error, String.t()}
  def createERFilterNM1(filename, opts) when is_binary(filename) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:maxArea, :minArea, :minProbability, :minProbabilityDiff, :nonMaxSuppression, :thresholdDelta])
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.text_createERFilterNM1(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec createERFilterNM1(term(), [{:maxArea, term()} | {:minArea, term()} | {:minProbability, term()} | {:minProbabilityDiff, term()} | {:nonMaxSuppression, term()} | {:thresholdDelta, term()}] | nil) :: Evision.Text.ERFilter.t() | {:error, String.t()}
  def createERFilterNM1(cb, opts) when is_struct(cb, Evision.Text.ERFilter.Callback) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:maxArea, :minArea, :minProbability, :minProbabilityDiff, :nonMaxSuppression, :thresholdDelta])
    positional = [
      cb: Evision.Internal.Structurise.from_struct(cb)
    ]
    :evision_nif.text_createERFilterNM1(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Reads an Extremal Region Filter for the 1st stage classifier of N&M algorithm
  from the provided path e.g. /path/to/cpp/trained_classifierNM1.xml

  ##### Positional Arguments
  - **filename**: `String`

  ##### Keyword Arguments
  - **thresholdDelta**: `integer()`.
  - **minArea**: `float`.
  - **maxArea**: `float`.
  - **minProbability**: `float`.
  - **nonMaxSuppression**: `bool`.
  - **minProbabilityDiff**: `float`.

  ##### Return
  - **retval**: `Evision.Text.ERFilter.t()`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  createERFilterNM1(filename[, thresholdDelta[, minArea[, maxArea[, minProbability[, nonMaxSuppression[, minProbabilityDiff]]]]]]) -> retval
  ```
  #### Variant 2:
  Create an Extremal Region Filter for the 1st stage classifier of N&M algorithm @cite Neumann12.

  ##### Positional Arguments
  - **cb**: `Evision.Text.ERFilter.Callback`

  ##### Keyword Arguments
  - **thresholdDelta**: `integer()`.
  - **minArea**: `float`.
  - **maxArea**: `float`.
  - **minProbability**: `float`.
  - **nonMaxSuppression**: `bool`.
  - **minProbabilityDiff**: `float`.

  ##### Return
  - **retval**: `Evision.Text.ERFilter.t()`

  The component tree of the image is extracted by a threshold increased step by step from 0 to 255,
  incrementally computable descriptors (aspect_ratio, compactness, number of holes, and number of
  horizontal crossings) are computed for each ER and used as features for a classifier which estimates
  the class-conditional probability P(er|character). The value of P(er|character) is tracked using the
  inclusion relation of ER across all thresholds and only the ERs which correspond to local maximum of
  the probability P(er|character) are selected (if the local maximum of the probability is above a
  global limit pmin and the difference between local maximum and local minimum is greater than
  minProbabilityDiff).

  Python prototype (for reference only):
  ```python3
  createERFilterNM1(cb[, thresholdDelta[, minArea[, maxArea[, minProbability[, nonMaxSuppression[, minProbabilityDiff]]]]]]) -> retval
  ```

  """
  @spec createERFilterNM1(binary()) :: Evision.Text.ERFilter.t() | {:error, String.t()}
  def createERFilterNM1(filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.text_createERFilterNM1(positional)
    |> to_struct()
  end
  @spec createERFilterNM1(term()) :: Evision.Text.ERFilter.t() | {:error, String.t()}
  def createERFilterNM1(cb) when is_struct(cb, Evision.Text.ERFilter.Callback)
  do
    positional = [
      cb: Evision.Internal.Structurise.from_struct(cb)
    ]
    :evision_nif.text_createERFilterNM1(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Reads an Extremal Region Filter for the 2nd stage classifier of N&M algorithm
  from the provided path e.g. /path/to/cpp/trained_classifierNM2.xml

  ##### Positional Arguments
  - **filename**: `String`

  ##### Keyword Arguments
  - **minProbability**: `float`.

  ##### Return
  - **retval**: `Evision.Text.ERFilter.t()`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  createERFilterNM2(filename[, minProbability]) -> retval
  ```
  #### Variant 2:
  Create an Extremal Region Filter for the 2nd stage classifier of N&M algorithm @cite Neumann12.

  ##### Positional Arguments
  - **cb**: `Evision.Text.ERFilter.Callback`

  ##### Keyword Arguments
  - **minProbability**: `float`.

  ##### Return
  - **retval**: `Evision.Text.ERFilter.t()`

  In the second stage, the ERs that passed the first stage are classified into character and
  non-character classes using more informative but also more computationally expensive features. The
  classifier uses all the features calculated in the first stage and the following additional
  features: hole area ratio, convex hull ratio, and number of outer inflexion points.

  Python prototype (for reference only):
  ```python3
  createERFilterNM2(cb[, minProbability]) -> retval
  ```

  """
  @spec createERFilterNM2(binary(), [{:minProbability, term()}] | nil) :: Evision.Text.ERFilter.t() | {:error, String.t()}
  def createERFilterNM2(filename, opts) when is_binary(filename) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:minProbability])
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.text_createERFilterNM2(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec createERFilterNM2(term(), [{:minProbability, term()}] | nil) :: Evision.Text.ERFilter.t() | {:error, String.t()}
  def createERFilterNM2(cb, opts) when is_struct(cb, Evision.Text.ERFilter.Callback) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:minProbability])
    positional = [
      cb: Evision.Internal.Structurise.from_struct(cb)
    ]
    :evision_nif.text_createERFilterNM2(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Reads an Extremal Region Filter for the 2nd stage classifier of N&M algorithm
  from the provided path e.g. /path/to/cpp/trained_classifierNM2.xml

  ##### Positional Arguments
  - **filename**: `String`

  ##### Keyword Arguments
  - **minProbability**: `float`.

  ##### Return
  - **retval**: `Evision.Text.ERFilter.t()`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  createERFilterNM2(filename[, minProbability]) -> retval
  ```
  #### Variant 2:
  Create an Extremal Region Filter for the 2nd stage classifier of N&M algorithm @cite Neumann12.

  ##### Positional Arguments
  - **cb**: `Evision.Text.ERFilter.Callback`

  ##### Keyword Arguments
  - **minProbability**: `float`.

  ##### Return
  - **retval**: `Evision.Text.ERFilter.t()`

  In the second stage, the ERs that passed the first stage are classified into character and
  non-character classes using more informative but also more computationally expensive features. The
  classifier uses all the features calculated in the first stage and the following additional
  features: hole area ratio, convex hull ratio, and number of outer inflexion points.

  Python prototype (for reference only):
  ```python3
  createERFilterNM2(cb[, minProbability]) -> retval
  ```

  """
  @spec createERFilterNM2(binary()) :: Evision.Text.ERFilter.t() | {:error, String.t()}
  def createERFilterNM2(filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.text_createERFilterNM2(positional)
    |> to_struct()
  end
  @spec createERFilterNM2(term()) :: Evision.Text.ERFilter.t() | {:error, String.t()}
  def createERFilterNM2(cb) when is_struct(cb, Evision.Text.ERFilter.Callback)
  do
    positional = [
      cb: Evision.Internal.Structurise.from_struct(cb)
    ]
    :evision_nif.text_createERFilterNM2(positional)
    |> to_struct()
  end

  @doc """
  Utility function to create a tailored language model transitions table from a given list of words (lexicon).

  ##### Positional Arguments
  - **vocabulary**: `String`.

    The language vocabulary (chars when ASCII English text).

  - **lexicon**: `[String]`.

    The list of words that are expected to be found in a particular image.

  ##### Return
  - **retval**: `Evision.Mat.t()`

   The function calculate frequency statistics of character pairs from the given lexicon and fills the output transition_probabilities_table with them. The transition_probabilities_table can be used as input in the OCRHMMDecoder::create() and OCRBeamSearchDecoder::create() methods.
  **Note**: 
  - (C++) An alternative would be to load the default generic language transition table provided in the text module samples folder (created from ispell 42869 english words list) :
    <https://github.com/opencv/opencv_contrib/blob/master/modules/text/samples/OCRHMM_transitions_table.xml>

  Python prototype (for reference only):
  ```python3
  createOCRHMMTransitionsTable(vocabulary, lexicon) -> retval
  ```
  """
  @spec createOCRHMMTransitionsTable(binary(), list(binary())) :: Evision.Mat.t() | {:error, String.t()}
  def createOCRHMMTransitionsTable(vocabulary, lexicon) when is_binary(vocabulary) and is_list(lexicon)
  do
    positional = [
      vocabulary: Evision.Internal.Structurise.from_struct(vocabulary),
      lexicon: Evision.Internal.Structurise.from_struct(lexicon)
    ]
    :evision_nif.text_createOCRHMMTransitionsTable(positional)
    |> to_struct()
  end

  @doc """
  Extracts text regions from image.

  ##### Positional Arguments
  - **image**: `Evision.Mat`.

    Source image where text blocks needs to be extracted from.  Should be CV_8UC3 (color).

  - **er_filter1**: `Evision.Text.ERFilter`.

    Extremal Region Filter for the 1st stage classifier of N&M algorithm @cite Neumann12

  - **er_filter2**: `Evision.Text.ERFilter`.

    Extremal Region Filter for the 2nd stage classifier of N&M algorithm @cite Neumann12

  ##### Keyword Arguments
  - **method**: `integer()`.

    Grouping method (see text::erGrouping_Modes). Can be one of ERGROUPING_ORIENTATION_HORIZ, ERGROUPING_ORIENTATION_ANY.

  - **filename**: `String`.

    The XML or YAML file with the classifier model (e.g. samples/trained_classifier_erGrouping.xml). Only to use when grouping method is ERGROUPING_ORIENTATION_ANY.

  - **minProbability**: `float`.

    The minimum probability for accepting a group. Only to use when grouping method is ERGROUPING_ORIENTATION_ANY.

  ##### Return
  - **groups_rects**: `[Rect]`.

    Output list of rectangle blocks with text

  Python prototype (for reference only):
  ```python3
  detectRegions(image, er_filter1, er_filter2[, method[, filename[, minProbability]]]) -> groups_rects
  ```
  """
  @spec detectRegions(Evision.Mat.maybe_mat_in(), Evision.Text.ERFilter.t(), Evision.Text.ERFilter.t(), [{:filename, term()} | {:method, term()} | {:minProbability, term()}] | nil) :: list({number(), number(), number(), number()}) | {:error, String.t()}
  def detectRegions(image, er_filter1, er_filter2, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and is_struct(er_filter1, Evision.Text.ERFilter) and is_struct(er_filter2, Evision.Text.ERFilter) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:filename, :method, :minProbability])
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      er_filter1: Evision.Internal.Structurise.from_struct(er_filter1),
      er_filter2: Evision.Internal.Structurise.from_struct(er_filter2)
    ]
    :evision_nif.text_detectRegions(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Extracts text regions from image.

  ##### Positional Arguments
  - **image**: `Evision.Mat`.

    Source image where text blocks needs to be extracted from.  Should be CV_8UC3 (color).

  - **er_filter1**: `Evision.Text.ERFilter`.

    Extremal Region Filter for the 1st stage classifier of N&M algorithm @cite Neumann12

  - **er_filter2**: `Evision.Text.ERFilter`.

    Extremal Region Filter for the 2nd stage classifier of N&M algorithm @cite Neumann12

  ##### Keyword Arguments
  - **method**: `integer()`.

    Grouping method (see text::erGrouping_Modes). Can be one of ERGROUPING_ORIENTATION_HORIZ, ERGROUPING_ORIENTATION_ANY.

  - **filename**: `String`.

    The XML or YAML file with the classifier model (e.g. samples/trained_classifier_erGrouping.xml). Only to use when grouping method is ERGROUPING_ORIENTATION_ANY.

  - **minProbability**: `float`.

    The minimum probability for accepting a group. Only to use when grouping method is ERGROUPING_ORIENTATION_ANY.

  ##### Return
  - **groups_rects**: `[Rect]`.

    Output list of rectangle blocks with text

  Python prototype (for reference only):
  ```python3
  detectRegions(image, er_filter1, er_filter2[, method[, filename[, minProbability]]]) -> groups_rects
  ```
  """
  @spec detectRegions(Evision.Mat.maybe_mat_in(), Evision.Text.ERFilter.t(), Evision.Text.ERFilter.t()) :: list({number(), number(), number(), number()}) | {:error, String.t()}
  def detectRegions(image, er_filter1, er_filter2) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and is_struct(er_filter1, Evision.Text.ERFilter) and is_struct(er_filter2, Evision.Text.ERFilter)
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      er_filter1: Evision.Internal.Structurise.from_struct(er_filter1),
      er_filter2: Evision.Internal.Structurise.from_struct(er_filter2)
    ]
    :evision_nif.text_detectRegions(positional)
    |> to_struct()
  end

  @doc """
  Applies the Stroke Width Transform operator followed by filtering of connected components of similar Stroke Widths to return letter candidates. It also chain them by proximity and size, saving the result in chainBBs.

  ##### Positional Arguments
  - **input**: `Evision.Mat`.

    the input image with 3 channels.

  - **dark_on_light**: `bool`.

    a boolean value signifying whether the text is darker or lighter than the background, it is observed to reverse the gradient obtained from Scharr operator, and significantly affect the result.

  ##### Return
  - **result**: `[Rect]`.

    a vector of resulting bounding boxes where probability of finding text is high

  - **draw**: `Evision.Mat.t()`.

    an optional Mat of type CV_8UC3 which visualises the detected letters using bounding boxes.

  - **chainBBs**: `Evision.Mat.t()`.

    an optional parameter which chains the letter candidates according to heuristics in the paper and returns all possible regions where text is likely to occur.

  Python prototype (for reference only):
  ```python3
  detectTextSWT(input, dark_on_light[, draw[, chainBBs]]) -> result, draw, chainBBs
  ```
  """
  @spec detectTextSWT(Evision.Mat.maybe_mat_in(), boolean(), [{atom(), term()},...] | nil) :: {list({number(), number(), number(), number()}), Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def detectTextSWT(input, dark_on_light, opts) when (is_struct(input, Evision.Mat) or is_struct(input, Nx.Tensor) or is_number(input) or is_tuple(input)) and is_boolean(dark_on_light) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      input: Evision.Internal.Structurise.from_struct(input),
      dark_on_light: Evision.Internal.Structurise.from_struct(dark_on_light)
    ]
    :evision_nif.text_detectTextSWT(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Applies the Stroke Width Transform operator followed by filtering of connected components of similar Stroke Widths to return letter candidates. It also chain them by proximity and size, saving the result in chainBBs.

  ##### Positional Arguments
  - **input**: `Evision.Mat`.

    the input image with 3 channels.

  - **dark_on_light**: `bool`.

    a boolean value signifying whether the text is darker or lighter than the background, it is observed to reverse the gradient obtained from Scharr operator, and significantly affect the result.

  ##### Return
  - **result**: `[Rect]`.

    a vector of resulting bounding boxes where probability of finding text is high

  - **draw**: `Evision.Mat.t()`.

    an optional Mat of type CV_8UC3 which visualises the detected letters using bounding boxes.

  - **chainBBs**: `Evision.Mat.t()`.

    an optional parameter which chains the letter candidates according to heuristics in the paper and returns all possible regions where text is likely to occur.

  Python prototype (for reference only):
  ```python3
  detectTextSWT(input, dark_on_light[, draw[, chainBBs]]) -> result, draw, chainBBs
  ```
  """
  @spec detectTextSWT(Evision.Mat.maybe_mat_in(), boolean()) :: {list({number(), number(), number(), number()}), Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def detectTextSWT(input, dark_on_light) when (is_struct(input, Evision.Mat) or is_struct(input, Nx.Tensor) or is_number(input) or is_tuple(input)) and is_boolean(dark_on_light)
  do
    positional = [
      input: Evision.Internal.Structurise.from_struct(input),
      dark_on_light: Evision.Internal.Structurise.from_struct(dark_on_light)
    ]
    :evision_nif.text_detectTextSWT(positional)
    |> to_struct()
  end

  @doc """
  Find groups of Extremal Regions that are organized as text blocks.

  ##### Positional Arguments
  - **image**: `Evision.Mat`
  - **channel**: `Evision.Mat`
  - **regions**: `[[Point]]`.

    Vector of ER's retrieved from the ERFilter algorithm from each channel.

  ##### Keyword Arguments
  - **method**: `integer()`.

    Grouping method (see text::erGrouping_Modes). Can be one of ERGROUPING_ORIENTATION_HORIZ,
    ERGROUPING_ORIENTATION_ANY.

  - **filename**: `String`.

    The XML or YAML file with the classifier model (e.g.
    samples/trained_classifier_erGrouping.xml). Only to use when grouping method is
    ERGROUPING_ORIENTATION_ANY.

  - **minProbablity**: `float`.

    The minimum probability for accepting a group. Only to use when grouping
    method is ERGROUPING_ORIENTATION_ANY.

  ##### Return
  - **groups_rects**: `[Rect]`.

    The output of the algorithm are stored in this parameter as list of rectangles.

  Python prototype (for reference only):
  ```python3
  erGrouping(image, channel, regions[, method[, filename[, minProbablity]]]) -> groups_rects
  ```
  """
  @spec erGrouping(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), list(list({number(), number()})), [{:filename, term()} | {:method, term()} | {:minProbablity, term()}] | nil) :: list({number(), number(), number(), number()}) | {:error, String.t()}
  def erGrouping(image, channel, regions, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (is_struct(channel, Evision.Mat) or is_struct(channel, Nx.Tensor) or is_number(channel) or is_tuple(channel)) and is_list(regions) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:filename, :method, :minProbablity])
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      channel: Evision.Internal.Structurise.from_struct(channel),
      regions: Evision.Internal.Structurise.from_struct(regions)
    ]
    :evision_nif.text_erGrouping(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Find groups of Extremal Regions that are organized as text blocks.

  ##### Positional Arguments
  - **image**: `Evision.Mat`
  - **channel**: `Evision.Mat`
  - **regions**: `[[Point]]`.

    Vector of ER's retrieved from the ERFilter algorithm from each channel.

  ##### Keyword Arguments
  - **method**: `integer()`.

    Grouping method (see text::erGrouping_Modes). Can be one of ERGROUPING_ORIENTATION_HORIZ,
    ERGROUPING_ORIENTATION_ANY.

  - **filename**: `String`.

    The XML or YAML file with the classifier model (e.g.
    samples/trained_classifier_erGrouping.xml). Only to use when grouping method is
    ERGROUPING_ORIENTATION_ANY.

  - **minProbablity**: `float`.

    The minimum probability for accepting a group. Only to use when grouping
    method is ERGROUPING_ORIENTATION_ANY.

  ##### Return
  - **groups_rects**: `[Rect]`.

    The output of the algorithm are stored in this parameter as list of rectangles.

  Python prototype (for reference only):
  ```python3
  erGrouping(image, channel, regions[, method[, filename[, minProbablity]]]) -> groups_rects
  ```
  """
  @spec erGrouping(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), list(list({number(), number()}))) :: list({number(), number(), number(), number()}) | {:error, String.t()}
  def erGrouping(image, channel, regions) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (is_struct(channel, Evision.Mat) or is_struct(channel, Nx.Tensor) or is_number(channel) or is_tuple(channel)) and is_list(regions)
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      channel: Evision.Internal.Structurise.from_struct(channel),
      regions: Evision.Internal.Structurise.from_struct(regions)
    ]
    :evision_nif.text_erGrouping(positional)
    |> to_struct()
  end

  @doc """
  Allow to implicitly load the default classifier when creating an ERFilter object.

  ##### Positional Arguments
  - **filename**: `String`.

    The XML or YAML file with the classifier model (e.g. trained_classifierNM1.xml)

  ##### Return
  - **retval**: `Evision.Text.ERFilter.Callback.t()`

  returns a pointer to ERFilter::Callback.

  Python prototype (for reference only):
  ```python3
  loadClassifierNM1(filename) -> retval
  ```
  """
  @spec loadClassifierNM1(binary()) :: term() | {:error, String.t()}
  def loadClassifierNM1(filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.text_loadClassifierNM1(positional)
    |> to_struct()
  end

  @doc """
  Allow to implicitly load the default classifier when creating an ERFilter object.

  ##### Positional Arguments
  - **filename**: `String`.

    The XML or YAML file with the classifier model (e.g. trained_classifierNM2.xml)

  ##### Return
  - **retval**: `Evision.Text.ERFilter.Callback.t()`

  returns a pointer to ERFilter::Callback.

  Python prototype (for reference only):
  ```python3
  loadClassifierNM2(filename) -> retval
  ```
  """
  @spec loadClassifierNM2(binary()) :: term() | {:error, String.t()}
  def loadClassifierNM2(filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.text_loadClassifierNM2(positional)
    |> to_struct()
  end

  @doc """
  Allow to implicitly load the default character classifier when creating an OCRBeamSearchDecoder object.

  ##### Positional Arguments
  - **filename**: `String`.

    The XML or YAML file with the classifier model (e.g. OCRBeamSearch_CNN_model_data.xml.gz)

  ##### Return
  - **retval**: `Evision.Text.OCRBeamSearchDecoder.ClassifierCallback.t()`

  The CNN default classifier is based in the scene text recognition method proposed by Adam Coates &
  Andrew NG in [Coates11a]. The character classifier consists in a Single Layer Convolutional Neural Network and
  a linear classifier. It is applied to the input image in a sliding window fashion, providing a set of recognitions
  at each window location.

  Python prototype (for reference only):
  ```python3
  loadOCRBeamSearchClassifierCNN(filename) -> retval
  ```
  """
  @spec loadOCRBeamSearchClassifierCNN(binary()) :: term() | {:error, String.t()}
  def loadOCRBeamSearchClassifierCNN(filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.text_loadOCRBeamSearchClassifierCNN(positional)
    |> to_struct()
  end

  @doc """
  Allow to implicitly load the default character classifier when creating an OCRHMMDecoder object.

  ##### Positional Arguments
  - **filename**: `String`.

    The XML or YAML file with the classifier model (e.g. OCRBeamSearch_CNN_model_data.xml.gz)

  - **classifier**: `integer()`.

    Can be one of classifier_type enum values.

  ##### Return
  - **retval**: `Evision.Text.OCRHMMDecoder.ClassifierCallback.t()`

  Python prototype (for reference only):
  ```python3
  loadOCRHMMClassifier(filename, classifier) -> retval
  ```
  """
  @spec loadOCRHMMClassifier(binary(), integer()) :: term() | {:error, String.t()}
  def loadOCRHMMClassifier(filename, classifier) when is_binary(filename) and is_integer(classifier)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename),
      classifier: Evision.Internal.Structurise.from_struct(classifier)
    ]
    :evision_nif.text_loadOCRHMMClassifier(positional)
    |> to_struct()
  end

  @doc """
  Allow to implicitly load the default character classifier when creating an OCRHMMDecoder object.

  ##### Positional Arguments
  - **filename**: `String`.

    The XML or YAML file with the classifier model (e.g. OCRBeamSearch_CNN_model_data.xml.gz)

  ##### Return
  - **retval**: `Evision.Text.OCRHMMDecoder.ClassifierCallback.t()`

  The CNN default classifier is based in the scene text recognition method proposed by Adam Coates &
  Andrew NG in [Coates11a]. The character classifier consists in a Single Layer Convolutional Neural Network and
  a linear classifier. It is applied to the input image in a sliding window fashion, providing a set of recognitions
  at each window location.
  @deprecated use loadOCRHMMClassifier instead

  Python prototype (for reference only):
  ```python3
  loadOCRHMMClassifierCNN(filename) -> retval
  ```
  """
  @spec loadOCRHMMClassifierCNN(binary()) :: term() | {:error, String.t()}
  def loadOCRHMMClassifierCNN(filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.text_loadOCRHMMClassifierCNN(positional)
    |> to_struct()
  end

  @doc """
  Allow to implicitly load the default character classifier when creating an OCRHMMDecoder object.

  ##### Positional Arguments
  - **filename**: `String`.

    The XML or YAML file with the classifier model (e.g. OCRHMM_knn_model_data.xml)

  ##### Return
  - **retval**: `Evision.Text.OCRHMMDecoder.ClassifierCallback.t()`

  The KNN default classifier is based in the scene text recognition method proposed by Lukás Neumann &
  Jiri Matas in [Neumann11b]. Basically, the region (contour) in the input image is normalized to a
  fixed size, while retaining the centroid and aspect ratio, in order to extract a feature vector
  based on gradient orientations along the chain-code of its perimeter. Then, the region is classified
  using a KNN model trained with synthetic data of rendered characters with different standard font
  types.
  @deprecated loadOCRHMMClassifier instead

  Python prototype (for reference only):
  ```python3
  loadOCRHMMClassifierNM(filename) -> retval
  ```
  """
  @spec loadOCRHMMClassifierNM(binary()) :: term() | {:error, String.t()}
  def loadOCRHMMClassifierNM(filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.text_loadOCRHMMClassifierNM(positional)
    |> to_struct()
  end
end
