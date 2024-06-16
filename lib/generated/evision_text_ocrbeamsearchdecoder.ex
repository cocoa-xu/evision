defmodule Evision.Text.OCRBeamSearchDecoder do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Text.OCRBeamSearchDecoder` struct.

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
  def to_struct({:ok, %{class: Evision.Text.OCRBeamSearchDecoder, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Text.OCRBeamSearchDecoder, ref: ref}) do
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
  Creates an instance of the OCRBeamSearchDecoder class. Initializes HMMDecoder.

  ##### Positional Arguments
  - **classifier**: `Evision.Text.OCRBeamSearchDecoder.ClassifierCallback`.

    The character classifier with built in feature extractor.

  - **vocabulary**: `string`.

    The language vocabulary (chars when ASCII English text). vocabulary.size()
    must be equal to the number of classes of the classifier.

  - **transition_probabilities_table**: `Evision.Mat`.

    Table with transition probabilities between character
    pairs. cols == rows == vocabulary.size().

  - **emission_probabilities_table**: `Evision.Mat`.

    Table with observation emission probabilities. cols ==
    rows == vocabulary.size().

  ##### Keyword Arguments
  - **mode**: `text_decoder_mode`.

    HMM Decoding algorithm. Only OCR_DECODER_VITERBI is available for the moment
    (<http://en.wikipedia.org/wiki/Viterbi_algorithm>).

  - **beam_size**: `integer()`.

    Size of the beam in Beam Search algorithm.

  ##### Return
  - **retval**: `OCRBeamSearchDecoder`

  Python prototype (for reference only):
  ```python3
  create(classifier, vocabulary, transition_probabilities_table, emission_probabilities_table[, mode[, beam_size]]) -> retval
  ```
  """
  @spec create(term(), binary(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:beam_size, term()} | {:mode, term()}] | nil) :: Evision.Text.OCRBeamSearchDecoder.t() | {:error, String.t()}
  def create(classifier, vocabulary, transition_probabilities_table, emission_probabilities_table, opts) when is_struct(classifier, Evision.Text.OCRBeamSearchDecoder.ClassifierCallback) and is_binary(vocabulary) and (is_struct(transition_probabilities_table, Evision.Mat) or is_struct(transition_probabilities_table, Nx.Tensor) or is_number(transition_probabilities_table) or is_tuple(transition_probabilities_table)) and (is_struct(emission_probabilities_table, Evision.Mat) or is_struct(emission_probabilities_table, Nx.Tensor) or is_number(emission_probabilities_table) or is_tuple(emission_probabilities_table)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:beam_size, :mode])
    positional = [
      classifier: Evision.Internal.Structurise.from_struct(classifier),
      vocabulary: Evision.Internal.Structurise.from_struct(vocabulary),
      transition_probabilities_table: Evision.Internal.Structurise.from_struct(transition_probabilities_table),
      emission_probabilities_table: Evision.Internal.Structurise.from_struct(emission_probabilities_table)
    ]
    :evision_nif.text_text_OCRBeamSearchDecoder_create_static(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Creates an instance of the OCRBeamSearchDecoder class. Initializes HMMDecoder.

  ##### Positional Arguments
  - **classifier**: `Evision.Text.OCRBeamSearchDecoder.ClassifierCallback`.

    The character classifier with built in feature extractor.

  - **vocabulary**: `string`.

    The language vocabulary (chars when ASCII English text). vocabulary.size()
    must be equal to the number of classes of the classifier.

  - **transition_probabilities_table**: `Evision.Mat`.

    Table with transition probabilities between character
    pairs. cols == rows == vocabulary.size().

  - **emission_probabilities_table**: `Evision.Mat`.

    Table with observation emission probabilities. cols ==
    rows == vocabulary.size().

  ##### Keyword Arguments
  - **mode**: `text_decoder_mode`.

    HMM Decoding algorithm. Only OCR_DECODER_VITERBI is available for the moment
    (<http://en.wikipedia.org/wiki/Viterbi_algorithm>).

  - **beam_size**: `integer()`.

    Size of the beam in Beam Search algorithm.

  ##### Return
  - **retval**: `OCRBeamSearchDecoder`

  Python prototype (for reference only):
  ```python3
  create(classifier, vocabulary, transition_probabilities_table, emission_probabilities_table[, mode[, beam_size]]) -> retval
  ```
  """
  @spec create(term(), binary(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.Text.OCRBeamSearchDecoder.t() | {:error, String.t()}
  def create(classifier, vocabulary, transition_probabilities_table, emission_probabilities_table) when is_struct(classifier, Evision.Text.OCRBeamSearchDecoder.ClassifierCallback) and is_binary(vocabulary) and (is_struct(transition_probabilities_table, Evision.Mat) or is_struct(transition_probabilities_table, Nx.Tensor) or is_number(transition_probabilities_table) or is_tuple(transition_probabilities_table)) and (is_struct(emission_probabilities_table, Evision.Mat) or is_struct(emission_probabilities_table, Nx.Tensor) or is_number(emission_probabilities_table) or is_tuple(emission_probabilities_table))
  do
    positional = [
      classifier: Evision.Internal.Structurise.from_struct(classifier),
      vocabulary: Evision.Internal.Structurise.from_struct(vocabulary),
      transition_probabilities_table: Evision.Internal.Structurise.from_struct(transition_probabilities_table),
      emission_probabilities_table: Evision.Internal.Structurise.from_struct(emission_probabilities_table)
    ]
    :evision_nif.text_text_OCRBeamSearchDecoder_create_static(positional)
    |> to_struct()
  end

  @doc """
  run

  ##### Positional Arguments
  - **self**: `Evision.Text.OCRBeamSearchDecoder.t()`
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
  @spec run(Evision.Text.OCRBeamSearchDecoder.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), integer(), [{:component_level, term()}] | nil) :: binary() | {:error, String.t()}
  def run(self, image, mask, min_confidence, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (is_struct(mask, Evision.Mat) or is_struct(mask, Nx.Tensor) or is_number(mask) or is_tuple(mask)) and is_integer(min_confidence) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:component_level])
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      mask: Evision.Internal.Structurise.from_struct(mask),
      min_confidence: Evision.Internal.Structurise.from_struct(min_confidence)
    ]
    :evision_nif.text_text_OCRBeamSearchDecoder_run(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  run

  ##### Positional Arguments
  - **self**: `Evision.Text.OCRBeamSearchDecoder.t()`
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
  Recognize text using Beam Search.

  ##### Positional Arguments
  - **self**: `Evision.Text.OCRBeamSearchDecoder.t()`
  - **image**: `Evision.Mat`.

    Input binary image CV_8UC1 with a single text line (or word).

  - **min_confidence**: `integer()`

  ##### Keyword Arguments
  - **component_level**: `integer()`.

    Only OCR_LEVEL_WORD is supported.

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
  @spec run(Evision.Text.OCRBeamSearchDecoder.t(), Evision.Mat.maybe_mat_in(), integer(), [{:component_level, term()}] | nil) :: binary() | {:error, String.t()}
  def run(self, image, min_confidence, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and is_integer(min_confidence) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:component_level])
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      min_confidence: Evision.Internal.Structurise.from_struct(min_confidence)
    ]
    :evision_nif.text_text_OCRBeamSearchDecoder_run(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec run(Evision.Text.OCRBeamSearchDecoder.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), integer()) :: binary() | {:error, String.t()}
  def run(self, image, mask, min_confidence) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (is_struct(mask, Evision.Mat) or is_struct(mask, Nx.Tensor) or is_number(mask) or is_tuple(mask)) and is_integer(min_confidence)
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      mask: Evision.Internal.Structurise.from_struct(mask),
      min_confidence: Evision.Internal.Structurise.from_struct(min_confidence)
    ]
    :evision_nif.text_text_OCRBeamSearchDecoder_run(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Recognize text using Beam Search.

  ##### Positional Arguments
  - **self**: `Evision.Text.OCRBeamSearchDecoder.t()`
  - **image**: `Evision.Mat`.

    Input binary image CV_8UC1 with a single text line (or word).

  - **min_confidence**: `integer()`

  ##### Keyword Arguments
  - **component_level**: `integer()`.

    Only OCR_LEVEL_WORD is supported.

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
  @spec run(Evision.Text.OCRBeamSearchDecoder.t(), Evision.Mat.maybe_mat_in(), integer()) :: binary() | {:error, String.t()}
  def run(self, image, min_confidence) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and is_integer(min_confidence)
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      min_confidence: Evision.Internal.Structurise.from_struct(min_confidence)
    ]
    :evision_nif.text_text_OCRBeamSearchDecoder_run(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
