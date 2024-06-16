defmodule Evision.DNN.TextRecognitionModel do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `DNN.TextRecognitionModel` struct.

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
  def to_struct({:ok, %{class: Evision.DNN.TextRecognitionModel, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.DNN.TextRecognitionModel, ref: ref}) do
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
  Create text recognition model from network represented in one of the supported formats
  Call setDecodeType() and setVocabulary() after constructor to initialize the decoding method

  ##### Positional Arguments
  - **model**: `string`.

    Binary file contains trained weights

  ##### Keyword Arguments
  - **config**: `string`.

    Text file contains network configuration

  ##### Return
  - **self**: `Evision.DNN.TextRecognitionModel.t()`

  Python prototype (for reference only):
  ```python3
  TextRecognitionModel(model[, config]) -> <dnn_TextRecognitionModel object>
  ```
  """
  @spec textRecognitionModel(binary(), [{:config, term()}] | nil) :: Evision.DNN.TextRecognitionModel.t() | {:error, String.t()}
  def textRecognitionModel(model, opts) when is_binary(model) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:config])
    positional = [
      model: Evision.Internal.Structurise.from_struct(model)
    ]
    :evision_nif.dnn_dnn_TextRecognitionModel_TextRecognitionModel(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Create text recognition model from network represented in one of the supported formats
  Call setDecodeType() and setVocabulary() after constructor to initialize the decoding method

  ##### Positional Arguments
  - **model**: `string`.

    Binary file contains trained weights

  ##### Keyword Arguments
  - **config**: `string`.

    Text file contains network configuration

  ##### Return
  - **self**: `Evision.DNN.TextRecognitionModel.t()`

  Python prototype (for reference only):
  ```python3
  TextRecognitionModel(model[, config]) -> <dnn_TextRecognitionModel object>
  ```
  #### Variant 2:
  Create Text Recognition model from deep learning network
  Call setDecodeType() and setVocabulary() after constructor to initialize the decoding method

  ##### Positional Arguments
  - **network**: `Evision.DNN.Net.t()`.

    Net object

  ##### Return
  - **self**: `Evision.DNN.TextRecognitionModel.t()`

  Python prototype (for reference only):
  ```python3
  TextRecognitionModel(network) -> <dnn_TextRecognitionModel object>
  ```

  """
  @spec textRecognitionModel(binary()) :: Evision.DNN.TextRecognitionModel.t() | {:error, String.t()}
  def textRecognitionModel(model) when is_binary(model)
  do
    positional = [
      model: Evision.Internal.Structurise.from_struct(model)
    ]
    :evision_nif.dnn_dnn_TextRecognitionModel_TextRecognitionModel(positional)
    |> to_struct()
  end
  @spec textRecognitionModel(Evision.DNN.Net.t()) :: Evision.DNN.TextRecognitionModel.t() | {:error, String.t()}
  def textRecognitionModel(network) when is_struct(network, Evision.DNN.Net)
  do
    positional = [
      network: Evision.Internal.Structurise.from_struct(network)
    ]
    :evision_nif.dnn_dnn_TextRecognitionModel_TextRecognitionModel(positional)
    |> to_struct()
  end

  @doc """
  enableWinograd

  ##### Positional Arguments
  - **self**: `Evision.DNN.TextRecognitionModel.t()`
  - **useWinograd**: `bool`

  ##### Return
  - **retval**: `Evision.DNN.Model.t()`

  Python prototype (for reference only):
  ```python3
  enableWinograd(useWinograd) -> retval
  ```
  """
  @spec enableWinograd(Evision.DNN.TextRecognitionModel.t(), boolean()) :: Evision.DNN.Model.t() | {:error, String.t()}
  def enableWinograd(self, useWinograd) when is_boolean(useWinograd)
  do
    positional = [
      useWinograd: Evision.Internal.Structurise.from_struct(useWinograd)
    ]
    :evision_nif.dnn_dnn_TextRecognitionModel_enableWinograd(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Get the decoding method

  ##### Positional Arguments
  - **self**: `Evision.DNN.TextRecognitionModel.t()`

  ##### Return
  - **retval**: `string`

  @return the decoding method

  Python prototype (for reference only):
  ```python3
  getDecodeType() -> retval
  ```
  """
  @spec getDecodeType(Evision.DNN.TextRecognitionModel.t()) :: binary() | {:error, String.t()}
  def getDecodeType(self) do
    positional = [
    ]
    :evision_nif.dnn_dnn_TextRecognitionModel_getDecodeType(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Get the vocabulary for recognition.

  ##### Positional Arguments
  - **self**: `Evision.DNN.TextRecognitionModel.t()`

  ##### Return
  - **retval**: `[string]`

  @return vocabulary the associated vocabulary

  Python prototype (for reference only):
  ```python3
  getVocabulary() -> retval
  ```
  """
  @spec getVocabulary(Evision.DNN.TextRecognitionModel.t()) :: list(binary()) | {:error, String.t()}
  def getVocabulary(self) do
    positional = [
    ]
    :evision_nif.dnn_dnn_TextRecognitionModel_getVocabulary(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Given the @p input frame, create input blob, run net and return the output @p blobs.

  ##### Positional Arguments
  - **self**: `Evision.DNN.TextRecognitionModel.t()`
  - **frame**: `Evision.Mat`

  ##### Return
  - **outs**: `[Evision.Mat]`.

    Allocated output blobs, which will store results of the computation.

  Python prototype (for reference only):
  ```python3
  predict(frame[, outs]) -> outs
  ```
  """
  @spec predict(Evision.DNN.TextRecognitionModel.t(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: list(Evision.Mat.t()) | {:error, String.t()}
  def predict(self, frame, opts) when (is_struct(frame, Evision.Mat) or is_struct(frame, Nx.Tensor) or is_number(frame) or is_tuple(frame)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      frame: Evision.Internal.Structurise.from_struct(frame)
    ]
    :evision_nif.dnn_dnn_TextRecognitionModel_predict(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Given the @p input frame, create input blob, run net and return the output @p blobs.

  ##### Positional Arguments
  - **self**: `Evision.DNN.TextRecognitionModel.t()`
  - **frame**: `Evision.Mat`

  ##### Return
  - **outs**: `[Evision.Mat]`.

    Allocated output blobs, which will store results of the computation.

  Python prototype (for reference only):
  ```python3
  predict(frame[, outs]) -> outs
  ```
  """
  @spec predict(Evision.DNN.TextRecognitionModel.t(), Evision.Mat.maybe_mat_in()) :: list(Evision.Mat.t()) | {:error, String.t()}
  def predict(self, frame) when (is_struct(frame, Evision.Mat) or is_struct(frame, Nx.Tensor) or is_number(frame) or is_tuple(frame))
  do
    positional = [
      frame: Evision.Internal.Structurise.from_struct(frame)
    ]
    :evision_nif.dnn_dnn_TextRecognitionModel_predict(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Given the @p input frame, create input blob, run net and return recognition result

  ##### Positional Arguments
  - **self**: `Evision.DNN.TextRecognitionModel.t()`
  - **frame**: `Evision.Mat`.

    The input image

  - **roiRects**: `[Evision.Mat]`.

    List of text detection regions of interest (cv::Rect, CV_32SC4). ROIs is be cropped as the network inputs

  ##### Return
  - **results**: `[string]`.

    A set of text recognition results.

  Python prototype (for reference only):
  ```python3
  recognize(frame, roiRects) -> results
  ```
  """
  @spec recognize(Evision.DNN.TextRecognitionModel.t(), Evision.Mat.maybe_mat_in(), list(Evision.Mat.maybe_mat_in())) :: list(binary()) | {:error, String.t()}
  def recognize(self, frame, roiRects) when (is_struct(frame, Evision.Mat) or is_struct(frame, Nx.Tensor) or is_number(frame) or is_tuple(frame)) and is_list(roiRects)
  do
    positional = [
      frame: Evision.Internal.Structurise.from_struct(frame),
      roiRects: Evision.Internal.Structurise.from_struct(roiRects)
    ]
    :evision_nif.dnn_dnn_TextRecognitionModel_recognize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Given the @p input frame, create input blob, run net and return recognition result

  ##### Positional Arguments
  - **self**: `Evision.DNN.TextRecognitionModel.t()`
  - **frame**: `Evision.Mat`.

    The input image

  ##### Return
  - **retval**: `string`

  @return The text recognition result

  Python prototype (for reference only):
  ```python3
  recognize(frame) -> retval
  ```
  """
  @spec recognize(Evision.DNN.TextRecognitionModel.t(), Evision.Mat.maybe_mat_in()) :: binary() | {:error, String.t()}
  def recognize(self, frame) when (is_struct(frame, Evision.Mat) or is_struct(frame, Nx.Tensor) or is_number(frame) or is_tuple(frame))
  do
    positional = [
      frame: Evision.Internal.Structurise.from_struct(frame)
    ]
    :evision_nif.dnn_dnn_TextRecognitionModel_recognize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Set the decoding method options for `"CTC-prefix-beam-search"` decode usage

  ##### Positional Arguments
  - **self**: `Evision.DNN.TextRecognitionModel.t()`
  - **beamSize**: `integer()`.

    Beam size for search

  ##### Keyword Arguments
  - **vocPruneSize**: `integer()`.

    Parameter to optimize big vocabulary search,
    only take top @p vocPruneSize tokens in each search step, @p vocPruneSize <= 0 stands for disable this prune.

  ##### Return
  - **retval**: `Evision.DNN.TextRecognitionModel.t()`

  Python prototype (for reference only):
  ```python3
  setDecodeOptsCTCPrefixBeamSearch(beamSize[, vocPruneSize]) -> retval
  ```
  """
  @spec setDecodeOptsCTCPrefixBeamSearch(Evision.DNN.TextRecognitionModel.t(), integer(), [{:vocPruneSize, term()}] | nil) :: Evision.DNN.TextRecognitionModel.t() | {:error, String.t()}
  def setDecodeOptsCTCPrefixBeamSearch(self, beamSize, opts) when is_integer(beamSize) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:vocPruneSize])
    positional = [
      beamSize: Evision.Internal.Structurise.from_struct(beamSize)
    ]
    :evision_nif.dnn_dnn_TextRecognitionModel_setDecodeOptsCTCPrefixBeamSearch(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Set the decoding method options for `"CTC-prefix-beam-search"` decode usage

  ##### Positional Arguments
  - **self**: `Evision.DNN.TextRecognitionModel.t()`
  - **beamSize**: `integer()`.

    Beam size for search

  ##### Keyword Arguments
  - **vocPruneSize**: `integer()`.

    Parameter to optimize big vocabulary search,
    only take top @p vocPruneSize tokens in each search step, @p vocPruneSize <= 0 stands for disable this prune.

  ##### Return
  - **retval**: `Evision.DNN.TextRecognitionModel.t()`

  Python prototype (for reference only):
  ```python3
  setDecodeOptsCTCPrefixBeamSearch(beamSize[, vocPruneSize]) -> retval
  ```
  """
  @spec setDecodeOptsCTCPrefixBeamSearch(Evision.DNN.TextRecognitionModel.t(), integer()) :: Evision.DNN.TextRecognitionModel.t() | {:error, String.t()}
  def setDecodeOptsCTCPrefixBeamSearch(self, beamSize) when is_integer(beamSize)
  do
    positional = [
      beamSize: Evision.Internal.Structurise.from_struct(beamSize)
    ]
    :evision_nif.dnn_dnn_TextRecognitionModel_setDecodeOptsCTCPrefixBeamSearch(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Set the decoding method of translating the network output into string

  ##### Positional Arguments
  - **self**: `Evision.DNN.TextRecognitionModel.t()`
  - **decodeType**: `string`.

    The decoding method of translating the network output into string, currently supported type:
    - `"CTC-greedy"` greedy decoding for the output of CTC-based methods
    - `"CTC-prefix-beam-search"` Prefix beam search decoding for the output of CTC-based methods

  ##### Return
  - **retval**: `Evision.DNN.TextRecognitionModel.t()`

  Python prototype (for reference only):
  ```python3
  setDecodeType(decodeType) -> retval
  ```
  """
  @spec setDecodeType(Evision.DNN.TextRecognitionModel.t(), binary()) :: Evision.DNN.TextRecognitionModel.t() | {:error, String.t()}
  def setDecodeType(self, decodeType) when is_binary(decodeType)
  do
    positional = [
      decodeType: Evision.Internal.Structurise.from_struct(decodeType)
    ]
    :evision_nif.dnn_dnn_TextRecognitionModel_setDecodeType(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Set flag crop for frame.

  ##### Positional Arguments
  - **self**: `Evision.DNN.TextRecognitionModel.t()`
  - **crop**: `bool`.

    Flag which indicates whether image will be cropped after resize or not.

  ##### Return
  - **retval**: `Evision.DNN.Model.t()`

  Python prototype (for reference only):
  ```python3
  setInputCrop(crop) -> retval
  ```
  """
  @spec setInputCrop(Evision.DNN.TextRecognitionModel.t(), boolean()) :: Evision.DNN.Model.t() | {:error, String.t()}
  def setInputCrop(self, crop) when is_boolean(crop)
  do
    positional = [
      crop: Evision.Internal.Structurise.from_struct(crop)
    ]
    :evision_nif.dnn_dnn_TextRecognitionModel_setInputCrop(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Set mean value for frame.

  ##### Positional Arguments
  - **self**: `Evision.DNN.TextRecognitionModel.t()`
  - **mean**: `Evision.scalar()`.

    Scalar with mean values which are subtracted from channels.

  ##### Return
  - **retval**: `Evision.DNN.Model.t()`

  Python prototype (for reference only):
  ```python3
  setInputMean(mean) -> retval
  ```
  """
  @spec setInputMean(Evision.DNN.TextRecognitionModel.t(), Evision.scalar()) :: Evision.DNN.Model.t() | {:error, String.t()}
  def setInputMean(self, mean) when (is_number(mean) or is_tuple(mean))
  do
    positional = [
      mean: Evision.Internal.Structurise.from_struct(mean)
    ]
    :evision_nif.dnn_dnn_TextRecognitionModel_setInputMean(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Set preprocessing parameters for frame.

  ##### Positional Arguments
  - **self**: `Evision.DNN.TextRecognitionModel.t()`

  ##### Keyword Arguments
  - **scale**: `double`.

    Multiplier for frame values.

  - **size**: `Size`.

    New input size.

  - **mean**: `Evision.scalar()`.

    Scalar with mean values which are subtracted from channels.

  - **swapRB**: `bool`.

    Flag which indicates that swap first and last channels.

  - **crop**: `bool`.

    Flag which indicates whether image will be cropped after resize or not.
    blob(n, c, y, x) = scale * resize( frame(y, x, c) ) - mean(c) )

  Python prototype (for reference only):
  ```python3
  setInputParams([, scale[, size[, mean[, swapRB[, crop]]]]]) -> None
  ```
  """
  @spec setInputParams(Evision.DNN.TextRecognitionModel.t(), [{:crop, term()} | {:mean, term()} | {:scale, term()} | {:size, term()} | {:swapRB, term()}] | nil) :: Evision.DNN.Model.t() | {:error, String.t()}
  def setInputParams(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:crop, :mean, :scale, :size, :swapRB])
    positional = [
    ]
    :evision_nif.dnn_dnn_TextRecognitionModel_setInputParams(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Set preprocessing parameters for frame.

  ##### Positional Arguments
  - **self**: `Evision.DNN.TextRecognitionModel.t()`

  ##### Keyword Arguments
  - **scale**: `double`.

    Multiplier for frame values.

  - **size**: `Size`.

    New input size.

  - **mean**: `Evision.scalar()`.

    Scalar with mean values which are subtracted from channels.

  - **swapRB**: `bool`.

    Flag which indicates that swap first and last channels.

  - **crop**: `bool`.

    Flag which indicates whether image will be cropped after resize or not.
    blob(n, c, y, x) = scale * resize( frame(y, x, c) ) - mean(c) )

  Python prototype (for reference only):
  ```python3
  setInputParams([, scale[, size[, mean[, swapRB[, crop]]]]]) -> None
  ```
  """
  @spec setInputParams(Evision.DNN.TextRecognitionModel.t()) :: Evision.DNN.Model.t() | {:error, String.t()}
  def setInputParams(self) do
    positional = [
    ]
    :evision_nif.dnn_dnn_TextRecognitionModel_setInputParams(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Set scalefactor value for frame.

  ##### Positional Arguments
  - **self**: `Evision.DNN.TextRecognitionModel.t()`
  - **scale**: `Evision.scalar()`.

    Multiplier for frame values.

  ##### Return
  - **retval**: `Evision.DNN.Model.t()`

  Python prototype (for reference only):
  ```python3
  setInputScale(scale) -> retval
  ```
  """
  @spec setInputScale(Evision.DNN.TextRecognitionModel.t(), Evision.scalar()) :: Evision.DNN.Model.t() | {:error, String.t()}
  def setInputScale(self, scale) when (is_number(scale) or is_tuple(scale))
  do
    positional = [
      scale: Evision.Internal.Structurise.from_struct(scale)
    ]
    :evision_nif.dnn_dnn_TextRecognitionModel_setInputScale(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setInputSize

  ##### Positional Arguments
  - **self**: `Evision.DNN.TextRecognitionModel.t()`
  - **width**: `integer()`.

    New input width.

  - **height**: `integer()`.

    New input height.

  ##### Return
  - **retval**: `Evision.DNN.Model.t()`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  setInputSize(width, height) -> retval
  ```
  """
  @spec setInputSize(Evision.DNN.TextRecognitionModel.t(), integer(), integer()) :: Evision.DNN.Model.t() | {:error, String.t()}
  def setInputSize(self, width, height) when is_integer(width) and is_integer(height)
  do
    positional = [
      width: Evision.Internal.Structurise.from_struct(width),
      height: Evision.Internal.Structurise.from_struct(height)
    ]
    :evision_nif.dnn_dnn_TextRecognitionModel_setInputSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Set input size for frame.

  ##### Positional Arguments
  - **self**: `Evision.DNN.TextRecognitionModel.t()`
  - **size**: `Size`.

    New input size.

  ##### Return
  - **retval**: `Evision.DNN.Model.t()`

  **Note**: If shape of the new blob less than 0, then frame size not change.

  Python prototype (for reference only):
  ```python3
  setInputSize(size) -> retval
  ```
  """
  @spec setInputSize(Evision.DNN.TextRecognitionModel.t(), {number(), number()}) :: Evision.DNN.Model.t() | {:error, String.t()}
  def setInputSize(self, size) when is_tuple(size)
  do
    positional = [
      size: Evision.Internal.Structurise.from_struct(size)
    ]
    :evision_nif.dnn_dnn_TextRecognitionModel_setInputSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Set flag swapRB for frame.

  ##### Positional Arguments
  - **self**: `Evision.DNN.TextRecognitionModel.t()`
  - **swapRB**: `bool`.

    Flag which indicates that swap first and last channels.

  ##### Return
  - **retval**: `Evision.DNN.Model.t()`

  Python prototype (for reference only):
  ```python3
  setInputSwapRB(swapRB) -> retval
  ```
  """
  @spec setInputSwapRB(Evision.DNN.TextRecognitionModel.t(), boolean()) :: Evision.DNN.Model.t() | {:error, String.t()}
  def setInputSwapRB(self, swapRB) when is_boolean(swapRB)
  do
    positional = [
      swapRB: Evision.Internal.Structurise.from_struct(swapRB)
    ]
    :evision_nif.dnn_dnn_TextRecognitionModel_setInputSwapRB(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Set output names for frame.

  ##### Positional Arguments
  - **self**: `Evision.DNN.TextRecognitionModel.t()`
  - **outNames**: `[String]`.

    Names for output layers.

  ##### Return
  - **retval**: `Evision.DNN.Model.t()`

  Python prototype (for reference only):
  ```python3
  setOutputNames(outNames) -> retval
  ```
  """
  @spec setOutputNames(Evision.DNN.TextRecognitionModel.t(), list(binary())) :: Evision.DNN.Model.t() | {:error, String.t()}
  def setOutputNames(self, outNames) when is_list(outNames)
  do
    positional = [
      outNames: Evision.Internal.Structurise.from_struct(outNames)
    ]
    :evision_nif.dnn_dnn_TextRecognitionModel_setOutputNames(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setPreferableBackend

  ##### Positional Arguments
  - **self**: `Evision.DNN.TextRecognitionModel.t()`
  - **backendId**: `dnn_Backend`

  ##### Return
  - **retval**: `Evision.DNN.Model.t()`

  Python prototype (for reference only):
  ```python3
  setPreferableBackend(backendId) -> retval
  ```
  """
  @spec setPreferableBackend(Evision.DNN.TextRecognitionModel.t(), Evision.DNN.Backend.enum()) :: Evision.DNN.Model.t() | {:error, String.t()}
  def setPreferableBackend(self, backendId) when is_integer(backendId)
  do
    positional = [
      backendId: Evision.Internal.Structurise.from_struct(backendId)
    ]
    :evision_nif.dnn_dnn_TextRecognitionModel_setPreferableBackend(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setPreferableTarget

  ##### Positional Arguments
  - **self**: `Evision.DNN.TextRecognitionModel.t()`
  - **targetId**: `dnn_Target`

  ##### Return
  - **retval**: `Evision.DNN.Model.t()`

  Python prototype (for reference only):
  ```python3
  setPreferableTarget(targetId) -> retval
  ```
  """
  @spec setPreferableTarget(Evision.DNN.TextRecognitionModel.t(), Evision.DNN.Target.enum()) :: Evision.DNN.Model.t() | {:error, String.t()}
  def setPreferableTarget(self, targetId) when is_integer(targetId)
  do
    positional = [
      targetId: Evision.Internal.Structurise.from_struct(targetId)
    ]
    :evision_nif.dnn_dnn_TextRecognitionModel_setPreferableTarget(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Set the vocabulary for recognition.

  ##### Positional Arguments
  - **self**: `Evision.DNN.TextRecognitionModel.t()`
  - **vocabulary**: `[string]`.

    the associated vocabulary of the network.

  ##### Return
  - **retval**: `Evision.DNN.TextRecognitionModel.t()`

  Python prototype (for reference only):
  ```python3
  setVocabulary(vocabulary) -> retval
  ```
  """
  @spec setVocabulary(Evision.DNN.TextRecognitionModel.t(), list(binary())) :: Evision.DNN.TextRecognitionModel.t() | {:error, String.t()}
  def setVocabulary(self, vocabulary) when is_list(vocabulary)
  do
    positional = [
      vocabulary: Evision.Internal.Structurise.from_struct(vocabulary)
    ]
    :evision_nif.dnn_dnn_TextRecognitionModel_setVocabulary(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
