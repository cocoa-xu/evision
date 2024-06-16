defmodule Evision.DNN.TextDetectionModelDB do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `DNN.TextDetectionModelDB` struct.

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
  def to_struct({:ok, %{class: Evision.DNN.TextDetectionModelDB, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.DNN.TextDetectionModelDB, ref: ref}) do
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
  Create text detection model from network represented in one of the supported formats.
  An order of @p model and @p config arguments does not matter.

  ##### Positional Arguments
  - **model**: `string`.

    Binary file contains trained weights.

  ##### Keyword Arguments
  - **config**: `string`.

    Text file contains network configuration.

  ##### Return
  - **self**: `Evision.DNN.TextDetectionModelDB.t()`

  Python prototype (for reference only):
  ```python3
  TextDetectionModel_DB(model[, config]) -> <dnn_TextDetectionModel_DB object>
  ```
  """
  @spec textDetectionModelDB(binary(), [{:config, term()}] | nil) :: Evision.DNN.TextDetectionModelDB.t() | {:error, String.t()}
  def textDetectionModelDB(model, opts) when is_binary(model) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:config])
    positional = [
      model: Evision.Internal.Structurise.from_struct(model)
    ]
    :evision_nif.dnn_dnn_TextDetectionModel_DB_TextDetectionModel_DB(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Create text detection model from network represented in one of the supported formats.
  An order of @p model and @p config arguments does not matter.

  ##### Positional Arguments
  - **model**: `string`.

    Binary file contains trained weights.

  ##### Keyword Arguments
  - **config**: `string`.

    Text file contains network configuration.

  ##### Return
  - **self**: `Evision.DNN.TextDetectionModelDB.t()`

  Python prototype (for reference only):
  ```python3
  TextDetectionModel_DB(model[, config]) -> <dnn_TextDetectionModel_DB object>
  ```
  #### Variant 2:
  Create text detection algorithm from deep learning network.

  ##### Positional Arguments
  - **network**: `Evision.DNN.Net.t()`.

    Net object.

  ##### Return
  - **self**: `Evision.DNN.TextDetectionModelDB.t()`

  Python prototype (for reference only):
  ```python3
  TextDetectionModel_DB(network) -> <dnn_TextDetectionModel_DB object>
  ```

  """
  @spec textDetectionModelDB(binary()) :: Evision.DNN.TextDetectionModelDB.t() | {:error, String.t()}
  def textDetectionModelDB(model) when is_binary(model)
  do
    positional = [
      model: Evision.Internal.Structurise.from_struct(model)
    ]
    :evision_nif.dnn_dnn_TextDetectionModel_DB_TextDetectionModel_DB(positional)
    |> to_struct()
  end
  @spec textDetectionModelDB(Evision.DNN.Net.t()) :: Evision.DNN.TextDetectionModelDB.t() | {:error, String.t()}
  def textDetectionModelDB(network) when is_struct(network, Evision.DNN.Net)
  do
    positional = [
      network: Evision.Internal.Structurise.from_struct(network)
    ]
    :evision_nif.dnn_dnn_TextDetectionModel_DB_TextDetectionModel_DB(positional)
    |> to_struct()
  end

  @doc """
  detect

  ##### Positional Arguments
  - **self**: `Evision.DNN.TextDetectionModelDB.t()`
  - **frame**: `Evision.Mat`

  ##### Return
  - **detections**: `[[Point]]`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  detect(frame) -> detections
  ```
  """
  @spec detect(Evision.DNN.TextDetectionModelDB.t(), Evision.Mat.maybe_mat_in()) :: list(list({number(), number()})) | {:error, String.t()}
  def detect(self, frame) when (is_struct(frame, Evision.Mat) or is_struct(frame, Nx.Tensor) or is_number(frame) or is_tuple(frame))
  do
    positional = [
      frame: Evision.Internal.Structurise.from_struct(frame)
    ]
    :evision_nif.dnn_dnn_TextDetectionModel_DB_detect(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Performs detection

  ##### Positional Arguments
  - **self**: `Evision.DNN.TextDetectionModelDB.t()`
  - **frame**: `Evision.Mat`.

    the input image

  ##### Return
  - **detections**: `[{centre={x, y}, size={s1, s2}, angle}]`.

    array with detections' RotationRect results

  - **confidences**: `[float]`.

    array with detection confidences

   Given the input @p frame, prepare network input, run network inference, post-process network output and return result detections.
   Each result is rotated rectangle.
  **Note**: Result may be inaccurate in case of strong perspective transformations.

  Python prototype (for reference only):
  ```python3
  detectTextRectangles(frame) -> detections, confidences
  ```
  """
  @spec detectTextRectangles(Evision.DNN.TextDetectionModelDB.t(), Evision.Mat.maybe_mat_in()) :: {list({{number(), number()}, {number(), number()}, number()}), list(number())} | {:error, String.t()}
  def detectTextRectangles(self, frame) when (is_struct(frame, Evision.Mat) or is_struct(frame, Nx.Tensor) or is_number(frame) or is_tuple(frame))
  do
    positional = [
      frame: Evision.Internal.Structurise.from_struct(frame)
    ]
    :evision_nif.dnn_dnn_TextDetectionModel_DB_detectTextRectangles(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  enableWinograd

  ##### Positional Arguments
  - **self**: `Evision.DNN.TextDetectionModelDB.t()`
  - **useWinograd**: `bool`

  ##### Return
  - **retval**: `Evision.DNN.Model.t()`

  Python prototype (for reference only):
  ```python3
  enableWinograd(useWinograd) -> retval
  ```
  """
  @spec enableWinograd(Evision.DNN.TextDetectionModelDB.t(), boolean()) :: Evision.DNN.Model.t() | {:error, String.t()}
  def enableWinograd(self, useWinograd) when is_boolean(useWinograd)
  do
    positional = [
      useWinograd: Evision.Internal.Structurise.from_struct(useWinograd)
    ]
    :evision_nif.dnn_dnn_TextDetectionModel_DB_enableWinograd(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getBinaryThreshold

  ##### Positional Arguments
  - **self**: `Evision.DNN.TextDetectionModelDB.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  getBinaryThreshold() -> retval
  ```
  """
  @spec getBinaryThreshold(Evision.DNN.TextDetectionModelDB.t()) :: number() | {:error, String.t()}
  def getBinaryThreshold(self) do
    positional = [
    ]
    :evision_nif.dnn_dnn_TextDetectionModel_DB_getBinaryThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMaxCandidates

  ##### Positional Arguments
  - **self**: `Evision.DNN.TextDetectionModelDB.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getMaxCandidates() -> retval
  ```
  """
  @spec getMaxCandidates(Evision.DNN.TextDetectionModelDB.t()) :: integer() | {:error, String.t()}
  def getMaxCandidates(self) do
    positional = [
    ]
    :evision_nif.dnn_dnn_TextDetectionModel_DB_getMaxCandidates(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getPolygonThreshold

  ##### Positional Arguments
  - **self**: `Evision.DNN.TextDetectionModelDB.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  getPolygonThreshold() -> retval
  ```
  """
  @spec getPolygonThreshold(Evision.DNN.TextDetectionModelDB.t()) :: number() | {:error, String.t()}
  def getPolygonThreshold(self) do
    positional = [
    ]
    :evision_nif.dnn_dnn_TextDetectionModel_DB_getPolygonThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getUnclipRatio

  ##### Positional Arguments
  - **self**: `Evision.DNN.TextDetectionModelDB.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getUnclipRatio() -> retval
  ```
  """
  @spec getUnclipRatio(Evision.DNN.TextDetectionModelDB.t()) :: number() | {:error, String.t()}
  def getUnclipRatio(self) do
    positional = [
    ]
    :evision_nif.dnn_dnn_TextDetectionModel_DB_getUnclipRatio(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Given the @p input frame, create input blob, run net and return the output @p blobs.

  ##### Positional Arguments
  - **self**: `Evision.DNN.TextDetectionModelDB.t()`
  - **frame**: `Evision.Mat`

  ##### Return
  - **outs**: `[Evision.Mat]`.

    Allocated output blobs, which will store results of the computation.

  Python prototype (for reference only):
  ```python3
  predict(frame[, outs]) -> outs
  ```
  """
  @spec predict(Evision.DNN.TextDetectionModelDB.t(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: list(Evision.Mat.t()) | {:error, String.t()}
  def predict(self, frame, opts) when (is_struct(frame, Evision.Mat) or is_struct(frame, Nx.Tensor) or is_number(frame) or is_tuple(frame)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      frame: Evision.Internal.Structurise.from_struct(frame)
    ]
    :evision_nif.dnn_dnn_TextDetectionModel_DB_predict(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Given the @p input frame, create input blob, run net and return the output @p blobs.

  ##### Positional Arguments
  - **self**: `Evision.DNN.TextDetectionModelDB.t()`
  - **frame**: `Evision.Mat`

  ##### Return
  - **outs**: `[Evision.Mat]`.

    Allocated output blobs, which will store results of the computation.

  Python prototype (for reference only):
  ```python3
  predict(frame[, outs]) -> outs
  ```
  """
  @spec predict(Evision.DNN.TextDetectionModelDB.t(), Evision.Mat.maybe_mat_in()) :: list(Evision.Mat.t()) | {:error, String.t()}
  def predict(self, frame) when (is_struct(frame, Evision.Mat) or is_struct(frame, Nx.Tensor) or is_number(frame) or is_tuple(frame))
  do
    positional = [
      frame: Evision.Internal.Structurise.from_struct(frame)
    ]
    :evision_nif.dnn_dnn_TextDetectionModel_DB_predict(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setBinaryThreshold

  ##### Positional Arguments
  - **self**: `Evision.DNN.TextDetectionModelDB.t()`
  - **binaryThreshold**: `float`

  ##### Return
  - **retval**: `Evision.DNN.TextDetectionModelDB.t()`

  Python prototype (for reference only):
  ```python3
  setBinaryThreshold(binaryThreshold) -> retval
  ```
  """
  @spec setBinaryThreshold(Evision.DNN.TextDetectionModelDB.t(), number()) :: Evision.DNN.TextDetectionModelDB.t() | {:error, String.t()}
  def setBinaryThreshold(self, binaryThreshold) when is_float(binaryThreshold)
  do
    positional = [
      binaryThreshold: Evision.Internal.Structurise.from_struct(binaryThreshold)
    ]
    :evision_nif.dnn_dnn_TextDetectionModel_DB_setBinaryThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Set flag crop for frame.

  ##### Positional Arguments
  - **self**: `Evision.DNN.TextDetectionModelDB.t()`
  - **crop**: `bool`.

    Flag which indicates whether image will be cropped after resize or not.

  ##### Return
  - **retval**: `Evision.DNN.Model.t()`

  Python prototype (for reference only):
  ```python3
  setInputCrop(crop) -> retval
  ```
  """
  @spec setInputCrop(Evision.DNN.TextDetectionModelDB.t(), boolean()) :: Evision.DNN.Model.t() | {:error, String.t()}
  def setInputCrop(self, crop) when is_boolean(crop)
  do
    positional = [
      crop: Evision.Internal.Structurise.from_struct(crop)
    ]
    :evision_nif.dnn_dnn_TextDetectionModel_DB_setInputCrop(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Set mean value for frame.

  ##### Positional Arguments
  - **self**: `Evision.DNN.TextDetectionModelDB.t()`
  - **mean**: `Evision.scalar()`.

    Scalar with mean values which are subtracted from channels.

  ##### Return
  - **retval**: `Evision.DNN.Model.t()`

  Python prototype (for reference only):
  ```python3
  setInputMean(mean) -> retval
  ```
  """
  @spec setInputMean(Evision.DNN.TextDetectionModelDB.t(), Evision.scalar()) :: Evision.DNN.Model.t() | {:error, String.t()}
  def setInputMean(self, mean) when (is_number(mean) or is_tuple(mean))
  do
    positional = [
      mean: Evision.Internal.Structurise.from_struct(mean)
    ]
    :evision_nif.dnn_dnn_TextDetectionModel_DB_setInputMean(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Set preprocessing parameters for frame.

  ##### Positional Arguments
  - **self**: `Evision.DNN.TextDetectionModelDB.t()`

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
  @spec setInputParams(Evision.DNN.TextDetectionModelDB.t(), [{:crop, term()} | {:mean, term()} | {:scale, term()} | {:size, term()} | {:swapRB, term()}] | nil) :: Evision.DNN.Model.t() | {:error, String.t()}
  def setInputParams(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:crop, :mean, :scale, :size, :swapRB])
    positional = [
    ]
    :evision_nif.dnn_dnn_TextDetectionModel_DB_setInputParams(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Set preprocessing parameters for frame.

  ##### Positional Arguments
  - **self**: `Evision.DNN.TextDetectionModelDB.t()`

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
  @spec setInputParams(Evision.DNN.TextDetectionModelDB.t()) :: Evision.DNN.Model.t() | {:error, String.t()}
  def setInputParams(self) do
    positional = [
    ]
    :evision_nif.dnn_dnn_TextDetectionModel_DB_setInputParams(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Set scalefactor value for frame.

  ##### Positional Arguments
  - **self**: `Evision.DNN.TextDetectionModelDB.t()`
  - **scale**: `Evision.scalar()`.

    Multiplier for frame values.

  ##### Return
  - **retval**: `Evision.DNN.Model.t()`

  Python prototype (for reference only):
  ```python3
  setInputScale(scale) -> retval
  ```
  """
  @spec setInputScale(Evision.DNN.TextDetectionModelDB.t(), Evision.scalar()) :: Evision.DNN.Model.t() | {:error, String.t()}
  def setInputScale(self, scale) when (is_number(scale) or is_tuple(scale))
  do
    positional = [
      scale: Evision.Internal.Structurise.from_struct(scale)
    ]
    :evision_nif.dnn_dnn_TextDetectionModel_DB_setInputScale(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setInputSize

  ##### Positional Arguments
  - **self**: `Evision.DNN.TextDetectionModelDB.t()`
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
  @spec setInputSize(Evision.DNN.TextDetectionModelDB.t(), integer(), integer()) :: Evision.DNN.Model.t() | {:error, String.t()}
  def setInputSize(self, width, height) when is_integer(width) and is_integer(height)
  do
    positional = [
      width: Evision.Internal.Structurise.from_struct(width),
      height: Evision.Internal.Structurise.from_struct(height)
    ]
    :evision_nif.dnn_dnn_TextDetectionModel_DB_setInputSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Set input size for frame.

  ##### Positional Arguments
  - **self**: `Evision.DNN.TextDetectionModelDB.t()`
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
  @spec setInputSize(Evision.DNN.TextDetectionModelDB.t(), {number(), number()}) :: Evision.DNN.Model.t() | {:error, String.t()}
  def setInputSize(self, size) when is_tuple(size)
  do
    positional = [
      size: Evision.Internal.Structurise.from_struct(size)
    ]
    :evision_nif.dnn_dnn_TextDetectionModel_DB_setInputSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Set flag swapRB for frame.

  ##### Positional Arguments
  - **self**: `Evision.DNN.TextDetectionModelDB.t()`
  - **swapRB**: `bool`.

    Flag which indicates that swap first and last channels.

  ##### Return
  - **retval**: `Evision.DNN.Model.t()`

  Python prototype (for reference only):
  ```python3
  setInputSwapRB(swapRB) -> retval
  ```
  """
  @spec setInputSwapRB(Evision.DNN.TextDetectionModelDB.t(), boolean()) :: Evision.DNN.Model.t() | {:error, String.t()}
  def setInputSwapRB(self, swapRB) when is_boolean(swapRB)
  do
    positional = [
      swapRB: Evision.Internal.Structurise.from_struct(swapRB)
    ]
    :evision_nif.dnn_dnn_TextDetectionModel_DB_setInputSwapRB(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMaxCandidates

  ##### Positional Arguments
  - **self**: `Evision.DNN.TextDetectionModelDB.t()`
  - **maxCandidates**: `integer()`

  ##### Return
  - **retval**: `Evision.DNN.TextDetectionModelDB.t()`

  Python prototype (for reference only):
  ```python3
  setMaxCandidates(maxCandidates) -> retval
  ```
  """
  @spec setMaxCandidates(Evision.DNN.TextDetectionModelDB.t(), integer()) :: Evision.DNN.TextDetectionModelDB.t() | {:error, String.t()}
  def setMaxCandidates(self, maxCandidates) when is_integer(maxCandidates)
  do
    positional = [
      maxCandidates: Evision.Internal.Structurise.from_struct(maxCandidates)
    ]
    :evision_nif.dnn_dnn_TextDetectionModel_DB_setMaxCandidates(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Set output names for frame.

  ##### Positional Arguments
  - **self**: `Evision.DNN.TextDetectionModelDB.t()`
  - **outNames**: `[String]`.

    Names for output layers.

  ##### Return
  - **retval**: `Evision.DNN.Model.t()`

  Python prototype (for reference only):
  ```python3
  setOutputNames(outNames) -> retval
  ```
  """
  @spec setOutputNames(Evision.DNN.TextDetectionModelDB.t(), list(binary())) :: Evision.DNN.Model.t() | {:error, String.t()}
  def setOutputNames(self, outNames) when is_list(outNames)
  do
    positional = [
      outNames: Evision.Internal.Structurise.from_struct(outNames)
    ]
    :evision_nif.dnn_dnn_TextDetectionModel_DB_setOutputNames(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setPolygonThreshold

  ##### Positional Arguments
  - **self**: `Evision.DNN.TextDetectionModelDB.t()`
  - **polygonThreshold**: `float`

  ##### Return
  - **retval**: `Evision.DNN.TextDetectionModelDB.t()`

  Python prototype (for reference only):
  ```python3
  setPolygonThreshold(polygonThreshold) -> retval
  ```
  """
  @spec setPolygonThreshold(Evision.DNN.TextDetectionModelDB.t(), number()) :: Evision.DNN.TextDetectionModelDB.t() | {:error, String.t()}
  def setPolygonThreshold(self, polygonThreshold) when is_float(polygonThreshold)
  do
    positional = [
      polygonThreshold: Evision.Internal.Structurise.from_struct(polygonThreshold)
    ]
    :evision_nif.dnn_dnn_TextDetectionModel_DB_setPolygonThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setPreferableBackend

  ##### Positional Arguments
  - **self**: `Evision.DNN.TextDetectionModelDB.t()`
  - **backendId**: `dnn_Backend`

  ##### Return
  - **retval**: `Evision.DNN.Model.t()`

  Python prototype (for reference only):
  ```python3
  setPreferableBackend(backendId) -> retval
  ```
  """
  @spec setPreferableBackend(Evision.DNN.TextDetectionModelDB.t(), Evision.DNN.Backend.enum()) :: Evision.DNN.Model.t() | {:error, String.t()}
  def setPreferableBackend(self, backendId) when is_integer(backendId)
  do
    positional = [
      backendId: Evision.Internal.Structurise.from_struct(backendId)
    ]
    :evision_nif.dnn_dnn_TextDetectionModel_DB_setPreferableBackend(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setPreferableTarget

  ##### Positional Arguments
  - **self**: `Evision.DNN.TextDetectionModelDB.t()`
  - **targetId**: `dnn_Target`

  ##### Return
  - **retval**: `Evision.DNN.Model.t()`

  Python prototype (for reference only):
  ```python3
  setPreferableTarget(targetId) -> retval
  ```
  """
  @spec setPreferableTarget(Evision.DNN.TextDetectionModelDB.t(), Evision.DNN.Target.enum()) :: Evision.DNN.Model.t() | {:error, String.t()}
  def setPreferableTarget(self, targetId) when is_integer(targetId)
  do
    positional = [
      targetId: Evision.Internal.Structurise.from_struct(targetId)
    ]
    :evision_nif.dnn_dnn_TextDetectionModel_DB_setPreferableTarget(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setUnclipRatio

  ##### Positional Arguments
  - **self**: `Evision.DNN.TextDetectionModelDB.t()`
  - **unclipRatio**: `double`

  ##### Return
  - **retval**: `Evision.DNN.TextDetectionModelDB.t()`

  Python prototype (for reference only):
  ```python3
  setUnclipRatio(unclipRatio) -> retval
  ```
  """
  @spec setUnclipRatio(Evision.DNN.TextDetectionModelDB.t(), number()) :: Evision.DNN.TextDetectionModelDB.t() | {:error, String.t()}
  def setUnclipRatio(self, unclipRatio) when is_number(unclipRatio)
  do
    positional = [
      unclipRatio: Evision.Internal.Structurise.from_struct(unclipRatio)
    ]
    :evision_nif.dnn_dnn_TextDetectionModel_DB_setUnclipRatio(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
