defmodule Evision.DNN.SegmentationModel do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `DNN.SegmentationModel` struct.

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
  def to_struct({:ok, %{class: Evision.DNN.SegmentationModel, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.DNN.SegmentationModel, ref: ref}) do
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
  #### Variant 1:
  Create model from deep learning network.

  ##### Positional Arguments
  - **network**: `Evision.DNN.Net.t()`.

    Net object.

  ##### Return
  - **self**: `Evision.DNN.SegmentationModel.t()`

  Python prototype (for reference only):
  ```python3
  SegmentationModel(network) -> <dnn_SegmentationModel object>
  ```
  #### Variant 2:
  Create segmentation model from network represented in one of the supported formats.
  An order of @p model and @p config arguments does not matter.

  ##### Positional Arguments
  - **model**: `String`.

    Binary file contains trained weights.

  ##### Keyword Arguments
  - **config**: `String`.

    Text file contains network configuration.

  ##### Return
  - **self**: `Evision.DNN.SegmentationModel.t()`

  Python prototype (for reference only):
  ```python3
  SegmentationModel(model[, config]) -> <dnn_SegmentationModel object>
  ```

  """
  @spec segmentationModel(Evision.DNN.Net.t()) :: Evision.DNN.SegmentationModel.t() | {:error, String.t()}
  def segmentationModel(network) when is_struct(network, Evision.DNN.Net)
  do
    positional = [
      network: Evision.Internal.Structurise.from_struct(network)
    ]
    :evision_nif.dnn_dnn_SegmentationModel_SegmentationModel(positional)
    |> to_struct()
  end
  @spec segmentationModel(binary()) :: Evision.DNN.SegmentationModel.t() | {:error, String.t()}
  def segmentationModel(model) when is_binary(model)
  do
    positional = [
      model: Evision.Internal.Structurise.from_struct(model)
    ]
    :evision_nif.dnn_dnn_SegmentationModel_SegmentationModel(positional)
    |> to_struct()
  end

  @doc """
  Create segmentation model from network represented in one of the supported formats.
  An order of @p model and @p config arguments does not matter.

  ##### Positional Arguments
  - **model**: `String`.

    Binary file contains trained weights.

  ##### Keyword Arguments
  - **config**: `String`.

    Text file contains network configuration.

  ##### Return
  - **self**: `Evision.DNN.SegmentationModel.t()`

  Python prototype (for reference only):
  ```python3
  SegmentationModel(model[, config]) -> <dnn_SegmentationModel object>
  ```
  """
  @spec segmentationModel(binary(), [{:config, term()}] | nil) :: Evision.DNN.SegmentationModel.t() | {:error, String.t()}
  def segmentationModel(model, opts) when is_binary(model) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:config])
    positional = [
      model: Evision.Internal.Structurise.from_struct(model)
    ]
    :evision_nif.dnn_dnn_SegmentationModel_SegmentationModel(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  enableWinograd

  ##### Positional Arguments
  - **self**: `Evision.DNN.SegmentationModel.t()`
  - **useWinograd**: `bool`

  ##### Return
  - **retval**: `Evision.DNN.Model.t()`

  Python prototype (for reference only):
  ```python3
  enableWinograd(useWinograd) -> retval
  ```
  """
  @spec enableWinograd(Evision.DNN.SegmentationModel.t(), boolean()) :: Evision.DNN.Model.t() | {:error, String.t()}
  def enableWinograd(self, useWinograd) when is_boolean(useWinograd)
  do
    positional = [
      useWinograd: Evision.Internal.Structurise.from_struct(useWinograd)
    ]
    :evision_nif.dnn_dnn_SegmentationModel_enableWinograd(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Given the @p input frame, create input blob, run net and return the output @p blobs.

  ##### Positional Arguments
  - **self**: `Evision.DNN.SegmentationModel.t()`
  - **frame**: `Evision.Mat`

  ##### Return
  - **outs**: `[Evision.Mat]`.

    Allocated output blobs, which will store results of the computation.

  Python prototype (for reference only):
  ```python3
  predict(frame[, outs]) -> outs
  ```
  """
  @spec predict(Evision.DNN.SegmentationModel.t(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: list(Evision.Mat.t()) | {:error, String.t()}
  def predict(self, frame, opts) when (is_struct(frame, Evision.Mat) or is_struct(frame, Nx.Tensor) or is_number(frame) or is_tuple(frame)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      frame: Evision.Internal.Structurise.from_struct(frame)
    ]
    :evision_nif.dnn_dnn_SegmentationModel_predict(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Given the @p input frame, create input blob, run net and return the output @p blobs.

  ##### Positional Arguments
  - **self**: `Evision.DNN.SegmentationModel.t()`
  - **frame**: `Evision.Mat`

  ##### Return
  - **outs**: `[Evision.Mat]`.

    Allocated output blobs, which will store results of the computation.

  Python prototype (for reference only):
  ```python3
  predict(frame[, outs]) -> outs
  ```
  """
  @spec predict(Evision.DNN.SegmentationModel.t(), Evision.Mat.maybe_mat_in()) :: list(Evision.Mat.t()) | {:error, String.t()}
  def predict(self, frame) when (is_struct(frame, Evision.Mat) or is_struct(frame, Nx.Tensor) or is_number(frame) or is_tuple(frame))
  do
    positional = [
      frame: Evision.Internal.Structurise.from_struct(frame)
    ]
    :evision_nif.dnn_dnn_SegmentationModel_predict(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Given the @p input frame, create input blob, run net

  ##### Positional Arguments
  - **self**: `Evision.DNN.SegmentationModel.t()`
  - **frame**: `Evision.Mat`

  ##### Return
  - **mask**: `Evision.Mat.t()`.

    Allocated class prediction for each pixel

  Python prototype (for reference only):
  ```python3
  segment(frame[, mask]) -> mask
  ```
  """
  @spec segment(Evision.DNN.SegmentationModel.t(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def segment(self, frame, opts) when (is_struct(frame, Evision.Mat) or is_struct(frame, Nx.Tensor) or is_number(frame) or is_tuple(frame)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      frame: Evision.Internal.Structurise.from_struct(frame)
    ]
    :evision_nif.dnn_dnn_SegmentationModel_segment(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Given the @p input frame, create input blob, run net

  ##### Positional Arguments
  - **self**: `Evision.DNN.SegmentationModel.t()`
  - **frame**: `Evision.Mat`

  ##### Return
  - **mask**: `Evision.Mat.t()`.

    Allocated class prediction for each pixel

  Python prototype (for reference only):
  ```python3
  segment(frame[, mask]) -> mask
  ```
  """
  @spec segment(Evision.DNN.SegmentationModel.t(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def segment(self, frame) when (is_struct(frame, Evision.Mat) or is_struct(frame, Nx.Tensor) or is_number(frame) or is_tuple(frame))
  do
    positional = [
      frame: Evision.Internal.Structurise.from_struct(frame)
    ]
    :evision_nif.dnn_dnn_SegmentationModel_segment(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Set flag crop for frame.

  ##### Positional Arguments
  - **self**: `Evision.DNN.SegmentationModel.t()`
  - **crop**: `bool`.

    Flag which indicates whether image will be cropped after resize or not.

  ##### Return
  - **retval**: `Evision.DNN.Model.t()`

  Python prototype (for reference only):
  ```python3
  setInputCrop(crop) -> retval
  ```
  """
  @spec setInputCrop(Evision.DNN.SegmentationModel.t(), boolean()) :: Evision.DNN.Model.t() | {:error, String.t()}
  def setInputCrop(self, crop) when is_boolean(crop)
  do
    positional = [
      crop: Evision.Internal.Structurise.from_struct(crop)
    ]
    :evision_nif.dnn_dnn_SegmentationModel_setInputCrop(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Set mean value for frame.

  ##### Positional Arguments
  - **self**: `Evision.DNN.SegmentationModel.t()`
  - **mean**: `Evision.scalar()`.

    Scalar with mean values which are subtracted from channels.

  ##### Return
  - **retval**: `Evision.DNN.Model.t()`

  Python prototype (for reference only):
  ```python3
  setInputMean(mean) -> retval
  ```
  """
  @spec setInputMean(Evision.DNN.SegmentationModel.t(), Evision.scalar()) :: Evision.DNN.Model.t() | {:error, String.t()}
  def setInputMean(self, mean) when (is_number(mean) or is_tuple(mean))
  do
    positional = [
      mean: Evision.Internal.Structurise.from_struct(mean)
    ]
    :evision_nif.dnn_dnn_SegmentationModel_setInputMean(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Set preprocessing parameters for frame.

  ##### Positional Arguments
  - **self**: `Evision.DNN.SegmentationModel.t()`

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
  @spec setInputParams(Evision.DNN.SegmentationModel.t(), [{:crop, term()} | {:mean, term()} | {:scale, term()} | {:size, term()} | {:swapRB, term()}] | nil) :: Evision.DNN.Model.t() | {:error, String.t()}
  def setInputParams(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:crop, :mean, :scale, :size, :swapRB])
    positional = [
    ]
    :evision_nif.dnn_dnn_SegmentationModel_setInputParams(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Set preprocessing parameters for frame.

  ##### Positional Arguments
  - **self**: `Evision.DNN.SegmentationModel.t()`

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
  @spec setInputParams(Evision.DNN.SegmentationModel.t()) :: Evision.DNN.Model.t() | {:error, String.t()}
  def setInputParams(self) do
    positional = [
    ]
    :evision_nif.dnn_dnn_SegmentationModel_setInputParams(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Set scalefactor value for frame.

  ##### Positional Arguments
  - **self**: `Evision.DNN.SegmentationModel.t()`
  - **scale**: `Evision.scalar()`.

    Multiplier for frame values.

  ##### Return
  - **retval**: `Evision.DNN.Model.t()`

  Python prototype (for reference only):
  ```python3
  setInputScale(scale) -> retval
  ```
  """
  @spec setInputScale(Evision.DNN.SegmentationModel.t(), Evision.scalar()) :: Evision.DNN.Model.t() | {:error, String.t()}
  def setInputScale(self, scale) when (is_number(scale) or is_tuple(scale))
  do
    positional = [
      scale: Evision.Internal.Structurise.from_struct(scale)
    ]
    :evision_nif.dnn_dnn_SegmentationModel_setInputScale(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setInputSize

  ##### Positional Arguments
  - **self**: `Evision.DNN.SegmentationModel.t()`
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
  @spec setInputSize(Evision.DNN.SegmentationModel.t(), integer(), integer()) :: Evision.DNN.Model.t() | {:error, String.t()}
  def setInputSize(self, width, height) when is_integer(width) and is_integer(height)
  do
    positional = [
      width: Evision.Internal.Structurise.from_struct(width),
      height: Evision.Internal.Structurise.from_struct(height)
    ]
    :evision_nif.dnn_dnn_SegmentationModel_setInputSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Set input size for frame.

  ##### Positional Arguments
  - **self**: `Evision.DNN.SegmentationModel.t()`
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
  @spec setInputSize(Evision.DNN.SegmentationModel.t(), {number(), number()}) :: Evision.DNN.Model.t() | {:error, String.t()}
  def setInputSize(self, size) when is_tuple(size)
  do
    positional = [
      size: Evision.Internal.Structurise.from_struct(size)
    ]
    :evision_nif.dnn_dnn_SegmentationModel_setInputSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Set flag swapRB for frame.

  ##### Positional Arguments
  - **self**: `Evision.DNN.SegmentationModel.t()`
  - **swapRB**: `bool`.

    Flag which indicates that swap first and last channels.

  ##### Return
  - **retval**: `Evision.DNN.Model.t()`

  Python prototype (for reference only):
  ```python3
  setInputSwapRB(swapRB) -> retval
  ```
  """
  @spec setInputSwapRB(Evision.DNN.SegmentationModel.t(), boolean()) :: Evision.DNN.Model.t() | {:error, String.t()}
  def setInputSwapRB(self, swapRB) when is_boolean(swapRB)
  do
    positional = [
      swapRB: Evision.Internal.Structurise.from_struct(swapRB)
    ]
    :evision_nif.dnn_dnn_SegmentationModel_setInputSwapRB(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Set output names for frame.

  ##### Positional Arguments
  - **self**: `Evision.DNN.SegmentationModel.t()`
  - **outNames**: `[String]`.

    Names for output layers.

  ##### Return
  - **retval**: `Evision.DNN.Model.t()`

  Python prototype (for reference only):
  ```python3
  setOutputNames(outNames) -> retval
  ```
  """
  @spec setOutputNames(Evision.DNN.SegmentationModel.t(), list(binary())) :: Evision.DNN.Model.t() | {:error, String.t()}
  def setOutputNames(self, outNames) when is_list(outNames)
  do
    positional = [
      outNames: Evision.Internal.Structurise.from_struct(outNames)
    ]
    :evision_nif.dnn_dnn_SegmentationModel_setOutputNames(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setPreferableBackend

  ##### Positional Arguments
  - **self**: `Evision.DNN.SegmentationModel.t()`
  - **backendId**: `dnn_Backend`

  ##### Return
  - **retval**: `Evision.DNN.Model.t()`

  Python prototype (for reference only):
  ```python3
  setPreferableBackend(backendId) -> retval
  ```
  """
  @spec setPreferableBackend(Evision.DNN.SegmentationModel.t(), Evision.DNN.Backend.enum()) :: Evision.DNN.Model.t() | {:error, String.t()}
  def setPreferableBackend(self, backendId) when is_integer(backendId)
  do
    positional = [
      backendId: Evision.Internal.Structurise.from_struct(backendId)
    ]
    :evision_nif.dnn_dnn_SegmentationModel_setPreferableBackend(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setPreferableTarget

  ##### Positional Arguments
  - **self**: `Evision.DNN.SegmentationModel.t()`
  - **targetId**: `dnn_Target`

  ##### Return
  - **retval**: `Evision.DNN.Model.t()`

  Python prototype (for reference only):
  ```python3
  setPreferableTarget(targetId) -> retval
  ```
  """
  @spec setPreferableTarget(Evision.DNN.SegmentationModel.t(), Evision.DNN.Target.enum()) :: Evision.DNN.Model.t() | {:error, String.t()}
  def setPreferableTarget(self, targetId) when is_integer(targetId)
  do
    positional = [
      targetId: Evision.Internal.Structurise.from_struct(targetId)
    ]
    :evision_nif.dnn_dnn_SegmentationModel_setPreferableTarget(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
