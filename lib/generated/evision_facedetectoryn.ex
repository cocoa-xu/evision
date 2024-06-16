defmodule Evision.FaceDetectorYN do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `FaceDetectorYN` struct.

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
  def to_struct({:ok, %{class: Evision.FaceDetectorYN, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.FaceDetectorYN, ref: ref}) do
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
  create

  ##### Positional Arguments
  - **framework**: `String`.

    Name of origin framework

  - **bufferModel**: `[uchar]`.

    A buffer with a content of binary file with weights

  - **bufferConfig**: `[uchar]`.

    A buffer with a content of text file contains network configuration

  - **input_size**: `Size`.

    the size of the input image

  ##### Keyword Arguments
  - **score_threshold**: `float`.

    the threshold to filter out bounding boxes of score smaller than the given value

  - **nms_threshold**: `float`.

    the threshold to suppress bounding boxes of IoU bigger than the given value

  - **top_k**: `integer()`.

    keep top K bboxes before NMS

  - **backend_id**: `integer()`.

    the id of backend

  - **target_id**: `integer()`.

    the id of target device

  ##### Return
  - **retval**: `Evision.FaceDetectorYN.t()`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  create(framework, bufferModel, bufferConfig, input_size[, score_threshold[, nms_threshold[, top_k[, backend_id[, target_id]]]]]) -> retval
  ```
  """
  @spec create(binary(), binary(), binary(), {number(), number()}, [{:backend_id, term()} | {:nms_threshold, term()} | {:score_threshold, term()} | {:target_id, term()} | {:top_k, term()}] | nil) :: Evision.FaceDetectorYN.t() | {:error, String.t()}
  def create(framework, bufferModel, bufferConfig, input_size, opts) when is_binary(framework) and is_binary(bufferModel) and is_binary(bufferConfig) and is_tuple(input_size) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:backend_id, :nms_threshold, :score_threshold, :target_id, :top_k])
    positional = [
      framework: Evision.Internal.Structurise.from_struct(framework),
      bufferModel: Evision.Internal.Structurise.from_struct(bufferModel),
      bufferConfig: Evision.Internal.Structurise.from_struct(bufferConfig),
      input_size: Evision.Internal.Structurise.from_struct(input_size)
    ]
    :evision_nif.faceDetectorYN_create_static(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  create

  ##### Positional Arguments
  - **framework**: `String`.

    Name of origin framework

  - **bufferModel**: `[uchar]`.

    A buffer with a content of binary file with weights

  - **bufferConfig**: `[uchar]`.

    A buffer with a content of text file contains network configuration

  - **input_size**: `Size`.

    the size of the input image

  ##### Keyword Arguments
  - **score_threshold**: `float`.

    the threshold to filter out bounding boxes of score smaller than the given value

  - **nms_threshold**: `float`.

    the threshold to suppress bounding boxes of IoU bigger than the given value

  - **top_k**: `integer()`.

    keep top K bboxes before NMS

  - **backend_id**: `integer()`.

    the id of backend

  - **target_id**: `integer()`.

    the id of target device

  ##### Return
  - **retval**: `Evision.FaceDetectorYN.t()`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  create(framework, bufferModel, bufferConfig, input_size[, score_threshold[, nms_threshold[, top_k[, backend_id[, target_id]]]]]) -> retval
  ```
  #### Variant 2:
  Creates an instance of face detector class with given parameters

  ##### Positional Arguments
  - **model**: `String`.

    the path to the requested model

  - **config**: `String`.

    the path to the config file for compability, which is not requested for ONNX models

  - **input_size**: `Size`.

    the size of the input image

  ##### Keyword Arguments
  - **score_threshold**: `float`.

    the threshold to filter out bounding boxes of score smaller than the given value

  - **nms_threshold**: `float`.

    the threshold to suppress bounding boxes of IoU bigger than the given value

  - **top_k**: `integer()`.

    keep top K bboxes before NMS

  - **backend_id**: `integer()`.

    the id of backend

  - **target_id**: `integer()`.

    the id of target device

  ##### Return
  - **retval**: `Evision.FaceDetectorYN.t()`

  Python prototype (for reference only):
  ```python3
  create(model, config, input_size[, score_threshold[, nms_threshold[, top_k[, backend_id[, target_id]]]]]) -> retval
  ```

  """
  @spec create(binary(), binary(), {number(), number()}, [{:backend_id, term()} | {:nms_threshold, term()} | {:score_threshold, term()} | {:target_id, term()} | {:top_k, term()}] | nil) :: Evision.FaceDetectorYN.t() | {:error, String.t()}
  def create(model, config, input_size, opts) when is_binary(model) and is_binary(config) and is_tuple(input_size) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:backend_id, :nms_threshold, :score_threshold, :target_id, :top_k])
    positional = [
      model: Evision.Internal.Structurise.from_struct(model),
      config: Evision.Internal.Structurise.from_struct(config),
      input_size: Evision.Internal.Structurise.from_struct(input_size)
    ]
    :evision_nif.faceDetectorYN_create_static(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec create(binary(), binary(), binary(), {number(), number()}) :: Evision.FaceDetectorYN.t() | {:error, String.t()}
  def create(framework, bufferModel, bufferConfig, input_size) when is_binary(framework) and is_binary(bufferModel) and is_binary(bufferConfig) and is_tuple(input_size)
  do
    positional = [
      framework: Evision.Internal.Structurise.from_struct(framework),
      bufferModel: Evision.Internal.Structurise.from_struct(bufferModel),
      bufferConfig: Evision.Internal.Structurise.from_struct(bufferConfig),
      input_size: Evision.Internal.Structurise.from_struct(input_size)
    ]
    :evision_nif.faceDetectorYN_create_static(positional)
    |> to_struct()
  end

  @doc """
  Creates an instance of face detector class with given parameters

  ##### Positional Arguments
  - **model**: `String`.

    the path to the requested model

  - **config**: `String`.

    the path to the config file for compability, which is not requested for ONNX models

  - **input_size**: `Size`.

    the size of the input image

  ##### Keyword Arguments
  - **score_threshold**: `float`.

    the threshold to filter out bounding boxes of score smaller than the given value

  - **nms_threshold**: `float`.

    the threshold to suppress bounding boxes of IoU bigger than the given value

  - **top_k**: `integer()`.

    keep top K bboxes before NMS

  - **backend_id**: `integer()`.

    the id of backend

  - **target_id**: `integer()`.

    the id of target device

  ##### Return
  - **retval**: `Evision.FaceDetectorYN.t()`

  Python prototype (for reference only):
  ```python3
  create(model, config, input_size[, score_threshold[, nms_threshold[, top_k[, backend_id[, target_id]]]]]) -> retval
  ```
  """
  @spec create(binary(), binary(), {number(), number()}) :: Evision.FaceDetectorYN.t() | {:error, String.t()}
  def create(model, config, input_size) when is_binary(model) and is_binary(config) and is_tuple(input_size)
  do
    positional = [
      model: Evision.Internal.Structurise.from_struct(model),
      config: Evision.Internal.Structurise.from_struct(config),
      input_size: Evision.Internal.Structurise.from_struct(input_size)
    ]
    :evision_nif.faceDetectorYN_create_static(positional)
    |> to_struct()
  end

  @doc """
  Detects faces in the input image. Following is an example output.

  ##### Positional Arguments
  - **self**: `Evision.FaceDetectorYN.t()`
  - **image**: `Evision.Mat`.

    an image to detect

  ##### Return
  - **retval**: `integer()`
  - **faces**: `Evision.Mat.t()`.

    detection results stored in a 2D cv::Mat of shape [num_faces, 15]
    - 0-1: x, y of bbox top left corner
    - 2-3: width, height of bbox
    - 4-5: x, y of right eye (blue point in the example image)
    - 6-7: x, y of left eye (red point in the example image)
    - 8-9: x, y of nose tip (green point in the example image)
    - 10-11: x, y of right corner of mouth (pink point in the example image)
    - 12-13: x, y of left corner of mouth (yellow point in the example image)
    - 14: face score

   ![image](pics/lena-face-detection.jpg)

  Python prototype (for reference only):
  ```python3
  detect(image[, faces]) -> retval, faces
  ```
  """
  @spec detect(Evision.FaceDetectorYN.t(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: {integer(), Evision.Mat.t()} | {:error, String.t()}
  def detect(self, image, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.faceDetectorYN_detect(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Detects faces in the input image. Following is an example output.

  ##### Positional Arguments
  - **self**: `Evision.FaceDetectorYN.t()`
  - **image**: `Evision.Mat`.

    an image to detect

  ##### Return
  - **retval**: `integer()`
  - **faces**: `Evision.Mat.t()`.

    detection results stored in a 2D cv::Mat of shape [num_faces, 15]
    - 0-1: x, y of bbox top left corner
    - 2-3: width, height of bbox
    - 4-5: x, y of right eye (blue point in the example image)
    - 6-7: x, y of left eye (red point in the example image)
    - 8-9: x, y of nose tip (green point in the example image)
    - 10-11: x, y of right corner of mouth (pink point in the example image)
    - 12-13: x, y of left corner of mouth (yellow point in the example image)
    - 14: face score

   ![image](pics/lena-face-detection.jpg)

  Python prototype (for reference only):
  ```python3
  detect(image[, faces]) -> retval, faces
  ```
  """
  @spec detect(Evision.FaceDetectorYN.t(), Evision.Mat.maybe_mat_in()) :: {integer(), Evision.Mat.t()} | {:error, String.t()}
  def detect(self, image) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.faceDetectorYN_detect(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getInputSize

  ##### Positional Arguments
  - **self**: `Evision.FaceDetectorYN.t()`

  ##### Return
  - **retval**: `Size`

  Python prototype (for reference only):
  ```python3
  getInputSize() -> retval
  ```
  """
  @spec getInputSize(Evision.FaceDetectorYN.t()) :: {number(), number()} | {:error, String.t()}
  def getInputSize(self) do
    positional = [
    ]
    :evision_nif.faceDetectorYN_getInputSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getNMSThreshold

  ##### Positional Arguments
  - **self**: `Evision.FaceDetectorYN.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  getNMSThreshold() -> retval
  ```
  """
  @spec getNMSThreshold(Evision.FaceDetectorYN.t()) :: number() | {:error, String.t()}
  def getNMSThreshold(self) do
    positional = [
    ]
    :evision_nif.faceDetectorYN_getNMSThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getScoreThreshold

  ##### Positional Arguments
  - **self**: `Evision.FaceDetectorYN.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  getScoreThreshold() -> retval
  ```
  """
  @spec getScoreThreshold(Evision.FaceDetectorYN.t()) :: number() | {:error, String.t()}
  def getScoreThreshold(self) do
    positional = [
    ]
    :evision_nif.faceDetectorYN_getScoreThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getTopK

  ##### Positional Arguments
  - **self**: `Evision.FaceDetectorYN.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getTopK() -> retval
  ```
  """
  @spec getTopK(Evision.FaceDetectorYN.t()) :: integer() | {:error, String.t()}
  def getTopK(self) do
    positional = [
    ]
    :evision_nif.faceDetectorYN_getTopK(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Set the size for the network input, which overwrites the input size of creating model. Call this method when the size of input image does not match the input size when creating model

  ##### Positional Arguments
  - **self**: `Evision.FaceDetectorYN.t()`
  - **input_size**: `Size`.

    the size of the input image

  Python prototype (for reference only):
  ```python3
  setInputSize(input_size) -> None
  ```
  """
  @spec setInputSize(Evision.FaceDetectorYN.t(), {number(), number()}) :: Evision.FaceDetectorYN.t() | {:error, String.t()}
  def setInputSize(self, input_size) when is_tuple(input_size)
  do
    positional = [
      input_size: Evision.Internal.Structurise.from_struct(input_size)
    ]
    :evision_nif.faceDetectorYN_setInputSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Set the Non-maximum-suppression threshold to suppress bounding boxes that have IoU greater than the given value

  ##### Positional Arguments
  - **self**: `Evision.FaceDetectorYN.t()`
  - **nms_threshold**: `float`.

    threshold for NMS operation

  Python prototype (for reference only):
  ```python3
  setNMSThreshold(nms_threshold) -> None
  ```
  """
  @spec setNMSThreshold(Evision.FaceDetectorYN.t(), number()) :: Evision.FaceDetectorYN.t() | {:error, String.t()}
  def setNMSThreshold(self, nms_threshold) when is_float(nms_threshold)
  do
    positional = [
      nms_threshold: Evision.Internal.Structurise.from_struct(nms_threshold)
    ]
    :evision_nif.faceDetectorYN_setNMSThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Set the score threshold to filter out bounding boxes of score less than the given value

  ##### Positional Arguments
  - **self**: `Evision.FaceDetectorYN.t()`
  - **score_threshold**: `float`.

    threshold for filtering out bounding boxes

  Python prototype (for reference only):
  ```python3
  setScoreThreshold(score_threshold) -> None
  ```
  """
  @spec setScoreThreshold(Evision.FaceDetectorYN.t(), number()) :: Evision.FaceDetectorYN.t() | {:error, String.t()}
  def setScoreThreshold(self, score_threshold) when is_float(score_threshold)
  do
    positional = [
      score_threshold: Evision.Internal.Structurise.from_struct(score_threshold)
    ]
    :evision_nif.faceDetectorYN_setScoreThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Set the number of bounding boxes preserved before NMS

  ##### Positional Arguments
  - **self**: `Evision.FaceDetectorYN.t()`
  - **top_k**: `integer()`.

    the number of bounding boxes to preserve from top rank based on score

  Python prototype (for reference only):
  ```python3
  setTopK(top_k) -> None
  ```
  """
  @spec setTopK(Evision.FaceDetectorYN.t(), integer()) :: Evision.FaceDetectorYN.t() | {:error, String.t()}
  def setTopK(self, top_k) when is_integer(top_k)
  do
    positional = [
      top_k: Evision.Internal.Structurise.from_struct(top_k)
    ]
    :evision_nif.faceDetectorYN_setTopK(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
