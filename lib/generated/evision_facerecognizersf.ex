defmodule Evision.FaceRecognizerSF do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `FaceRecognizerSF` struct.

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
  def to_struct({:ok, %{class: Evision.FaceRecognizerSF, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.FaceRecognizerSF, ref: ref}) do
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
  Aligning image to put face on the standard position

  ##### Positional Arguments
  - **self**: `Evision.FaceRecognizerSF.t()`
  - **src_img**: `Evision.Mat`.

    input image

  - **face_box**: `Evision.Mat`.

    the detection result used for indicate face in input image

  ##### Return
  - **aligned_img**: `Evision.Mat.t()`.

    output aligned image

  Python prototype (for reference only):
  ```python3
  alignCrop(src_img, face_box[, aligned_img]) -> aligned_img
  ```
  """
  @spec alignCrop(Evision.FaceRecognizerSF.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def alignCrop(self, src_img, face_box, opts) when (is_struct(src_img, Evision.Mat) or is_struct(src_img, Nx.Tensor) or is_number(src_img) or is_tuple(src_img)) and (is_struct(face_box, Evision.Mat) or is_struct(face_box, Nx.Tensor) or is_number(face_box) or is_tuple(face_box)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      src_img: Evision.Internal.Structurise.from_struct(src_img),
      face_box: Evision.Internal.Structurise.from_struct(face_box)
    ]
    :evision_nif.faceRecognizerSF_alignCrop(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Aligning image to put face on the standard position

  ##### Positional Arguments
  - **self**: `Evision.FaceRecognizerSF.t()`
  - **src_img**: `Evision.Mat`.

    input image

  - **face_box**: `Evision.Mat`.

    the detection result used for indicate face in input image

  ##### Return
  - **aligned_img**: `Evision.Mat.t()`.

    output aligned image

  Python prototype (for reference only):
  ```python3
  alignCrop(src_img, face_box[, aligned_img]) -> aligned_img
  ```
  """
  @spec alignCrop(Evision.FaceRecognizerSF.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def alignCrop(self, src_img, face_box) when (is_struct(src_img, Evision.Mat) or is_struct(src_img, Nx.Tensor) or is_number(src_img) or is_tuple(src_img)) and (is_struct(face_box, Evision.Mat) or is_struct(face_box, Nx.Tensor) or is_number(face_box) or is_tuple(face_box))
  do
    positional = [
      src_img: Evision.Internal.Structurise.from_struct(src_img),
      face_box: Evision.Internal.Structurise.from_struct(face_box)
    ]
    :evision_nif.faceRecognizerSF_alignCrop(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Creates an instance of this class with given parameters

  ##### Positional Arguments
  - **model**: `String`.

    the path of the onnx model used for face recognition

  - **config**: `String`.

    the path to the config file for compability, which is not requested for ONNX models

  ##### Keyword Arguments
  - **backend_id**: `integer()`.

    the id of backend

  - **target_id**: `integer()`.

    the id of target device

  ##### Return
  - **retval**: `Evision.FaceRecognizerSF.t()`

  Python prototype (for reference only):
  ```python3
  create(model, config[, backend_id[, target_id]]) -> retval
  ```
  """
  @spec create(binary(), binary(), [{:backend_id, term()} | {:target_id, term()}] | nil) :: Evision.FaceRecognizerSF.t() | {:error, String.t()}
  def create(model, config, opts) when is_binary(model) and is_binary(config) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:backend_id, :target_id])
    positional = [
      model: Evision.Internal.Structurise.from_struct(model),
      config: Evision.Internal.Structurise.from_struct(config)
    ]
    :evision_nif.faceRecognizerSF_create_static(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Creates an instance of this class with given parameters

  ##### Positional Arguments
  - **model**: `String`.

    the path of the onnx model used for face recognition

  - **config**: `String`.

    the path to the config file for compability, which is not requested for ONNX models

  ##### Keyword Arguments
  - **backend_id**: `integer()`.

    the id of backend

  - **target_id**: `integer()`.

    the id of target device

  ##### Return
  - **retval**: `Evision.FaceRecognizerSF.t()`

  Python prototype (for reference only):
  ```python3
  create(model, config[, backend_id[, target_id]]) -> retval
  ```
  """
  @spec create(binary(), binary()) :: Evision.FaceRecognizerSF.t() | {:error, String.t()}
  def create(model, config) when is_binary(model) and is_binary(config)
  do
    positional = [
      model: Evision.Internal.Structurise.from_struct(model),
      config: Evision.Internal.Structurise.from_struct(config)
    ]
    :evision_nif.faceRecognizerSF_create_static(positional)
    |> to_struct()
  end

  @doc """
  Extracting face feature from aligned image

  ##### Positional Arguments
  - **self**: `Evision.FaceRecognizerSF.t()`
  - **aligned_img**: `Evision.Mat`.

    input aligned image

  ##### Return
  - **face_feature**: `Evision.Mat.t()`.

    output face feature

  Python prototype (for reference only):
  ```python3
  feature(aligned_img[, face_feature]) -> face_feature
  ```
  """
  @spec feature(Evision.FaceRecognizerSF.t(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def feature(self, aligned_img, opts) when (is_struct(aligned_img, Evision.Mat) or is_struct(aligned_img, Nx.Tensor) or is_number(aligned_img) or is_tuple(aligned_img)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      aligned_img: Evision.Internal.Structurise.from_struct(aligned_img)
    ]
    :evision_nif.faceRecognizerSF_feature(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Extracting face feature from aligned image

  ##### Positional Arguments
  - **self**: `Evision.FaceRecognizerSF.t()`
  - **aligned_img**: `Evision.Mat`.

    input aligned image

  ##### Return
  - **face_feature**: `Evision.Mat.t()`.

    output face feature

  Python prototype (for reference only):
  ```python3
  feature(aligned_img[, face_feature]) -> face_feature
  ```
  """
  @spec feature(Evision.FaceRecognizerSF.t(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def feature(self, aligned_img) when (is_struct(aligned_img, Evision.Mat) or is_struct(aligned_img, Nx.Tensor) or is_number(aligned_img) or is_tuple(aligned_img))
  do
    positional = [
      aligned_img: Evision.Internal.Structurise.from_struct(aligned_img)
    ]
    :evision_nif.faceRecognizerSF_feature(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Calculating the distance between two face features

  ##### Positional Arguments
  - **self**: `Evision.FaceRecognizerSF.t()`
  - **face_feature1**: `Evision.Mat`.

    the first input feature

  - **face_feature2**: `Evision.Mat`.

    the second input feature of the same size and the same type as face_feature1

  ##### Keyword Arguments
  - **dis_type**: `integer()`.

    defining the similarity with optional values "FR_OSINE" or "FR_NORM_L2"

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  match(face_feature1, face_feature2[, dis_type]) -> retval
  ```
  """
  @spec match(Evision.FaceRecognizerSF.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:dis_type, term()}] | nil) :: number() | {:error, String.t()}
  def match(self, face_feature1, face_feature2, opts) when (is_struct(face_feature1, Evision.Mat) or is_struct(face_feature1, Nx.Tensor) or is_number(face_feature1) or is_tuple(face_feature1)) and (is_struct(face_feature2, Evision.Mat) or is_struct(face_feature2, Nx.Tensor) or is_number(face_feature2) or is_tuple(face_feature2)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:dis_type])
    positional = [
      face_feature1: Evision.Internal.Structurise.from_struct(face_feature1),
      face_feature2: Evision.Internal.Structurise.from_struct(face_feature2)
    ]
    :evision_nif.faceRecognizerSF_match(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Calculating the distance between two face features

  ##### Positional Arguments
  - **self**: `Evision.FaceRecognizerSF.t()`
  - **face_feature1**: `Evision.Mat`.

    the first input feature

  - **face_feature2**: `Evision.Mat`.

    the second input feature of the same size and the same type as face_feature1

  ##### Keyword Arguments
  - **dis_type**: `integer()`.

    defining the similarity with optional values "FR_OSINE" or "FR_NORM_L2"

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  match(face_feature1, face_feature2[, dis_type]) -> retval
  ```
  """
  @spec match(Evision.FaceRecognizerSF.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: number() | {:error, String.t()}
  def match(self, face_feature1, face_feature2) when (is_struct(face_feature1, Evision.Mat) or is_struct(face_feature1, Nx.Tensor) or is_number(face_feature1) or is_tuple(face_feature1)) and (is_struct(face_feature2, Evision.Mat) or is_struct(face_feature2, Nx.Tensor) or is_number(face_feature2) or is_tuple(face_feature2))
  do
    positional = [
      face_feature1: Evision.Internal.Structurise.from_struct(face_feature1),
      face_feature2: Evision.Internal.Structurise.from_struct(face_feature2)
    ]
    :evision_nif.faceRecognizerSF_match(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
