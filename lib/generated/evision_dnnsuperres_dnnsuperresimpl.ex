defmodule Evision.DNNSuperRes.DNNSuperResImpl do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `DNNSuperRes.DNNSuperResImpl` struct.

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
  def to_struct({:ok, %{class: Evision.DNNSuperRes.DNNSuperResImpl, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.DNNSuperRes.DNNSuperResImpl, ref: ref}) do
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
  Empty constructor for python
  ##### Return
  - **retval**: `Evision.DNNSuperRes.DNNSuperResImpl.t()`

  Python prototype (for reference only):
  ```python3
  create() -> retval
  ```
  """
  @spec create() :: Evision.DNNSuperRes.DNNSuperResImpl.t() | {:error, String.t()}
  def create() do
    positional = [
    ]
    :evision_nif.dnn_superres_dnn_superres_DnnSuperResImpl_create_static(positional)
    |> to_struct()
  end

  @doc """
  Returns the scale factor of the model:

  ##### Positional Arguments
  - **self**: `Evision.DNNSuperRes.DNNSuperResImpl.t()`

  ##### Return
  - **retval**: `String`

  @return Current algorithm.

  Python prototype (for reference only):
  ```python3
  getAlgorithm() -> retval
  ```
  """
  @spec getAlgorithm(Evision.DNNSuperRes.DNNSuperResImpl.t()) :: binary() | {:error, String.t()}
  def getAlgorithm(self) do
    positional = [
    ]
    :evision_nif.dnn_superres_dnn_superres_DnnSuperResImpl_getAlgorithm(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns the scale factor of the model:

  ##### Positional Arguments
  - **self**: `Evision.DNNSuperRes.DNNSuperResImpl.t()`

  ##### Return
  - **retval**: `integer()`

  @return Current scale factor.

  Python prototype (for reference only):
  ```python3
  getScale() -> retval
  ```
  """
  @spec getScale(Evision.DNNSuperRes.DNNSuperResImpl.t()) :: integer() | {:error, String.t()}
  def getScale(self) do
    positional = [
    ]
    :evision_nif.dnn_superres_dnn_superres_DnnSuperResImpl_getScale(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Read the model from the given path

  ##### Positional Arguments
  - **self**: `Evision.DNNSuperRes.DNNSuperResImpl.t()`
  - **path**: `String`.

    Path to the model file.

  Python prototype (for reference only):
  ```python3
  readModel(path) -> None
  ```
  """
  @spec readModel(Evision.DNNSuperRes.DNNSuperResImpl.t(), binary()) :: Evision.DNNSuperRes.DNNSuperResImpl.t() | {:error, String.t()}
  def readModel(self, path) when is_binary(path)
  do
    positional = [
      path: Evision.Internal.Structurise.from_struct(path)
    ]
    :evision_nif.dnn_superres_dnn_superres_DnnSuperResImpl_readModel(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Set desired model

  ##### Positional Arguments
  - **self**: `Evision.DNNSuperRes.DNNSuperResImpl.t()`
  - **algo**: `String`.

    String containing one of the desired models:
    - __edsr__
    - __espcn__
    - __fsrcnn__
    - __lapsrn__

  - **scale**: `integer()`.

    Integer specifying the upscale factor

  Python prototype (for reference only):
  ```python3
  setModel(algo, scale) -> None
  ```
  """
  @spec setModel(Evision.DNNSuperRes.DNNSuperResImpl.t(), binary(), integer()) :: Evision.DNNSuperRes.DNNSuperResImpl.t() | {:error, String.t()}
  def setModel(self, algo, scale) when is_binary(algo) and is_integer(scale)
  do
    positional = [
      algo: Evision.Internal.Structurise.from_struct(algo),
      scale: Evision.Internal.Structurise.from_struct(scale)
    ]
    :evision_nif.dnn_superres_dnn_superres_DnnSuperResImpl_setModel(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Set computation backend

  ##### Positional Arguments
  - **self**: `Evision.DNNSuperRes.DNNSuperResImpl.t()`
  - **backendId**: `integer()`

  Python prototype (for reference only):
  ```python3
  setPreferableBackend(backendId) -> None
  ```
  """
  @spec setPreferableBackend(Evision.DNNSuperRes.DNNSuperResImpl.t(), integer()) :: Evision.DNNSuperRes.DNNSuperResImpl.t() | {:error, String.t()}
  def setPreferableBackend(self, backendId) when is_integer(backendId)
  do
    positional = [
      backendId: Evision.Internal.Structurise.from_struct(backendId)
    ]
    :evision_nif.dnn_superres_dnn_superres_DnnSuperResImpl_setPreferableBackend(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Set computation target

  ##### Positional Arguments
  - **self**: `Evision.DNNSuperRes.DNNSuperResImpl.t()`
  - **targetId**: `integer()`

  Python prototype (for reference only):
  ```python3
  setPreferableTarget(targetId) -> None
  ```
  """
  @spec setPreferableTarget(Evision.DNNSuperRes.DNNSuperResImpl.t(), integer()) :: Evision.DNNSuperRes.DNNSuperResImpl.t() | {:error, String.t()}
  def setPreferableTarget(self, targetId) when is_integer(targetId)
  do
    positional = [
      targetId: Evision.Internal.Structurise.from_struct(targetId)
    ]
    :evision_nif.dnn_superres_dnn_superres_DnnSuperResImpl_setPreferableTarget(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Upsample via neural network

  ##### Positional Arguments
  - **self**: `Evision.DNNSuperRes.DNNSuperResImpl.t()`
  - **img**: `Evision.Mat`.

    Image to upscale

  ##### Return
  - **result**: `Evision.Mat.t()`.

    Destination upscaled image

  Python prototype (for reference only):
  ```python3
  upsample(img[, result]) -> result
  ```
  """
  @spec upsample(Evision.DNNSuperRes.DNNSuperResImpl.t(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def upsample(self, img, opts) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img)
    ]
    :evision_nif.dnn_superres_dnn_superres_DnnSuperResImpl_upsample(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Upsample via neural network

  ##### Positional Arguments
  - **self**: `Evision.DNNSuperRes.DNNSuperResImpl.t()`
  - **img**: `Evision.Mat`.

    Image to upscale

  ##### Return
  - **result**: `Evision.Mat.t()`.

    Destination upscaled image

  Python prototype (for reference only):
  ```python3
  upsample(img[, result]) -> result
  ```
  """
  @spec upsample(Evision.DNNSuperRes.DNNSuperResImpl.t(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def upsample(self, img) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img)
    ]
    :evision_nif.dnn_superres_dnn_superres_DnnSuperResImpl_upsample(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Upsample via neural network of multiple outputs

  ##### Positional Arguments
  - **self**: `Evision.DNNSuperRes.DNNSuperResImpl.t()`
  - **img**: `Evision.Mat`.

    Image to upscale

  - **imgs_new**: `[Evision.Mat]`.

    Destination upscaled images

  - **scale_factors**: `[integer()]`.

    Scaling factors of the output nodes

  - **node_names**: `[String]`.

    Names of the output nodes in the neural network

  Python prototype (for reference only):
  ```python3
  upsampleMultioutput(img, imgs_new, scale_factors, node_names) -> None
  ```
  """
  @spec upsampleMultioutput(Evision.DNNSuperRes.DNNSuperResImpl.t(), Evision.Mat.maybe_mat_in(), list(Evision.Mat.maybe_mat_in()), list(integer()), list(binary())) :: Evision.DNNSuperRes.DNNSuperResImpl.t() | {:error, String.t()}
  def upsampleMultioutput(self, img, imgs_new, scale_factors, node_names) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img)) and is_list(imgs_new) and is_list(scale_factors) and is_list(node_names)
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img),
      imgs_new: Evision.Internal.Structurise.from_struct(imgs_new),
      scale_factors: Evision.Internal.Structurise.from_struct(scale_factors),
      node_names: Evision.Internal.Structurise.from_struct(node_names)
    ]
    :evision_nif.dnn_superres_dnn_superres_DnnSuperResImpl_upsampleMultioutput(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
