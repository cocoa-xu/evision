defmodule Evision.Detail.ExposureCompensator do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Detail.ExposureCompensator` struct.

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
  def to_struct({:ok, %{class: Evision.Detail.ExposureCompensator, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Detail.ExposureCompensator, ref: ref}) do
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
  def cv_NO, do: 0
  @doc enum: true
  def cv_GAIN, do: 1
  @doc enum: true
  def cv_GAIN_BLOCKS, do: 2
  @doc enum: true
  def cv_CHANNELS, do: 3
  @doc enum: true
  def cv_CHANNELS_BLOCKS, do: 4


  @doc """
  Compensate exposure in the specified image.

  ##### Positional Arguments
  - **self**: `Evision.Detail.ExposureCompensator.t()`
  - **index**: `integer()`.

    Image index

  - **corner**: `Point`.

    Image top-left corner

  - **mask**: `Evision.Mat`.

    Image mask

  ##### Return
  - **image**: `Evision.Mat.t()`.

    Image to process

  Python prototype (for reference only):
  ```python3
  apply(index, corner, image, mask) -> image
  ```
  """
  @spec apply(Evision.Detail.ExposureCompensator.t(), integer(), {number(), number()}, Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def apply(self, index, corner, image, mask) when is_integer(index) and is_tuple(corner) and (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (is_struct(mask, Evision.Mat) or is_struct(mask, Nx.Tensor) or is_number(mask) or is_tuple(mask))
  do
    positional = [
      index: Evision.Internal.Structurise.from_struct(index),
      corner: Evision.Internal.Structurise.from_struct(corner),
      image: Evision.Internal.Structurise.from_struct(image),
      mask: Evision.Internal.Structurise.from_struct(mask)
    ]
    :evision_nif.detail_detail_ExposureCompensator_apply(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  createDefault

  ##### Positional Arguments
  - **type**: `integer()`

  ##### Return
  - **retval**: `Evision.Detail.ExposureCompensator.t()`

  Python prototype (for reference only):
  ```python3
  createDefault(type) -> retval
  ```
  """
  @spec createDefault(integer()) :: Evision.Detail.ExposureCompensator.t() | {:error, String.t()}
  def createDefault(type) when is_integer(type)
  do
    positional = [
      type: Evision.Internal.Structurise.from_struct(type)
    ]
    :evision_nif.detail_detail_ExposureCompensator_createDefault_static(positional)
    |> to_struct()
  end

  @doc """
  feed

  ##### Positional Arguments
  - **self**: `Evision.Detail.ExposureCompensator.t()`
  - **corners**: `[Point]`.

    Source image top-left corners

  - **images**: `[Evision.Mat]`.

    Source images

  - **masks**: `[Evision.Mat]`.

    Image masks to update (second value in pair specifies the value which should be used
    to detect where image is)

  Python prototype (for reference only):
  ```python3
  feed(corners, images, masks) -> None
  ```
  """
  @spec feed(Evision.Detail.ExposureCompensator.t(), list({number(), number()}), list(Evision.Mat.maybe_mat_in()), list(Evision.Mat.maybe_mat_in())) :: Evision.Detail.ExposureCompensator.t() | {:error, String.t()}
  def feed(self, corners, images, masks) when is_list(corners) and is_list(images) and is_list(masks)
  do
    positional = [
      corners: Evision.Internal.Structurise.from_struct(corners),
      images: Evision.Internal.Structurise.from_struct(images),
      masks: Evision.Internal.Structurise.from_struct(masks)
    ]
    :evision_nif.detail_detail_ExposureCompensator_feed(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMatGains

  ##### Positional Arguments
  - **self**: `Evision.Detail.ExposureCompensator.t()`

  ##### Return
  - **arg1**: `[Evision.Mat]`.

  Python prototype (for reference only):
  ```python3
  getMatGains([, arg1]) -> arg1
  ```
  """
  @spec getMatGains(Evision.Detail.ExposureCompensator.t(), [{atom(), term()},...] | nil) :: list(Evision.Mat.t()) | {:error, String.t()}
  def getMatGains(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    positional = [
    ]
    :evision_nif.detail_detail_ExposureCompensator_getMatGains(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  getMatGains

  ##### Positional Arguments
  - **self**: `Evision.Detail.ExposureCompensator.t()`

  ##### Return
  - **arg1**: `[Evision.Mat]`.

  Python prototype (for reference only):
  ```python3
  getMatGains([, arg1]) -> arg1
  ```
  """
  @spec getMatGains(Evision.Detail.ExposureCompensator.t()) :: list(Evision.Mat.t()) | {:error, String.t()}
  def getMatGains(self) do
    positional = [
    ]
    :evision_nif.detail_detail_ExposureCompensator_getMatGains(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getUpdateGain

  ##### Positional Arguments
  - **self**: `Evision.Detail.ExposureCompensator.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  getUpdateGain() -> retval
  ```
  """
  @spec getUpdateGain(Evision.Detail.ExposureCompensator.t()) :: boolean() | {:error, String.t()}
  def getUpdateGain(self) do
    positional = [
    ]
    :evision_nif.detail_detail_ExposureCompensator_getUpdateGain(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMatGains

  ##### Positional Arguments
  - **self**: `Evision.Detail.ExposureCompensator.t()`
  - **arg1**: `[Evision.Mat]`

  Python prototype (for reference only):
  ```python3
  setMatGains(arg1) -> None
  ```
  """
  @spec setMatGains(Evision.Detail.ExposureCompensator.t(), list(Evision.Mat.maybe_mat_in())) :: Evision.Detail.ExposureCompensator.t() | {:error, String.t()}
  def setMatGains(self, arg1) when is_list(arg1)
  do
    positional = [
      arg1: Evision.Internal.Structurise.from_struct(arg1)
    ]
    :evision_nif.detail_detail_ExposureCompensator_setMatGains(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setUpdateGain

  ##### Positional Arguments
  - **self**: `Evision.Detail.ExposureCompensator.t()`
  - **b**: `bool`

  Python prototype (for reference only):
  ```python3
  setUpdateGain(b) -> None
  ```
  """
  @spec setUpdateGain(Evision.Detail.ExposureCompensator.t(), boolean()) :: Evision.Detail.ExposureCompensator.t() | {:error, String.t()}
  def setUpdateGain(self, b) when is_boolean(b)
  do
    positional = [
      b: Evision.Internal.Structurise.from_struct(b)
    ]
    :evision_nif.detail_detail_ExposureCompensator_setUpdateGain(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
