defmodule Evision.WeChatQRCode.WeChatQRCode do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `WeChatQRCode.WeChatQRCode` struct.

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
  def to_struct({:ok, %{class: Evision.WeChatQRCode.WeChatQRCode, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.WeChatQRCode.WeChatQRCode, ref: ref}) do
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
  Initialize the WeChatQRCode.
  It includes two models, which are packaged with caffe format.
  Therefore, there are prototxt and caffe models (In total, four paramenters).

  ##### Keyword Arguments
  - **detector_prototxt_path**: `string`.

    prototxt file path for the detector

  - **detector_caffe_model_path**: `string`.

    caffe model file path for the detector

  - **super_resolution_prototxt_path**: `string`.

    prototxt file path for the super resolution model

  - **super_resolution_caffe_model_path**: `string`.

    caffe file path for the super resolution model

  ##### Return
  - **self**: `Evision.WeChatQRCode.WeChatQRCode.t()`

  Python prototype (for reference only):
  ```python3
  WeChatQRCode([, detector_prototxt_path[, detector_caffe_model_path[, super_resolution_prototxt_path[, super_resolution_caffe_model_path]]]]) -> <wechat_qrcode_WeChatQRCode object>
  ```
  """
  @spec weChatQRCode([{:detector_caffe_model_path, term()} | {:detector_prototxt_path, term()} | {:super_resolution_caffe_model_path, term()} | {:super_resolution_prototxt_path, term()}] | nil) :: Evision.WeChatQRCode.WeChatQRCode.t() | {:error, String.t()}
  def weChatQRCode(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:detector_caffe_model_path, :detector_prototxt_path, :super_resolution_caffe_model_path, :super_resolution_prototxt_path])
    positional = [
    ]
    :evision_nif.wechat_qrcode_wechat_qrcode_WeChatQRCode_WeChatQRCode(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Initialize the WeChatQRCode.
  It includes two models, which are packaged with caffe format.
  Therefore, there are prototxt and caffe models (In total, four paramenters).

  ##### Keyword Arguments
  - **detector_prototxt_path**: `string`.

    prototxt file path for the detector

  - **detector_caffe_model_path**: `string`.

    caffe model file path for the detector

  - **super_resolution_prototxt_path**: `string`.

    prototxt file path for the super resolution model

  - **super_resolution_caffe_model_path**: `string`.

    caffe file path for the super resolution model

  ##### Return
  - **self**: `Evision.WeChatQRCode.WeChatQRCode.t()`

  Python prototype (for reference only):
  ```python3
  WeChatQRCode([, detector_prototxt_path[, detector_caffe_model_path[, super_resolution_prototxt_path[, super_resolution_caffe_model_path]]]]) -> <wechat_qrcode_WeChatQRCode object>
  ```
  """
  @spec weChatQRCode() :: Evision.WeChatQRCode.WeChatQRCode.t() | {:error, String.t()}
  def weChatQRCode() do
    positional = [
    ]
    :evision_nif.wechat_qrcode_wechat_qrcode_WeChatQRCode_WeChatQRCode(positional)
    |> to_struct()
  end

  @doc """
  Both detects and decodes QR code.
  To simplify the usage, there is a only API: detectAndDecode

  ##### Positional Arguments
  - **self**: `Evision.WeChatQRCode.WeChatQRCode.t()`
  - **img**: `Evision.Mat`.

    supports grayscale or color (BGR) image.

  ##### Return
  - **retval**: `[string]`
  - **points**: `[Evision.Mat]`.

    optional output array of vertices of the found QR code quadrangle. Will be
    empty if not found.

  @return list of decoded string.

  Python prototype (for reference only):
  ```python3
  detectAndDecode(img[, points]) -> retval, points
  ```
  """
  @spec detectAndDecode(Evision.WeChatQRCode.WeChatQRCode.t(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: {list(binary()), list(Evision.Mat.t())} | {:error, String.t()}
  def detectAndDecode(self, img, opts) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img)
    ]
    :evision_nif.wechat_qrcode_wechat_qrcode_WeChatQRCode_detectAndDecode(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Both detects and decodes QR code.
  To simplify the usage, there is a only API: detectAndDecode

  ##### Positional Arguments
  - **self**: `Evision.WeChatQRCode.WeChatQRCode.t()`
  - **img**: `Evision.Mat`.

    supports grayscale or color (BGR) image.

  ##### Return
  - **retval**: `[string]`
  - **points**: `[Evision.Mat]`.

    optional output array of vertices of the found QR code quadrangle. Will be
    empty if not found.

  @return list of decoded string.

  Python prototype (for reference only):
  ```python3
  detectAndDecode(img[, points]) -> retval, points
  ```
  """
  @spec detectAndDecode(Evision.WeChatQRCode.WeChatQRCode.t(), Evision.Mat.maybe_mat_in()) :: {list(binary()), list(Evision.Mat.t())} | {:error, String.t()}
  def detectAndDecode(self, img) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img)
    ]
    :evision_nif.wechat_qrcode_wechat_qrcode_WeChatQRCode_detectAndDecode(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getScaleFactor

  ##### Positional Arguments
  - **self**: `Evision.WeChatQRCode.WeChatQRCode.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  getScaleFactor() -> retval
  ```
  """
  @spec getScaleFactor(Evision.WeChatQRCode.WeChatQRCode.t()) :: number() | {:error, String.t()}
  def getScaleFactor(self) do
    positional = [
    ]
    :evision_nif.wechat_qrcode_wechat_qrcode_WeChatQRCode_getScaleFactor(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  set scale factor
  QR code detector use neural network to detect QR.
  Before running the neural network, the input image is pre-processed by scaling.
  By default, the input image is scaled to an image with an area of 160000 pixels.
  The scale factor allows to use custom scale the input image:
  width = scaleFactor*width
  height = scaleFactor*width

  ##### Positional Arguments
  - **self**: `Evision.WeChatQRCode.WeChatQRCode.t()`
  - **scalingFactor**: `float`

   scaleFactor valuse must be > 0 and <= 1, otherwise the scaleFactor value is set to -1
   and use default scaled to an image with an area of 160000 pixels.

  Python prototype (for reference only):
  ```python3
  setScaleFactor(_scalingFactor) -> None
  ```
  """
  @spec setScaleFactor(Evision.WeChatQRCode.WeChatQRCode.t(), number()) :: Evision.WeChatQRCode.WeChatQRCode.t() | {:error, String.t()}
  def setScaleFactor(self, scalingFactor) when is_float(scalingFactor)
  do
    positional = [
      scalingFactor: Evision.Internal.Structurise.from_struct(scalingFactor)
    ]
    :evision_nif.wechat_qrcode_wechat_qrcode_WeChatQRCode_setScaleFactor(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
