defmodule Evision.DNN.Image2BlobParams do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `DNN.Image2BlobParams` struct.

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
  def to_struct({:ok, %{class: Evision.DNN.Image2BlobParams, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.DNN.Image2BlobParams, ref: ref}) do
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
  Image2BlobParams

  ##### Positional Arguments
  - **scalefactor**: `Evision.scalar()`

  ##### Keyword Arguments
  - **size**: `Size`.
  - **mean**: `Evision.scalar()`.
  - **swapRB**: `bool`.
  - **ddepth**: `integer()`.
  - **datalayout**: `DataLayout`.
  - **mode**: `ImagePaddingMode`.
  - **borderValue**: `Evision.scalar()`.

  ##### Return
  - **self**: `Image2BlobParams`

  Python prototype (for reference only):
  ```python3
  Image2BlobParams(scalefactor[, size[, mean[, swapRB[, ddepth[, datalayout[, mode[, borderValue]]]]]]]) -> <dnn_Image2BlobParams object>
  ```
  """
  @spec image2BlobParams(Evision.scalar(), [{:borderValue, term()} | {:datalayout, term()} | {:ddepth, term()} | {:mean, term()} | {:mode, term()} | {:size, term()} | {:swapRB, term()}] | nil) :: Evision.DNN.Image2BlobParams.t() | {:error, String.t()}
  def image2BlobParams(scalefactor, opts) when (is_number(scalefactor) or is_tuple(scalefactor)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:borderValue, :datalayout, :ddepth, :mean, :mode, :size, :swapRB])
    positional = [
      scalefactor: Evision.Internal.Structurise.from_struct(scalefactor)
    ]
    :evision_nif.dnn_dnn_Image2BlobParams_Image2BlobParams(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Image2BlobParams

  ##### Positional Arguments
  - **scalefactor**: `Evision.scalar()`

  ##### Keyword Arguments
  - **size**: `Size`.
  - **mean**: `Evision.scalar()`.
  - **swapRB**: `bool`.
  - **ddepth**: `integer()`.
  - **datalayout**: `DataLayout`.
  - **mode**: `ImagePaddingMode`.
  - **borderValue**: `Evision.scalar()`.

  ##### Return
  - **self**: `Image2BlobParams`

  Python prototype (for reference only):
  ```python3
  Image2BlobParams(scalefactor[, size[, mean[, swapRB[, ddepth[, datalayout[, mode[, borderValue]]]]]]]) -> <dnn_Image2BlobParams object>
  ```
  """
  @spec image2BlobParams(Evision.scalar()) :: Evision.DNN.Image2BlobParams.t() | {:error, String.t()}
  def image2BlobParams(scalefactor) when (is_number(scalefactor) or is_tuple(scalefactor))
  do
    positional = [
      scalefactor: Evision.Internal.Structurise.from_struct(scalefactor)
    ]
    :evision_nif.dnn_dnn_Image2BlobParams_Image2BlobParams(positional)
    |> to_struct()
  end

  @doc """
  Image2BlobParams
  ##### Return
  - **self**: `Image2BlobParams`

  Python prototype (for reference only):
  ```python3
  Image2BlobParams() -> <dnn_Image2BlobParams object>
  ```
  """
  @spec image2BlobParams() :: Evision.DNN.Image2BlobParams.t() | {:error, String.t()}
  def image2BlobParams() do
    positional = [
    ]
    :evision_nif.dnn_dnn_Image2BlobParams_Image2BlobParams(positional)
    |> to_struct()
  end

  @doc """
  Get rectangle coordinates in original image system from rectangle in blob coordinates.

  ##### Positional Arguments
  - **self**: `Evision.DNN.Image2BlobParams.t()`
  - **rBlob**: `Rect`.

    rect in blob coordinates.

  - **size**: `Size`.

    original input image size.

  ##### Return
  - **retval**: `Rect`

  @returns rectangle in original image coordinates.

  Python prototype (for reference only):
  ```python3
  blobRectToImageRect(rBlob, size) -> retval
  ```
  """
  @spec blobRectToImageRect(Evision.DNN.Image2BlobParams.t(), {number(), number(), number(), number()}, {number(), number()}) :: {number(), number(), number(), number()} | {:error, String.t()}
  def blobRectToImageRect(self, rBlob, size) when is_tuple(rBlob) and is_tuple(size)
  do
    positional = [
      rBlob: Evision.Internal.Structurise.from_struct(rBlob),
      size: Evision.Internal.Structurise.from_struct(size)
    ]
    :evision_nif.dnn_dnn_Image2BlobParams_blobRectToImageRect(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Get rectangle coordinates in original image system from rectangle in blob coordinates.

  ##### Positional Arguments
  - **self**: `Evision.DNN.Image2BlobParams.t()`
  - **rBlob**: `[Rect]`.

    rect in blob coordinates.

  - **size**: `Size`.

    original input image size.

  ##### Return
  - **rImg**: `[Rect]`.

    result rect in image coordinates.

  Python prototype (for reference only):
  ```python3
  blobRectsToImageRects(rBlob, size) -> rImg
  ```
  """
  @spec blobRectsToImageRects(Evision.DNN.Image2BlobParams.t(), list({number(), number(), number(), number()}), {number(), number()}) :: list({number(), number(), number(), number()}) | {:error, String.t()}
  def blobRectsToImageRects(self, rBlob, size) when is_list(rBlob) and is_tuple(size)
  do
    positional = [
      rBlob: Evision.Internal.Structurise.from_struct(rBlob),
      size: Evision.Internal.Structurise.from_struct(size)
    ]
    :evision_nif.dnn_dnn_Image2BlobParams_blobRectsToImageRects(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec get_borderValue(Evision.DNN.Image2BlobParams.t()) :: Evision.scalar()
  def get_borderValue(self) do
    :evision_nif.dnn_Image2BlobParams_get_borderValue(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_borderValue(Evision.DNN.Image2BlobParams.t(), Evision.scalar()) :: Evision.DNN.Image2BlobParams.t()
  def set_borderValue(self, prop) do
    :evision_nif.dnn_Image2BlobParams_set_borderValue(
        Evision.Internal.Structurise.from_struct(self),
        [borderValue: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_datalayout(Evision.DNN.Image2BlobParams.t()) :: Evision.DNN.DataLayout.enum()
  def get_datalayout(self) do
    :evision_nif.dnn_Image2BlobParams_get_datalayout(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_datalayout(Evision.DNN.Image2BlobParams.t(), Evision.DNN.DataLayout.enum()) :: Evision.DNN.Image2BlobParams.t()
  def set_datalayout(self, prop) do
    :evision_nif.dnn_Image2BlobParams_set_datalayout(
        Evision.Internal.Structurise.from_struct(self),
        [datalayout: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_ddepth(Evision.DNN.Image2BlobParams.t()) :: integer()
  def get_ddepth(self) do
    :evision_nif.dnn_Image2BlobParams_get_ddepth(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_ddepth(Evision.DNN.Image2BlobParams.t(), integer()) :: Evision.DNN.Image2BlobParams.t()
  def set_ddepth(self, prop) do
    :evision_nif.dnn_Image2BlobParams_set_ddepth(
        Evision.Internal.Structurise.from_struct(self),
        [ddepth: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_mean(Evision.DNN.Image2BlobParams.t()) :: Evision.scalar()
  def get_mean(self) do
    :evision_nif.dnn_Image2BlobParams_get_mean(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_mean(Evision.DNN.Image2BlobParams.t(), Evision.scalar()) :: Evision.DNN.Image2BlobParams.t()
  def set_mean(self, prop) do
    :evision_nif.dnn_Image2BlobParams_set_mean(
        Evision.Internal.Structurise.from_struct(self),
        [mean: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_paddingmode(Evision.DNN.Image2BlobParams.t()) :: Evision.DNN.ImagePaddingMode.enum()
  def get_paddingmode(self) do
    :evision_nif.dnn_Image2BlobParams_get_paddingmode(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_paddingmode(Evision.DNN.Image2BlobParams.t(), Evision.DNN.ImagePaddingMode.enum()) :: Evision.DNN.Image2BlobParams.t()
  def set_paddingmode(self, prop) do
    :evision_nif.dnn_Image2BlobParams_set_paddingmode(
        Evision.Internal.Structurise.from_struct(self),
        [paddingmode: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_scalefactor(Evision.DNN.Image2BlobParams.t()) :: Evision.scalar()
  def get_scalefactor(self) do
    :evision_nif.dnn_Image2BlobParams_get_scalefactor(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_scalefactor(Evision.DNN.Image2BlobParams.t(), Evision.scalar()) :: Evision.DNN.Image2BlobParams.t()
  def set_scalefactor(self, prop) do
    :evision_nif.dnn_Image2BlobParams_set_scalefactor(
        Evision.Internal.Structurise.from_struct(self),
        [scalefactor: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_size(Evision.DNN.Image2BlobParams.t()) :: {number(), number()}
  def get_size(self) do
    :evision_nif.dnn_Image2BlobParams_get_size(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_size(Evision.DNN.Image2BlobParams.t(), {number(), number()}) :: Evision.DNN.Image2BlobParams.t()
  def set_size(self, prop) do
    :evision_nif.dnn_Image2BlobParams_set_size(
        Evision.Internal.Structurise.from_struct(self),
        [size: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_swapRB(Evision.DNN.Image2BlobParams.t()) :: boolean()
  def get_swapRB(self) do
    :evision_nif.dnn_Image2BlobParams_get_swapRB(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_swapRB(Evision.DNN.Image2BlobParams.t(), boolean()) :: Evision.DNN.Image2BlobParams.t()
  def set_swapRB(self, prop) do
    :evision_nif.dnn_Image2BlobParams_set_swapRB(
        Evision.Internal.Structurise.from_struct(self),
        [swapRB: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
end
