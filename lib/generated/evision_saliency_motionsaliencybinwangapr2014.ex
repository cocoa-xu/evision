defmodule Evision.Saliency.MotionSaliencyBinWangApr2014 do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Saliency.MotionSaliencyBinWangApr2014` struct.

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
  def to_struct({:ok, %{class: Evision.Saliency.MotionSaliencyBinWangApr2014, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Saliency.MotionSaliencyBinWangApr2014, ref: ref}) do
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
  computeSaliency

  ##### Positional Arguments
  - **self**: `Evision.Saliency.MotionSaliencyBinWangApr2014.t()`
  - **image**: `Evision.Mat`

  ##### Return
  - **retval**: `bool`
  - **saliencyMap**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  computeSaliency(image[, saliencyMap]) -> retval, saliencyMap
  ```
  """
  @spec computeSaliency(Evision.Saliency.MotionSaliencyBinWangApr2014.t(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | false | {:error, String.t()}
  def computeSaliency(self, image, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.saliency_saliency_MotionSaliencyBinWangApr2014_computeSaliency(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  computeSaliency

  ##### Positional Arguments
  - **self**: `Evision.Saliency.MotionSaliencyBinWangApr2014.t()`
  - **image**: `Evision.Mat`

  ##### Return
  - **retval**: `bool`
  - **saliencyMap**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  computeSaliency(image[, saliencyMap]) -> retval, saliencyMap
  ```
  """
  @spec computeSaliency(Evision.Saliency.MotionSaliencyBinWangApr2014.t(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | false | {:error, String.t()}
  def computeSaliency(self, image) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.saliency_saliency_MotionSaliencyBinWangApr2014_computeSaliency(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  create
  ##### Return
  - **retval**: `MotionSaliencyBinWangApr2014`

  Python prototype (for reference only):
  ```python3
  create() -> retval
  ```
  """
  @spec create() :: Evision.Saliency.MotionSaliencyBinWangApr2014.t() | {:error, String.t()}
  def create() do
    positional = [
    ]
    :evision_nif.saliency_saliency_MotionSaliencyBinWangApr2014_create_static(positional)
    |> to_struct()
  end

  @doc """
  getImageHeight

  ##### Positional Arguments
  - **self**: `Evision.Saliency.MotionSaliencyBinWangApr2014.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getImageHeight() -> retval
  ```
  """
  @spec getImageHeight(Evision.Saliency.MotionSaliencyBinWangApr2014.t()) :: integer() | {:error, String.t()}
  def getImageHeight(self) do
    positional = [
    ]
    :evision_nif.saliency_saliency_MotionSaliencyBinWangApr2014_getImageHeight(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getImageWidth

  ##### Positional Arguments
  - **self**: `Evision.Saliency.MotionSaliencyBinWangApr2014.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getImageWidth() -> retval
  ```
  """
  @spec getImageWidth(Evision.Saliency.MotionSaliencyBinWangApr2014.t()) :: integer() | {:error, String.t()}
  def getImageWidth(self) do
    positional = [
    ]
    :evision_nif.saliency_saliency_MotionSaliencyBinWangApr2014_getImageWidth(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  This function allows the correct initialization of all data structures that will be used by the
  algorithm.

  ##### Positional Arguments
  - **self**: `Evision.Saliency.MotionSaliencyBinWangApr2014.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  init() -> retval
  ```
  """
  @spec init(Evision.Saliency.MotionSaliencyBinWangApr2014.t()) :: boolean() | {:error, String.t()}
  def init(self) do
    positional = [
    ]
    :evision_nif.saliency_saliency_MotionSaliencyBinWangApr2014_init(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setImageHeight

  ##### Positional Arguments
  - **self**: `Evision.Saliency.MotionSaliencyBinWangApr2014.t()`
  - **val**: `integer()`

  Python prototype (for reference only):
  ```python3
  setImageHeight(val) -> None
  ```
  """
  @spec setImageHeight(Evision.Saliency.MotionSaliencyBinWangApr2014.t(), integer()) :: Evision.Saliency.MotionSaliencyBinWangApr2014.t() | {:error, String.t()}
  def setImageHeight(self, val) when is_integer(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.saliency_saliency_MotionSaliencyBinWangApr2014_setImageHeight(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setImageWidth

  ##### Positional Arguments
  - **self**: `Evision.Saliency.MotionSaliencyBinWangApr2014.t()`
  - **val**: `integer()`

  Python prototype (for reference only):
  ```python3
  setImageWidth(val) -> None
  ```
  """
  @spec setImageWidth(Evision.Saliency.MotionSaliencyBinWangApr2014.t(), integer()) :: Evision.Saliency.MotionSaliencyBinWangApr2014.t() | {:error, String.t()}
  def setImageWidth(self, val) when is_integer(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.saliency_saliency_MotionSaliencyBinWangApr2014_setImageWidth(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  This is a utility function that allows to set the correct size (taken from the input image) in the
  corresponding variables that will be used to size the data structures of the algorithm.

  ##### Positional Arguments
  - **self**: `Evision.Saliency.MotionSaliencyBinWangApr2014.t()`
  - **w**: `integer()`.

    width of input image

  - **h**: `integer()`.

    height of input image

  Python prototype (for reference only):
  ```python3
  setImagesize(W, H) -> None
  ```
  """
  @spec setImagesize(Evision.Saliency.MotionSaliencyBinWangApr2014.t(), integer(), integer()) :: Evision.Saliency.MotionSaliencyBinWangApr2014.t() | {:error, String.t()}
  def setImagesize(self, w, h) when is_integer(w) and is_integer(h)
  do
    positional = [
      w: Evision.Internal.Structurise.from_struct(w),
      h: Evision.Internal.Structurise.from_struct(h)
    ]
    :evision_nif.saliency_saliency_MotionSaliencyBinWangApr2014_setImagesize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
