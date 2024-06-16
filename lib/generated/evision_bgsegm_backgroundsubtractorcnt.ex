defmodule Evision.BgSegm.BackgroundSubtractorCNT do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `BgSegm.BackgroundSubtractorCNT` struct.

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
  def to_struct({:ok, %{class: Evision.BgSegm.BackgroundSubtractorCNT, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.BgSegm.BackgroundSubtractorCNT, ref: ref}) do
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
  apply

  ##### Positional Arguments
  - **self**: `Evision.BgSegm.BackgroundSubtractorCNT.t()`
  - **image**: `Evision.Mat`

  ##### Keyword Arguments
  - **learningRate**: `double`.

  ##### Return
  - **fgmask**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  apply(image[, fgmask[, learningRate]]) -> fgmask
  ```
  """
  @spec apply(Evision.BgSegm.BackgroundSubtractorCNT.t(), Evision.Mat.maybe_mat_in(), [{:learningRate, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def apply(self, image, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:learningRate])
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.bgsegm_bgsegm_BackgroundSubtractorCNT_apply(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  apply

  ##### Positional Arguments
  - **self**: `Evision.BgSegm.BackgroundSubtractorCNT.t()`
  - **image**: `Evision.Mat`

  ##### Keyword Arguments
  - **learningRate**: `double`.

  ##### Return
  - **fgmask**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  apply(image[, fgmask[, learningRate]]) -> fgmask
  ```
  """
  @spec apply(Evision.BgSegm.BackgroundSubtractorCNT.t(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def apply(self, image) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.bgsegm_bgsegm_BackgroundSubtractorCNT_apply(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getBackgroundImage

  ##### Positional Arguments
  - **self**: `Evision.BgSegm.BackgroundSubtractorCNT.t()`

  ##### Return
  - **backgroundImage**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  getBackgroundImage([, backgroundImage]) -> backgroundImage
  ```
  """
  @spec getBackgroundImage(Evision.BgSegm.BackgroundSubtractorCNT.t(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def getBackgroundImage(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    positional = [
    ]
    :evision_nif.bgsegm_bgsegm_BackgroundSubtractorCNT_getBackgroundImage(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  getBackgroundImage

  ##### Positional Arguments
  - **self**: `Evision.BgSegm.BackgroundSubtractorCNT.t()`

  ##### Return
  - **backgroundImage**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  getBackgroundImage([, backgroundImage]) -> backgroundImage
  ```
  """
  @spec getBackgroundImage(Evision.BgSegm.BackgroundSubtractorCNT.t()) :: Evision.Mat.t() | {:error, String.t()}
  def getBackgroundImage(self) do
    positional = [
    ]
    :evision_nif.bgsegm_bgsegm_BackgroundSubtractorCNT_getBackgroundImage(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns if we're parallelizing the algorithm.

  ##### Positional Arguments
  - **self**: `Evision.BgSegm.BackgroundSubtractorCNT.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  getIsParallel() -> retval
  ```
  """
  @spec getIsParallel(Evision.BgSegm.BackgroundSubtractorCNT.t()) :: boolean() | {:error, String.t()}
  def getIsParallel(self) do
    positional = [
    ]
    :evision_nif.bgsegm_bgsegm_BackgroundSubtractorCNT_getIsParallel(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns maximum allowed credit for a pixel in history.

  ##### Positional Arguments
  - **self**: `Evision.BgSegm.BackgroundSubtractorCNT.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getMaxPixelStability() -> retval
  ```
  """
  @spec getMaxPixelStability(Evision.BgSegm.BackgroundSubtractorCNT.t()) :: integer() | {:error, String.t()}
  def getMaxPixelStability(self) do
    positional = [
    ]
    :evision_nif.bgsegm_bgsegm_BackgroundSubtractorCNT_getMaxPixelStability(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns number of frames with same pixel color to consider stable.

  ##### Positional Arguments
  - **self**: `Evision.BgSegm.BackgroundSubtractorCNT.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getMinPixelStability() -> retval
  ```
  """
  @spec getMinPixelStability(Evision.BgSegm.BackgroundSubtractorCNT.t()) :: integer() | {:error, String.t()}
  def getMinPixelStability(self) do
    positional = [
    ]
    :evision_nif.bgsegm_bgsegm_BackgroundSubtractorCNT_getMinPixelStability(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns if we're giving a pixel credit for being stable for a long time.

  ##### Positional Arguments
  - **self**: `Evision.BgSegm.BackgroundSubtractorCNT.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  getUseHistory() -> retval
  ```
  """
  @spec getUseHistory(Evision.BgSegm.BackgroundSubtractorCNT.t()) :: boolean() | {:error, String.t()}
  def getUseHistory(self) do
    positional = [
    ]
    :evision_nif.bgsegm_bgsegm_BackgroundSubtractorCNT_getUseHistory(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Sets if we're parallelizing the algorithm.

  ##### Positional Arguments
  - **self**: `Evision.BgSegm.BackgroundSubtractorCNT.t()`
  - **value**: `bool`

  Python prototype (for reference only):
  ```python3
  setIsParallel(value) -> None
  ```
  """
  @spec setIsParallel(Evision.BgSegm.BackgroundSubtractorCNT.t(), boolean()) :: Evision.BgSegm.BackgroundSubtractorCNT.t() | {:error, String.t()}
  def setIsParallel(self, value) when is_boolean(value)
  do
    positional = [
      value: Evision.Internal.Structurise.from_struct(value)
    ]
    :evision_nif.bgsegm_bgsegm_BackgroundSubtractorCNT_setIsParallel(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Sets the maximum allowed credit for a pixel in history.

  ##### Positional Arguments
  - **self**: `Evision.BgSegm.BackgroundSubtractorCNT.t()`
  - **value**: `integer()`

  Python prototype (for reference only):
  ```python3
  setMaxPixelStability(value) -> None
  ```
  """
  @spec setMaxPixelStability(Evision.BgSegm.BackgroundSubtractorCNT.t(), integer()) :: Evision.BgSegm.BackgroundSubtractorCNT.t() | {:error, String.t()}
  def setMaxPixelStability(self, value) when is_integer(value)
  do
    positional = [
      value: Evision.Internal.Structurise.from_struct(value)
    ]
    :evision_nif.bgsegm_bgsegm_BackgroundSubtractorCNT_setMaxPixelStability(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Sets the number of frames with same pixel color to consider stable.

  ##### Positional Arguments
  - **self**: `Evision.BgSegm.BackgroundSubtractorCNT.t()`
  - **value**: `integer()`

  Python prototype (for reference only):
  ```python3
  setMinPixelStability(value) -> None
  ```
  """
  @spec setMinPixelStability(Evision.BgSegm.BackgroundSubtractorCNT.t(), integer()) :: Evision.BgSegm.BackgroundSubtractorCNT.t() | {:error, String.t()}
  def setMinPixelStability(self, value) when is_integer(value)
  do
    positional = [
      value: Evision.Internal.Structurise.from_struct(value)
    ]
    :evision_nif.bgsegm_bgsegm_BackgroundSubtractorCNT_setMinPixelStability(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Sets if we're giving a pixel credit for being stable for a long time.

  ##### Positional Arguments
  - **self**: `Evision.BgSegm.BackgroundSubtractorCNT.t()`
  - **value**: `bool`

  Python prototype (for reference only):
  ```python3
  setUseHistory(value) -> None
  ```
  """
  @spec setUseHistory(Evision.BgSegm.BackgroundSubtractorCNT.t(), boolean()) :: Evision.BgSegm.BackgroundSubtractorCNT.t() | {:error, String.t()}
  def setUseHistory(self, value) when is_boolean(value)
  do
    positional = [
      value: Evision.Internal.Structurise.from_struct(value)
    ]
    :evision_nif.bgsegm_bgsegm_BackgroundSubtractorCNT_setUseHistory(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
