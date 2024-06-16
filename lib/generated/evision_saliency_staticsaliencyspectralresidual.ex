defmodule Evision.Saliency.StaticSaliencySpectralResidual do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Saliency.StaticSaliencySpectralResidual` struct.

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
  def to_struct({:ok, %{class: Evision.Saliency.StaticSaliencySpectralResidual, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Saliency.StaticSaliencySpectralResidual, ref: ref}) do
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
  - **self**: `Evision.Saliency.StaticSaliencySpectralResidual.t()`
  - **image**: `Evision.Mat`

  ##### Return
  - **retval**: `bool`
  - **saliencyMap**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  computeSaliency(image[, saliencyMap]) -> retval, saliencyMap
  ```
  """
  @spec computeSaliency(Evision.Saliency.StaticSaliencySpectralResidual.t(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | false | {:error, String.t()}
  def computeSaliency(self, image, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.saliency_saliency_StaticSaliencySpectralResidual_computeSaliency(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  computeSaliency

  ##### Positional Arguments
  - **self**: `Evision.Saliency.StaticSaliencySpectralResidual.t()`
  - **image**: `Evision.Mat`

  ##### Return
  - **retval**: `bool`
  - **saliencyMap**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  computeSaliency(image[, saliencyMap]) -> retval, saliencyMap
  ```
  """
  @spec computeSaliency(Evision.Saliency.StaticSaliencySpectralResidual.t(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | false | {:error, String.t()}
  def computeSaliency(self, image) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.saliency_saliency_StaticSaliencySpectralResidual_computeSaliency(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  create
  ##### Return
  - **retval**: `StaticSaliencySpectralResidual`

  Python prototype (for reference only):
  ```python3
  create() -> retval
  ```
  """
  @spec create() :: Evision.Saliency.StaticSaliencySpectralResidual.t() | {:error, String.t()}
  def create() do
    positional = [
    ]
    :evision_nif.saliency_saliency_StaticSaliencySpectralResidual_create_static(positional)
    |> to_struct()
  end

  @doc """
  getImageHeight

  ##### Positional Arguments
  - **self**: `Evision.Saliency.StaticSaliencySpectralResidual.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getImageHeight() -> retval
  ```
  """
  @spec getImageHeight(Evision.Saliency.StaticSaliencySpectralResidual.t()) :: integer() | {:error, String.t()}
  def getImageHeight(self) do
    positional = [
    ]
    :evision_nif.saliency_saliency_StaticSaliencySpectralResidual_getImageHeight(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getImageWidth

  ##### Positional Arguments
  - **self**: `Evision.Saliency.StaticSaliencySpectralResidual.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getImageWidth() -> retval
  ```
  """
  @spec getImageWidth(Evision.Saliency.StaticSaliencySpectralResidual.t()) :: integer() | {:error, String.t()}
  def getImageWidth(self) do
    positional = [
    ]
    :evision_nif.saliency_saliency_StaticSaliencySpectralResidual_getImageWidth(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  read

  ##### Positional Arguments
  - **self**: `Evision.Saliency.StaticSaliencySpectralResidual.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.Saliency.StaticSaliencySpectralResidual.t(), Evision.FileNode.t()) :: Evision.Saliency.StaticSaliencySpectralResidual.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.saliency_saliency_StaticSaliencySpectralResidual_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setImageHeight

  ##### Positional Arguments
  - **self**: `Evision.Saliency.StaticSaliencySpectralResidual.t()`
  - **val**: `integer()`

  Python prototype (for reference only):
  ```python3
  setImageHeight(val) -> None
  ```
  """
  @spec setImageHeight(Evision.Saliency.StaticSaliencySpectralResidual.t(), integer()) :: Evision.Saliency.StaticSaliencySpectralResidual.t() | {:error, String.t()}
  def setImageHeight(self, val) when is_integer(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.saliency_saliency_StaticSaliencySpectralResidual_setImageHeight(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setImageWidth

  ##### Positional Arguments
  - **self**: `Evision.Saliency.StaticSaliencySpectralResidual.t()`
  - **val**: `integer()`

  Python prototype (for reference only):
  ```python3
  setImageWidth(val) -> None
  ```
  """
  @spec setImageWidth(Evision.Saliency.StaticSaliencySpectralResidual.t(), integer()) :: Evision.Saliency.StaticSaliencySpectralResidual.t() | {:error, String.t()}
  def setImageWidth(self, val) when is_integer(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.saliency_saliency_StaticSaliencySpectralResidual_setImageWidth(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
