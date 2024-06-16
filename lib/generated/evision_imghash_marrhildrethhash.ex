defmodule Evision.ImgHash.MarrHildrethHash do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `ImgHash.MarrHildrethHash` struct.

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
  def to_struct({:ok, %{class: Evision.ImgHash.MarrHildrethHash, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.ImgHash.MarrHildrethHash, ref: ref}) do
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
  ##### Keyword Arguments
  - **alpha**: `float`.

    int scale factor for marr wavelet (default=2).

  - **scale**: `float`.

    int level of scale factor (default = 1)

  ##### Return
  - **retval**: `Evision.ImgHash.MarrHildrethHash.t()`

  Python prototype (for reference only):
  ```python3
  create([, alpha[, scale]]) -> retval
  ```
  """
  @spec create([{:alpha, term()} | {:scale, term()}] | nil) :: Evision.ImgHash.MarrHildrethHash.t() | {:error, String.t()}
  def create(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:alpha, :scale])
    positional = [
    ]
    :evision_nif.img_hash_img_hash_MarrHildrethHash_create_static(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  create
  ##### Keyword Arguments
  - **alpha**: `float`.

    int scale factor for marr wavelet (default=2).

  - **scale**: `float`.

    int level of scale factor (default = 1)

  ##### Return
  - **retval**: `Evision.ImgHash.MarrHildrethHash.t()`

  Python prototype (for reference only):
  ```python3
  create([, alpha[, scale]]) -> retval
  ```
  """
  @spec create() :: Evision.ImgHash.MarrHildrethHash.t() | {:error, String.t()}
  def create() do
    positional = [
    ]
    :evision_nif.img_hash_img_hash_MarrHildrethHash_create_static(positional)
    |> to_struct()
  end

  @doc """
  self explain

  ##### Positional Arguments
  - **self**: `Evision.ImgHash.MarrHildrethHash.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  getAlpha() -> retval
  ```
  """
  @spec getAlpha(Evision.ImgHash.MarrHildrethHash.t()) :: number() | {:error, String.t()}
  def getAlpha(self) do
    positional = [
    ]
    :evision_nif.img_hash_img_hash_MarrHildrethHash_getAlpha(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  self explain

  ##### Positional Arguments
  - **self**: `Evision.ImgHash.MarrHildrethHash.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  getScale() -> retval
  ```
  """
  @spec getScale(Evision.ImgHash.MarrHildrethHash.t()) :: number() | {:error, String.t()}
  def getScale(self) do
    positional = [
    ]
    :evision_nif.img_hash_img_hash_MarrHildrethHash_getScale(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Set Mh kernel parameters

  ##### Positional Arguments
  - **self**: `Evision.ImgHash.MarrHildrethHash.t()`
  - **alpha**: `float`.

    int scale factor for marr wavelet (default=2).

  - **scale**: `float`.

    int level of scale factor (default = 1)

  Python prototype (for reference only):
  ```python3
  setKernelParam(alpha, scale) -> None
  ```
  """
  @spec setKernelParam(Evision.ImgHash.MarrHildrethHash.t(), number(), number()) :: Evision.ImgHash.MarrHildrethHash.t() | {:error, String.t()}
  def setKernelParam(self, alpha, scale) when is_float(alpha) and is_float(scale)
  do
    positional = [
      alpha: Evision.Internal.Structurise.from_struct(alpha),
      scale: Evision.Internal.Structurise.from_struct(scale)
    ]
    :evision_nif.img_hash_img_hash_MarrHildrethHash_setKernelParam(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
