defmodule Evision.ImgHash.RadialVarianceHash do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `ImgHash.RadialVarianceHash` struct.

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
  def to_struct({:ok, %{class: Evision.ImgHash.RadialVarianceHash, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.ImgHash.RadialVarianceHash, ref: ref}) do
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
  - **sigma**: `double`.
  - **numOfAngleLine**: `integer()`.

  ##### Return
  - **retval**: `Evision.ImgHash.RadialVarianceHash.t()`

  Python prototype (for reference only):
  ```python3
  create([, sigma[, numOfAngleLine]]) -> retval
  ```
  """
  @spec create([{:numOfAngleLine, term()} | {:sigma, term()}] | nil) :: Evision.ImgHash.RadialVarianceHash.t() | {:error, String.t()}
  def create(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:numOfAngleLine, :sigma])
    positional = [
    ]
    :evision_nif.img_hash_img_hash_RadialVarianceHash_create_static(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  create
  ##### Keyword Arguments
  - **sigma**: `double`.
  - **numOfAngleLine**: `integer()`.

  ##### Return
  - **retval**: `Evision.ImgHash.RadialVarianceHash.t()`

  Python prototype (for reference only):
  ```python3
  create([, sigma[, numOfAngleLine]]) -> retval
  ```
  """
  @spec create() :: Evision.ImgHash.RadialVarianceHash.t() | {:error, String.t()}
  def create() do
    positional = [
    ]
    :evision_nif.img_hash_img_hash_RadialVarianceHash_create_static(positional)
    |> to_struct()
  end

  @doc """
  getNumOfAngleLine

  ##### Positional Arguments
  - **self**: `Evision.ImgHash.RadialVarianceHash.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getNumOfAngleLine() -> retval
  ```
  """
  @spec getNumOfAngleLine(Evision.ImgHash.RadialVarianceHash.t()) :: integer() | {:error, String.t()}
  def getNumOfAngleLine(self) do
    positional = [
    ]
    :evision_nif.img_hash_img_hash_RadialVarianceHash_getNumOfAngleLine(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getSigma

  ##### Positional Arguments
  - **self**: `Evision.ImgHash.RadialVarianceHash.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getSigma() -> retval
  ```
  """
  @spec getSigma(Evision.ImgHash.RadialVarianceHash.t()) :: number() | {:error, String.t()}
  def getSigma(self) do
    positional = [
    ]
    :evision_nif.img_hash_img_hash_RadialVarianceHash_getSigma(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setNumOfAngleLine

  ##### Positional Arguments
  - **self**: `Evision.ImgHash.RadialVarianceHash.t()`
  - **value**: `integer()`

  Python prototype (for reference only):
  ```python3
  setNumOfAngleLine(value) -> None
  ```
  """
  @spec setNumOfAngleLine(Evision.ImgHash.RadialVarianceHash.t(), integer()) :: Evision.ImgHash.RadialVarianceHash.t() | {:error, String.t()}
  def setNumOfAngleLine(self, value) when is_integer(value)
  do
    positional = [
      value: Evision.Internal.Structurise.from_struct(value)
    ]
    :evision_nif.img_hash_img_hash_RadialVarianceHash_setNumOfAngleLine(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setSigma

  ##### Positional Arguments
  - **self**: `Evision.ImgHash.RadialVarianceHash.t()`
  - **value**: `double`

  Python prototype (for reference only):
  ```python3
  setSigma(value) -> None
  ```
  """
  @spec setSigma(Evision.ImgHash.RadialVarianceHash.t(), number()) :: Evision.ImgHash.RadialVarianceHash.t() | {:error, String.t()}
  def setSigma(self, value) when is_number(value)
  do
    positional = [
      value: Evision.Internal.Structurise.from_struct(value)
    ]
    :evision_nif.img_hash_img_hash_RadialVarianceHash_setSigma(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
