defmodule Evision.ImgHash.BlockMeanHash do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `ImgHash.BlockMeanHash` struct.

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
  def to_struct({:ok, %{class: Evision.ImgHash.BlockMeanHash, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.ImgHash.BlockMeanHash, ref: ref}) do
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
  - **mode**: `integer()`.

  ##### Return
  - **retval**: `BlockMeanHash`

  Python prototype (for reference only):
  ```python3
  create([, mode]) -> retval
  ```
  """
  @spec create([{:mode, term()}] | nil) :: Evision.ImgHash.BlockMeanHash.t() | {:error, String.t()}
  def create(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:mode])
    positional = [
    ]
    :evision_nif.img_hash_img_hash_BlockMeanHash_create_static(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  create
  ##### Keyword Arguments
  - **mode**: `integer()`.

  ##### Return
  - **retval**: `BlockMeanHash`

  Python prototype (for reference only):
  ```python3
  create([, mode]) -> retval
  ```
  """
  @spec create() :: Evision.ImgHash.BlockMeanHash.t() | {:error, String.t()}
  def create() do
    positional = [
    ]
    :evision_nif.img_hash_img_hash_BlockMeanHash_create_static(positional)
    |> to_struct()
  end

  @doc """
  getMean

  ##### Positional Arguments
  - **self**: `Evision.ImgHash.BlockMeanHash.t()`

  ##### Return
  - **retval**: `[double]`

  Python prototype (for reference only):
  ```python3
  getMean() -> retval
  ```
  """
  @spec getMean(Evision.ImgHash.BlockMeanHash.t()) :: list(number()) | {:error, String.t()}
  def getMean(self) do
    positional = [
    ]
    :evision_nif.img_hash_img_hash_BlockMeanHash_getMean(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Create BlockMeanHash object

  ##### Positional Arguments
  - **self**: `Evision.ImgHash.BlockMeanHash.t()`
  - **mode**: `integer()`.

    the mode

  Python prototype (for reference only):
  ```python3
  setMode(mode) -> None
  ```
  """
  @spec setMode(Evision.ImgHash.BlockMeanHash.t(), integer()) :: Evision.ImgHash.BlockMeanHash.t() | {:error, String.t()}
  def setMode(self, mode) when is_integer(mode)
  do
    positional = [
      mode: Evision.Internal.Structurise.from_struct(mode)
    ]
    :evision_nif.img_hash_img_hash_BlockMeanHash_setMode(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
