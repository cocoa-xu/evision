defmodule Evision.Detail.BlocksGainCompensator do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Detail.BlocksGainCompensator` struct.

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
  def to_struct({:ok, %{class: Evision.Detail.BlocksGainCompensator, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Detail.BlocksGainCompensator, ref: ref}) do
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
  BlocksGainCompensator

  ##### Positional Arguments
  - **bl_width**: `integer()`
  - **bl_height**: `integer()`
  - **nr_feeds**: `integer()`

  ##### Return
  - **self**: `Evision.Detail.BlocksGainCompensator.t()`

  Python prototype (for reference only):
  ```python3
  BlocksGainCompensator(bl_width, bl_height, nr_feeds) -> <detail_BlocksGainCompensator object>
  ```
  """
  @spec blocksGainCompensator(integer(), integer(), integer()) :: Evision.Detail.BlocksGainCompensator.t() | {:error, String.t()}
  def blocksGainCompensator(bl_width, bl_height, nr_feeds) when is_integer(bl_width) and is_integer(bl_height) and is_integer(nr_feeds)
  do
    positional = [
      bl_width: Evision.Internal.Structurise.from_struct(bl_width),
      bl_height: Evision.Internal.Structurise.from_struct(bl_height),
      nr_feeds: Evision.Internal.Structurise.from_struct(nr_feeds)
    ]
    :evision_nif.detail_detail_BlocksGainCompensator_BlocksGainCompensator(positional)
    |> to_struct()
  end

  @doc """
  BlocksGainCompensator
  ##### Keyword Arguments
  - **bl_width**: `integer()`.
  - **bl_height**: `integer()`.

  ##### Return
  - **self**: `Evision.Detail.BlocksGainCompensator.t()`

  Python prototype (for reference only):
  ```python3
  BlocksGainCompensator([, bl_width[, bl_height]]) -> <detail_BlocksGainCompensator object>
  ```
  """
  @spec blocksGainCompensator([{:bl_height, term()} | {:bl_width, term()}] | nil) :: Evision.Detail.BlocksGainCompensator.t() | {:error, String.t()}
  def blocksGainCompensator(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:bl_height, :bl_width])
    positional = [
    ]
    :evision_nif.detail_detail_BlocksGainCompensator_BlocksGainCompensator(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  BlocksGainCompensator
  ##### Keyword Arguments
  - **bl_width**: `integer()`.
  - **bl_height**: `integer()`.

  ##### Return
  - **self**: `Evision.Detail.BlocksGainCompensator.t()`

  Python prototype (for reference only):
  ```python3
  BlocksGainCompensator([, bl_width[, bl_height]]) -> <detail_BlocksGainCompensator object>
  ```
  """
  @spec blocksGainCompensator() :: Evision.Detail.BlocksGainCompensator.t() | {:error, String.t()}
  def blocksGainCompensator() do
    positional = [
    ]
    :evision_nif.detail_detail_BlocksGainCompensator_BlocksGainCompensator(positional)
    |> to_struct()
  end

  @doc """
  apply

  ##### Positional Arguments
  - **self**: `Evision.Detail.BlocksGainCompensator.t()`
  - **index**: `integer()`
  - **corner**: `Point`
  - **mask**: `Evision.Mat`

  ##### Return
  - **image**: `Evision.Mat.t()`

  Python prototype (for reference only):
  ```python3
  apply(index, corner, image, mask) -> image
  ```
  """
  @spec apply(Evision.Detail.BlocksGainCompensator.t(), integer(), {number(), number()}, Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def apply(self, index, corner, image, mask) when is_integer(index) and is_tuple(corner) and (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (is_struct(mask, Evision.Mat) or is_struct(mask, Nx.Tensor) or is_number(mask) or is_tuple(mask))
  do
    positional = [
      index: Evision.Internal.Structurise.from_struct(index),
      corner: Evision.Internal.Structurise.from_struct(corner),
      image: Evision.Internal.Structurise.from_struct(image),
      mask: Evision.Internal.Structurise.from_struct(mask)
    ]
    :evision_nif.detail_detail_BlocksGainCompensator_apply(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMatGains

  ##### Positional Arguments
  - **self**: `Evision.Detail.BlocksGainCompensator.t()`

  ##### Return
  - **umv**: `[Evision.Mat]`.

  Python prototype (for reference only):
  ```python3
  getMatGains([, umv]) -> umv
  ```
  """
  @spec getMatGains(Evision.Detail.BlocksGainCompensator.t(), [{atom(), term()},...] | nil) :: list(Evision.Mat.t()) | {:error, String.t()}
  def getMatGains(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    positional = [
    ]
    :evision_nif.detail_detail_BlocksGainCompensator_getMatGains(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  getMatGains

  ##### Positional Arguments
  - **self**: `Evision.Detail.BlocksGainCompensator.t()`

  ##### Return
  - **umv**: `[Evision.Mat]`.

  Python prototype (for reference only):
  ```python3
  getMatGains([, umv]) -> umv
  ```
  """
  @spec getMatGains(Evision.Detail.BlocksGainCompensator.t()) :: list(Evision.Mat.t()) | {:error, String.t()}
  def getMatGains(self) do
    positional = [
    ]
    :evision_nif.detail_detail_BlocksGainCompensator_getMatGains(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMatGains

  ##### Positional Arguments
  - **self**: `Evision.Detail.BlocksGainCompensator.t()`
  - **umv**: `[Evision.Mat]`

  Python prototype (for reference only):
  ```python3
  setMatGains(umv) -> None
  ```
  """
  @spec setMatGains(Evision.Detail.BlocksGainCompensator.t(), list(Evision.Mat.maybe_mat_in())) :: Evision.Detail.BlocksGainCompensator.t() | {:error, String.t()}
  def setMatGains(self, umv) when is_list(umv)
  do
    positional = [
      umv: Evision.Internal.Structurise.from_struct(umv)
    ]
    :evision_nif.detail_detail_BlocksGainCompensator_setMatGains(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
