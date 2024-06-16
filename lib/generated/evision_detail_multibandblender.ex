defmodule Evision.Detail.MultiBandBlender do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Detail.MultiBandBlender` struct.

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
  def to_struct({:ok, %{class: Evision.Detail.MultiBandBlender, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Detail.MultiBandBlender, ref: ref}) do
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
  MultiBandBlender
  ##### Keyword Arguments
  - **try_gpu**: `integer()`.
  - **num_bands**: `integer()`.
  - **weight_type**: `integer()`.

  ##### Return
  - **self**: `Evision.Detail.MultiBandBlender.t()`

  Python prototype (for reference only):
  ```python3
  MultiBandBlender([, try_gpu[, num_bands[, weight_type]]]) -> <detail_MultiBandBlender object>
  ```
  """
  @spec multiBandBlender([{:num_bands, term()} | {:try_gpu, term()} | {:weight_type, term()}] | nil) :: Evision.Detail.MultiBandBlender.t() | {:error, String.t()}
  def multiBandBlender(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:num_bands, :try_gpu, :weight_type])
    positional = [
    ]
    :evision_nif.detail_detail_MultiBandBlender_MultiBandBlender(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  MultiBandBlender
  ##### Keyword Arguments
  - **try_gpu**: `integer()`.
  - **num_bands**: `integer()`.
  - **weight_type**: `integer()`.

  ##### Return
  - **self**: `Evision.Detail.MultiBandBlender.t()`

  Python prototype (for reference only):
  ```python3
  MultiBandBlender([, try_gpu[, num_bands[, weight_type]]]) -> <detail_MultiBandBlender object>
  ```
  """
  @spec multiBandBlender() :: Evision.Detail.MultiBandBlender.t() | {:error, String.t()}
  def multiBandBlender() do
    positional = [
    ]
    :evision_nif.detail_detail_MultiBandBlender_MultiBandBlender(positional)
    |> to_struct()
  end

  @doc """
  blend

  ##### Positional Arguments
  - **self**: `Evision.Detail.MultiBandBlender.t()`

  ##### Return
  - **dst**: `Evision.Mat.t()`
  - **dst_mask**: `Evision.Mat.t()`

  Python prototype (for reference only):
  ```python3
  blend(dst, dst_mask) -> dst, dst_mask
  ```
  """
  @spec blend(Evision.Detail.MultiBandBlender.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: {Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def blend(self, dst, dst_mask) when (is_struct(dst, Evision.Mat) or is_struct(dst, Nx.Tensor) or is_number(dst) or is_tuple(dst)) and (is_struct(dst_mask, Evision.Mat) or is_struct(dst_mask, Nx.Tensor) or is_number(dst_mask) or is_tuple(dst_mask))
  do
    positional = [
      dst: Evision.Internal.Structurise.from_struct(dst),
      dst_mask: Evision.Internal.Structurise.from_struct(dst_mask)
    ]
    :evision_nif.detail_detail_MultiBandBlender_blend(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  feed

  ##### Positional Arguments
  - **self**: `Evision.Detail.MultiBandBlender.t()`
  - **img**: `Evision.Mat`
  - **mask**: `Evision.Mat`
  - **tl**: `Point`

  Python prototype (for reference only):
  ```python3
  feed(img, mask, tl) -> None
  ```
  """
  @spec feed(Evision.Detail.MultiBandBlender.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), {number(), number()}) :: Evision.Detail.MultiBandBlender.t() | {:error, String.t()}
  def feed(self, img, mask, tl) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img)) and (is_struct(mask, Evision.Mat) or is_struct(mask, Nx.Tensor) or is_number(mask) or is_tuple(mask)) and is_tuple(tl)
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img),
      mask: Evision.Internal.Structurise.from_struct(mask),
      tl: Evision.Internal.Structurise.from_struct(tl)
    ]
    :evision_nif.detail_detail_MultiBandBlender_feed(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  numBands

  ##### Positional Arguments
  - **self**: `Evision.Detail.MultiBandBlender.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  numBands() -> retval
  ```
  """
  @spec numBands(Evision.Detail.MultiBandBlender.t()) :: integer() | {:error, String.t()}
  def numBands(self) do
    positional = [
    ]
    :evision_nif.detail_detail_MultiBandBlender_numBands(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  prepare

  ##### Positional Arguments
  - **self**: `Evision.Detail.MultiBandBlender.t()`
  - **dst_roi**: `Rect`

  Python prototype (for reference only):
  ```python3
  prepare(dst_roi) -> None
  ```
  """
  @spec prepare(Evision.Detail.MultiBandBlender.t(), {number(), number(), number(), number()}) :: Evision.Detail.MultiBandBlender.t() | {:error, String.t()}
  def prepare(self, dst_roi) when is_tuple(dst_roi)
  do
    positional = [
      dst_roi: Evision.Internal.Structurise.from_struct(dst_roi)
    ]
    :evision_nif.detail_detail_MultiBandBlender_prepare(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setNumBands

  ##### Positional Arguments
  - **self**: `Evision.Detail.MultiBandBlender.t()`
  - **val**: `integer()`

  Python prototype (for reference only):
  ```python3
  setNumBands(val) -> None
  ```
  """
  @spec setNumBands(Evision.Detail.MultiBandBlender.t(), integer()) :: Evision.Detail.MultiBandBlender.t() | {:error, String.t()}
  def setNumBands(self, val) when is_integer(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.detail_detail_MultiBandBlender_setNumBands(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
