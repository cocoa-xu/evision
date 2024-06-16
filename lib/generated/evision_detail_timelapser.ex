defmodule Evision.Detail.Timelapser do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Detail.Timelapser` struct.

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
  def to_struct({:ok, %{class: Evision.Detail.Timelapser, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Detail.Timelapser, ref: ref}) do
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
  @type enum :: integer()
  @doc enum: true
  def cv_AS_IS, do: 0
  @doc enum: true
  def cv_CROP, do: 1


  @doc """
  createDefault

  ##### Positional Arguments
  - **type**: `integer()`

  ##### Return
  - **retval**: `Evision.Detail.Timelapser.t()`

  Python prototype (for reference only):
  ```python3
  createDefault(type) -> retval
  ```
  """
  @spec createDefault(integer()) :: Evision.Detail.Timelapser.t() | {:error, String.t()}
  def createDefault(type) when is_integer(type)
  do
    positional = [
      type: Evision.Internal.Structurise.from_struct(type)
    ]
    :evision_nif.detail_detail_Timelapser_createDefault_static(positional)
    |> to_struct()
  end

  @doc """
  getDst

  ##### Positional Arguments
  - **self**: `Evision.Detail.Timelapser.t()`

  ##### Return
  - **retval**: `Evision.Mat.t()`

  Python prototype (for reference only):
  ```python3
  getDst() -> retval
  ```
  """
  @spec getDst(Evision.Detail.Timelapser.t()) :: Evision.Mat.t() | {:error, String.t()}
  def getDst(self) do
    positional = [
    ]
    :evision_nif.detail_detail_Timelapser_getDst(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  initialize

  ##### Positional Arguments
  - **self**: `Evision.Detail.Timelapser.t()`
  - **corners**: `[Point]`
  - **sizes**: `[Size]`

  Python prototype (for reference only):
  ```python3
  initialize(corners, sizes) -> None
  ```
  """
  @spec initialize(Evision.Detail.Timelapser.t(), list({number(), number()}), list({number(), number()})) :: Evision.Detail.Timelapser.t() | {:error, String.t()}
  def initialize(self, corners, sizes) when is_list(corners) and is_list(sizes)
  do
    positional = [
      corners: Evision.Internal.Structurise.from_struct(corners),
      sizes: Evision.Internal.Structurise.from_struct(sizes)
    ]
    :evision_nif.detail_detail_Timelapser_initialize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  process

  ##### Positional Arguments
  - **self**: `Evision.Detail.Timelapser.t()`
  - **img**: `Evision.Mat`
  - **mask**: `Evision.Mat`
  - **tl**: `Point`

  Python prototype (for reference only):
  ```python3
  process(img, mask, tl) -> None
  ```
  """
  @spec process(Evision.Detail.Timelapser.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), {number(), number()}) :: Evision.Detail.Timelapser.t() | {:error, String.t()}
  def process(self, img, mask, tl) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img)) and (is_struct(mask, Evision.Mat) or is_struct(mask, Nx.Tensor) or is_number(mask) or is_tuple(mask)) and is_tuple(tl)
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img),
      mask: Evision.Internal.Structurise.from_struct(mask),
      tl: Evision.Internal.Structurise.from_struct(tl)
    ]
    :evision_nif.detail_detail_Timelapser_process(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
