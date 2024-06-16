defmodule Evision.Detail.GraphCutSeamFinder do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Detail.GraphCutSeamFinder` struct.

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
  def to_struct({:ok, %{class: Evision.Detail.GraphCutSeamFinder, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Detail.GraphCutSeamFinder, ref: ref}) do
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
  GraphCutSeamFinder

  ##### Positional Arguments
  - **cost_type**: `String`

  ##### Keyword Arguments
  - **terminal_cost**: `float`.
  - **bad_region_penalty**: `float`.

  ##### Return
  - **self**: `Evision.Detail.GraphCutSeamFinder.t()`

  Python prototype (for reference only):
  ```python3
  GraphCutSeamFinder(cost_type[, terminal_cost[, bad_region_penalty]]) -> <detail_GraphCutSeamFinder object>
  ```
  """
  @spec graphCutSeamFinder(binary(), [{:bad_region_penalty, term()} | {:terminal_cost, term()}] | nil) :: Evision.Detail.GraphCutSeamFinder.t() | {:error, String.t()}
  def graphCutSeamFinder(cost_type, opts) when is_binary(cost_type) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:bad_region_penalty, :terminal_cost])
    positional = [
      cost_type: Evision.Internal.Structurise.from_struct(cost_type)
    ]
    :evision_nif.detail_detail_GraphCutSeamFinder_GraphCutSeamFinder(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  GraphCutSeamFinder

  ##### Positional Arguments
  - **cost_type**: `String`

  ##### Keyword Arguments
  - **terminal_cost**: `float`.
  - **bad_region_penalty**: `float`.

  ##### Return
  - **self**: `Evision.Detail.GraphCutSeamFinder.t()`

  Python prototype (for reference only):
  ```python3
  GraphCutSeamFinder(cost_type[, terminal_cost[, bad_region_penalty]]) -> <detail_GraphCutSeamFinder object>
  ```
  """
  @spec graphCutSeamFinder(binary()) :: Evision.Detail.GraphCutSeamFinder.t() | {:error, String.t()}
  def graphCutSeamFinder(cost_type) when is_binary(cost_type)
  do
    positional = [
      cost_type: Evision.Internal.Structurise.from_struct(cost_type)
    ]
    :evision_nif.detail_detail_GraphCutSeamFinder_GraphCutSeamFinder(positional)
    |> to_struct()
  end

  @doc """
  find

  ##### Positional Arguments
  - **self**: `Evision.Detail.GraphCutSeamFinder.t()`
  - **src**: `[Evision.Mat]`
  - **corners**: `[Point]`

  ##### Return
  - **masks**: `[Evision.Mat]`

  Python prototype (for reference only):
  ```python3
  find(src, corners, masks) -> masks
  ```
  """
  @spec find(Evision.Detail.GraphCutSeamFinder.t(), list(Evision.Mat.maybe_mat_in()), list({number(), number()}), list(Evision.Mat.maybe_mat_in())) :: list(Evision.Mat.t()) | {:error, String.t()}
  def find(self, src, corners, masks) when is_list(src) and is_list(corners) and is_list(masks)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      corners: Evision.Internal.Structurise.from_struct(corners),
      masks: Evision.Internal.Structurise.from_struct(masks)
    ]
    :evision_nif.detail_detail_GraphCutSeamFinder_find(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
