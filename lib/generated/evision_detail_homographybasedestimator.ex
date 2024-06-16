defmodule Evision.Detail.HomographyBasedEstimator do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Detail.HomographyBasedEstimator` struct.

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
  def to_struct({:ok, %{class: Evision.Detail.HomographyBasedEstimator, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Detail.HomographyBasedEstimator, ref: ref}) do
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
  HomographyBasedEstimator
  ##### Keyword Arguments
  - **is_focals_estimated**: `bool`.

  ##### Return
  - **self**: `Evision.Detail.HomographyBasedEstimator.t()`

  Python prototype (for reference only):
  ```python3
  HomographyBasedEstimator([, is_focals_estimated]) -> <detail_HomographyBasedEstimator object>
  ```
  """
  @spec homographyBasedEstimator([{:is_focals_estimated, term()}] | nil) :: Evision.Detail.HomographyBasedEstimator.t() | {:error, String.t()}
  def homographyBasedEstimator(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:is_focals_estimated])
    positional = [
    ]
    :evision_nif.detail_detail_HomographyBasedEstimator_HomographyBasedEstimator(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  HomographyBasedEstimator
  ##### Keyword Arguments
  - **is_focals_estimated**: `bool`.

  ##### Return
  - **self**: `Evision.Detail.HomographyBasedEstimator.t()`

  Python prototype (for reference only):
  ```python3
  HomographyBasedEstimator([, is_focals_estimated]) -> <detail_HomographyBasedEstimator object>
  ```
  """
  @spec homographyBasedEstimator() :: Evision.Detail.HomographyBasedEstimator.t() | {:error, String.t()}
  def homographyBasedEstimator() do
    positional = [
    ]
    :evision_nif.detail_detail_HomographyBasedEstimator_HomographyBasedEstimator(positional)
    |> to_struct()
  end
end
