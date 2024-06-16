defmodule Evision.Detail.NoSeamFinder do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Detail.NoSeamFinder` struct.

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
  def to_struct({:ok, %{class: Evision.Detail.NoSeamFinder, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Detail.NoSeamFinder, ref: ref}) do
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
  find

  ##### Positional Arguments
  - **self**: `Evision.Detail.NoSeamFinder.t()`
  - **arg1**: `[Evision.Mat]`
  - **arg2**: `[Point]`

  ##### Return
  - **arg3**: `[Evision.Mat]`

  Python prototype (for reference only):
  ```python3
  find(arg1, arg2, arg3) -> arg3
  ```
  """
  @spec find(Evision.Detail.NoSeamFinder.t(), list(Evision.Mat.maybe_mat_in()), list({number(), number()}), list(Evision.Mat.maybe_mat_in())) :: list(Evision.Mat.t()) | {:error, String.t()}
  def find(self, arg1, arg2, arg3) when is_list(arg1) and is_list(arg2) and is_list(arg3)
  do
    positional = [
      arg1: Evision.Internal.Structurise.from_struct(arg1),
      arg2: Evision.Internal.Structurise.from_struct(arg2),
      arg3: Evision.Internal.Structurise.from_struct(arg3)
    ]
    :evision_nif.detail_detail_NoSeamFinder_find(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
