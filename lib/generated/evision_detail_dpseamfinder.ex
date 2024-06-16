defmodule Evision.Detail.DpSeamFinder do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Detail.DpSeamFinder` struct.

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
  def to_struct({:ok, %{class: Evision.Detail.DpSeamFinder, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Detail.DpSeamFinder, ref: ref}) do
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
  DpSeamFinder

  ##### Positional Arguments
  - **costFunc**: `String`

  ##### Return
  - **self**: `Evision.Detail.DpSeamFinder.t()`

  Python prototype (for reference only):
  ```python3
  DpSeamFinder(costFunc) -> <detail_DpSeamFinder object>
  ```
  """
  @spec dpSeamFinder(binary()) :: Evision.Detail.DpSeamFinder.t() | {:error, String.t()}
  def dpSeamFinder(costFunc) when is_binary(costFunc)
  do
    positional = [
      costFunc: Evision.Internal.Structurise.from_struct(costFunc)
    ]
    :evision_nif.detail_detail_DpSeamFinder_DpSeamFinder(positional)
    |> to_struct()
  end

  @doc """
  setCostFunction

  ##### Positional Arguments
  - **self**: `Evision.Detail.DpSeamFinder.t()`
  - **val**: `String`

  Python prototype (for reference only):
  ```python3
  setCostFunction(val) -> None
  ```
  """
  @spec setCostFunction(Evision.Detail.DpSeamFinder.t(), binary()) :: Evision.Detail.DpSeamFinder.t() | {:error, String.t()}
  def setCostFunction(self, val) when is_binary(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.detail_detail_DpSeamFinder_setCostFunction(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
