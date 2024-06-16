defmodule Evision.NormHistogramCostExtractor do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `NormHistogramCostExtractor` struct.

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
  def to_struct({:ok, %{class: Evision.NormHistogramCostExtractor, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.NormHistogramCostExtractor, ref: ref}) do
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
  getNormFlag

  ##### Positional Arguments
  - **self**: `Evision.NormHistogramCostExtractor.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getNormFlag() -> retval
  ```
  """
  @spec getNormFlag(Evision.NormHistogramCostExtractor.t()) :: integer() | {:error, String.t()}
  def getNormFlag(self) do
    positional = [
    ]
    :evision_nif.normHistogramCostExtractor_getNormFlag(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setNormFlag

  ##### Positional Arguments
  - **self**: `Evision.NormHistogramCostExtractor.t()`
  - **flag**: `integer()`

  Python prototype (for reference only):
  ```python3
  setNormFlag(flag) -> None
  ```
  """
  @spec setNormFlag(Evision.NormHistogramCostExtractor.t(), integer()) :: Evision.NormHistogramCostExtractor.t() | {:error, String.t()}
  def setNormFlag(self, flag) when is_integer(flag)
  do
    positional = [
      flag: Evision.Internal.Structurise.from_struct(flag)
    ]
    :evision_nif.normHistogramCostExtractor_setNormFlag(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
