defmodule Evision.Signal do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Signal` struct.

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
  def to_struct({:ok, %{class: Evision.Signal, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Signal, ref: ref}) do
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
  Signal resampling

  ##### Positional Arguments
  - **inputSignal**: `Evision.Mat`
  - **inFreq**: `integer()`
  - **outFreq**: `integer()`

  ##### Return
  - **outSignal**: `Evision.Mat.t()`.

    Array with output signal

  Detail: https://en.wikipedia.org/wiki/Sample-rate_conversion

  Python prototype (for reference only):
  ```python3
  resampleSignal(inputSignal, inFreq, outFreq[, outSignal]) -> outSignal
  ```
  """
  @spec resampleSignal(Evision.Mat.maybe_mat_in(), integer(), integer(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def resampleSignal(inputSignal, inFreq, outFreq, opts) when (is_struct(inputSignal, Evision.Mat) or is_struct(inputSignal, Nx.Tensor) or is_number(inputSignal) or is_tuple(inputSignal)) and is_integer(inFreq) and is_integer(outFreq) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      inputSignal: Evision.Internal.Structurise.from_struct(inputSignal),
      inFreq: Evision.Internal.Structurise.from_struct(inFreq),
      outFreq: Evision.Internal.Structurise.from_struct(outFreq)
    ]
    :evision_nif.signal_resampleSignal(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Signal resampling

  ##### Positional Arguments
  - **inputSignal**: `Evision.Mat`
  - **inFreq**: `integer()`
  - **outFreq**: `integer()`

  ##### Return
  - **outSignal**: `Evision.Mat.t()`.

    Array with output signal

  Detail: https://en.wikipedia.org/wiki/Sample-rate_conversion

  Python prototype (for reference only):
  ```python3
  resampleSignal(inputSignal, inFreq, outFreq[, outSignal]) -> outSignal
  ```
  """
  @spec resampleSignal(Evision.Mat.maybe_mat_in(), integer(), integer()) :: Evision.Mat.t() | {:error, String.t()}
  def resampleSignal(inputSignal, inFreq, outFreq) when (is_struct(inputSignal, Evision.Mat) or is_struct(inputSignal, Nx.Tensor) or is_number(inputSignal) or is_tuple(inputSignal)) and is_integer(inFreq) and is_integer(outFreq)
  do
    positional = [
      inputSignal: Evision.Internal.Structurise.from_struct(inputSignal),
      inFreq: Evision.Internal.Structurise.from_struct(inFreq),
      outFreq: Evision.Internal.Structurise.from_struct(outFreq)
    ]
    :evision_nif.signal_resampleSignal(positional)
    |> to_struct()
  end
end
