defmodule Evision.TickMeter do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `TickMeter` struct.

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
  def to_struct({:ok, %{class: Evision.TickMeter, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.TickMeter, ref: ref}) do
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
  TickMeter
  ##### Return
  - **self**: `Evision.TickMeter.t()`

  Python prototype (for reference only):
  ```python3
  TickMeter() -> <TickMeter object>
  ```
  """
  @spec tickMeter() :: Evision.TickMeter.t() | {:error, String.t()}
  def tickMeter() do
    positional = [
    ]
    :evision_nif.tickMeter_TickMeter(positional)
    |> to_struct()
  end

  @doc """
  getAvgTimeMilli

  ##### Positional Arguments
  - **self**: `Evision.TickMeter.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getAvgTimeMilli() -> retval
  ```
  """
  @spec getAvgTimeMilli(Evision.TickMeter.t()) :: number() | {:error, String.t()}
  def getAvgTimeMilli(self) do
    positional = [
    ]
    :evision_nif.tickMeter_getAvgTimeMilli(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getAvgTimeSec

  ##### Positional Arguments
  - **self**: `Evision.TickMeter.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getAvgTimeSec() -> retval
  ```
  """
  @spec getAvgTimeSec(Evision.TickMeter.t()) :: number() | {:error, String.t()}
  def getAvgTimeSec(self) do
    positional = [
    ]
    :evision_nif.tickMeter_getAvgTimeSec(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getCounter

  ##### Positional Arguments
  - **self**: `Evision.TickMeter.t()`

  ##### Return
  - **retval**: `int64`

  Python prototype (for reference only):
  ```python3
  getCounter() -> retval
  ```
  """
  @spec getCounter(Evision.TickMeter.t()) :: integer() | {:error, String.t()}
  def getCounter(self) do
    positional = [
    ]
    :evision_nif.tickMeter_getCounter(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getFPS

  ##### Positional Arguments
  - **self**: `Evision.TickMeter.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getFPS() -> retval
  ```
  """
  @spec getFPS(Evision.TickMeter.t()) :: number() | {:error, String.t()}
  def getFPS(self) do
    positional = [
    ]
    :evision_nif.tickMeter_getFPS(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getTimeMicro

  ##### Positional Arguments
  - **self**: `Evision.TickMeter.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getTimeMicro() -> retval
  ```
  """
  @spec getTimeMicro(Evision.TickMeter.t()) :: number() | {:error, String.t()}
  def getTimeMicro(self) do
    positional = [
    ]
    :evision_nif.tickMeter_getTimeMicro(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getTimeMilli

  ##### Positional Arguments
  - **self**: `Evision.TickMeter.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getTimeMilli() -> retval
  ```
  """
  @spec getTimeMilli(Evision.TickMeter.t()) :: number() | {:error, String.t()}
  def getTimeMilli(self) do
    positional = [
    ]
    :evision_nif.tickMeter_getTimeMilli(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getTimeSec

  ##### Positional Arguments
  - **self**: `Evision.TickMeter.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getTimeSec() -> retval
  ```
  """
  @spec getTimeSec(Evision.TickMeter.t()) :: number() | {:error, String.t()}
  def getTimeSec(self) do
    positional = [
    ]
    :evision_nif.tickMeter_getTimeSec(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getTimeTicks

  ##### Positional Arguments
  - **self**: `Evision.TickMeter.t()`

  ##### Return
  - **retval**: `int64`

  Python prototype (for reference only):
  ```python3
  getTimeTicks() -> retval
  ```
  """
  @spec getTimeTicks(Evision.TickMeter.t()) :: integer() | {:error, String.t()}
  def getTimeTicks(self) do
    positional = [
    ]
    :evision_nif.tickMeter_getTimeTicks(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  reset

  ##### Positional Arguments
  - **self**: `Evision.TickMeter.t()`

  Python prototype (for reference only):
  ```python3
  reset() -> None
  ```
  """
  @spec reset(Evision.TickMeter.t()) :: Evision.TickMeter.t() | {:error, String.t()}
  def reset(self) do
    positional = [
    ]
    :evision_nif.tickMeter_reset(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  start

  ##### Positional Arguments
  - **self**: `Evision.TickMeter.t()`

  Python prototype (for reference only):
  ```python3
  start() -> None
  ```
  """
  @spec start(Evision.TickMeter.t()) :: Evision.TickMeter.t() | {:error, String.t()}
  def start(self) do
    positional = [
    ]
    :evision_nif.tickMeter_start(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  stop

  ##### Positional Arguments
  - **self**: `Evision.TickMeter.t()`

  Python prototype (for reference only):
  ```python3
  stop() -> None
  ```
  """
  @spec stop(Evision.TickMeter.t()) :: Evision.TickMeter.t() | {:error, String.t()}
  def stop(self) do
    positional = [
    ]
    :evision_nif.tickMeter_stop(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
