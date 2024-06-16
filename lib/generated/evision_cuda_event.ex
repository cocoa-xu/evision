defmodule Evision.CUDA.Event do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `CUDA.Event` struct.

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
  def to_struct({:ok, %{class: Evision.CUDA.Event, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.CUDA.Event, ref: ref}) do
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
  Event
  ##### Keyword Arguments
  - **flags**: `Event_CreateFlags`.

  ##### Return
  - **self**: `Evision.CUDA.Event.t()`

  Python prototype (for reference only):
  ```python3
  Event([, flags]) -> <cuda_Event object>
  ```
  """
  @spec event([{:flags, term()}] | nil) :: Evision.CUDA.Event.t() | {:error, String.t()}
  def event(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:flags])
    positional = [
    ]
    :evision_nif.cuda_cuda_Event_Event(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Event
  ##### Keyword Arguments
  - **flags**: `Event_CreateFlags`.

  ##### Return
  - **self**: `Evision.CUDA.Event.t()`

  Python prototype (for reference only):
  ```python3
  Event([, flags]) -> <cuda_Event object>
  ```
  """
  @spec event() :: Evision.CUDA.Event.t() | {:error, String.t()}
  def event() do
    positional = [
    ]
    :evision_nif.cuda_cuda_Event_Event(positional)
    |> to_struct()
  end

  @doc """
  elapsedTime

  ##### Positional Arguments
  - **start**: `Evision.CUDA.Event.t()`
  - **end_arg**: `Evision.CUDA.Event.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  elapsedTime(start, end) -> retval
  ```
  """
  @spec elapsedTime(Evision.CUDA.Event.t(), Evision.CUDA.Event.t()) :: number() | {:error, String.t()}
  def elapsedTime(start, end_arg) when is_struct(start, Evision.CUDA.Event) and is_struct(end_arg, Evision.CUDA.Event)
  do
    positional = [
      start: Evision.Internal.Structurise.from_struct(start),
      end_arg: Evision.Internal.Structurise.from_struct(end_arg)
    ]
    :evision_nif.cuda_cuda_Event_elapsedTime_static(positional)
    |> to_struct()
  end

  @doc """
  queryIfComplete

  ##### Positional Arguments
  - **self**: `Evision.CUDA.Event.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  queryIfComplete() -> retval
  ```
  """
  @spec queryIfComplete(Evision.CUDA.Event.t()) :: boolean() | {:error, String.t()}
  def queryIfComplete(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_Event_queryIfComplete(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  record

  ##### Positional Arguments
  - **self**: `Evision.CUDA.Event.t()`

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

  Python prototype (for reference only):
  ```python3
  record([, stream]) -> None
  ```
  """
  @spec record(Evision.CUDA.Event.t(), [{:stream, term()}] | nil) :: Evision.CUDA.Event.t() | {:error, String.t()}
  def record(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
    ]
    :evision_nif.cuda_cuda_Event_record(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  record

  ##### Positional Arguments
  - **self**: `Evision.CUDA.Event.t()`

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

  Python prototype (for reference only):
  ```python3
  record([, stream]) -> None
  ```
  """
  @spec record(Evision.CUDA.Event.t()) :: Evision.CUDA.Event.t() | {:error, String.t()}
  def record(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_Event_record(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  waitForCompletion

  ##### Positional Arguments
  - **self**: `Evision.CUDA.Event.t()`

  Python prototype (for reference only):
  ```python3
  waitForCompletion() -> None
  ```
  """
  @spec waitForCompletion(Evision.CUDA.Event.t()) :: Evision.CUDA.Event.t() | {:error, String.t()}
  def waitForCompletion(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_Event_waitForCompletion(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
