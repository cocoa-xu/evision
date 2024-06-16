defmodule Evision.Parallel do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Parallel` struct.

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
  def to_struct({:ok, %{class: Evision.Parallel, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Parallel, ref: ref}) do
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
  Change OpenCV parallel_for backend

  ##### Positional Arguments
  - **backendName**: `string`

  ##### Keyword Arguments
  - **propagateNumThreads**: `bool`.

  ##### Return
  - **retval**: `bool`

  **Note**: This call is not thread-safe. Consider calling this function from the `main()` before any other OpenCV processing functions (and without any other created threads).

  Python prototype (for reference only):
  ```python3
  setParallelForBackend(backendName[, propagateNumThreads]) -> retval
  ```
  """
  @spec setParallelForBackend(binary(), [{:propagateNumThreads, term()}] | nil) :: boolean() | {:error, String.t()}
  def setParallelForBackend(backendName, opts) when is_binary(backendName) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:propagateNumThreads])
    positional = [
      backendName: Evision.Internal.Structurise.from_struct(backendName)
    ]
    :evision_nif.parallel_setParallelForBackend(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Change OpenCV parallel_for backend

  ##### Positional Arguments
  - **backendName**: `string`

  ##### Keyword Arguments
  - **propagateNumThreads**: `bool`.

  ##### Return
  - **retval**: `bool`

  **Note**: This call is not thread-safe. Consider calling this function from the `main()` before any other OpenCV processing functions (and without any other created threads).

  Python prototype (for reference only):
  ```python3
  setParallelForBackend(backendName[, propagateNumThreads]) -> retval
  ```
  """
  @spec setParallelForBackend(binary()) :: boolean() | {:error, String.t()}
  def setParallelForBackend(backendName) when is_binary(backendName)
  do
    positional = [
      backendName: Evision.Internal.Structurise.from_struct(backendName)
    ]
    :evision_nif.parallel_setParallelForBackend(positional)
    |> to_struct()
  end
end
