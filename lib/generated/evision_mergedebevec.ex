defmodule Evision.MergeDebevec do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `MergeDebevec` struct.

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
  def to_struct({:ok, %{class: Evision.MergeDebevec, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.MergeDebevec, ref: ref}) do
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
  process

  ##### Positional Arguments
  - **self**: `Evision.MergeDebevec.t()`
  - **src**: `[Evision.Mat]`
  - **times**: `Evision.Mat`
  - **response**: `Evision.Mat`

  ##### Return
  - **dst**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  process(src, times, response[, dst]) -> dst
  ```
  """
  @spec process(Evision.MergeDebevec.t(), list(Evision.Mat.maybe_mat_in()), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def process(self, src, times, response, opts) when is_list(src) and (is_struct(times, Evision.Mat) or is_struct(times, Nx.Tensor) or is_number(times) or is_tuple(times)) and (is_struct(response, Evision.Mat) or is_struct(response, Nx.Tensor) or is_number(response) or is_tuple(response)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      times: Evision.Internal.Structurise.from_struct(times),
      response: Evision.Internal.Structurise.from_struct(response)
    ]
    :evision_nif.mergeDebevec_process(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  process

  ##### Positional Arguments
  - **self**: `Evision.MergeDebevec.t()`
  - **src**: `[Evision.Mat]`
  - **times**: `Evision.Mat`
  - **response**: `Evision.Mat`

  ##### Return
  - **dst**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  process(src, times, response[, dst]) -> dst
  ```
  #### Variant 2:
  process

  ##### Positional Arguments
  - **self**: `Evision.MergeDebevec.t()`
  - **src**: `[Evision.Mat]`
  - **times**: `Evision.Mat`

  ##### Return
  - **dst**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  process(src, times[, dst]) -> dst
  ```

  """
  @spec process(Evision.MergeDebevec.t(), list(Evision.Mat.maybe_mat_in()), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def process(self, src, times, opts) when is_list(src) and (is_struct(times, Evision.Mat) or is_struct(times, Nx.Tensor) or is_number(times) or is_tuple(times)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      times: Evision.Internal.Structurise.from_struct(times)
    ]
    :evision_nif.mergeDebevec_process(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec process(Evision.MergeDebevec.t(), list(Evision.Mat.maybe_mat_in()), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def process(self, src, times, response) when is_list(src) and (is_struct(times, Evision.Mat) or is_struct(times, Nx.Tensor) or is_number(times) or is_tuple(times)) and (is_struct(response, Evision.Mat) or is_struct(response, Nx.Tensor) or is_number(response) or is_tuple(response))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      times: Evision.Internal.Structurise.from_struct(times),
      response: Evision.Internal.Structurise.from_struct(response)
    ]
    :evision_nif.mergeDebevec_process(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  process

  ##### Positional Arguments
  - **self**: `Evision.MergeDebevec.t()`
  - **src**: `[Evision.Mat]`
  - **times**: `Evision.Mat`

  ##### Return
  - **dst**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  process(src, times[, dst]) -> dst
  ```
  """
  @spec process(Evision.MergeDebevec.t(), list(Evision.Mat.maybe_mat_in()), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def process(self, src, times) when is_list(src) and (is_struct(times, Evision.Mat) or is_struct(times, Nx.Tensor) or is_number(times) or is_tuple(times))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      times: Evision.Internal.Structurise.from_struct(times)
    ]
    :evision_nif.mergeDebevec_process(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
