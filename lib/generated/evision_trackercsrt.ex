defmodule Evision.TrackerCSRT do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `TrackerCSRT` struct.

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
  def to_struct({:ok, %{class: Evision.TrackerCSRT, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.TrackerCSRT, ref: ref}) do
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
  Create CSRT tracker instance
  ##### Keyword Arguments
  - **parameters**: `Evision.TrackerCSRT.Params`.

    CSRT parameters TrackerCSRT::Params

  ##### Return
  - **retval**: `Evision.TrackerCSRT.t()`

  Python prototype (for reference only):
  ```python3
  create([, parameters]) -> retval
  ```
  """
  @spec create([{:parameters, term()}] | nil) :: Evision.TrackerCSRT.t() | {:error, String.t()}
  def create(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:parameters])
    positional = [
    ]
    :evision_nif.trackerCSRT_create_static(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Create CSRT tracker instance
  ##### Keyword Arguments
  - **parameters**: `Evision.TrackerCSRT.Params`.

    CSRT parameters TrackerCSRT::Params

  ##### Return
  - **retval**: `Evision.TrackerCSRT.t()`

  Python prototype (for reference only):
  ```python3
  create([, parameters]) -> retval
  ```
  """
  @spec create() :: Evision.TrackerCSRT.t() | {:error, String.t()}
  def create() do
    positional = [
    ]
    :evision_nif.trackerCSRT_create_static(positional)
    |> to_struct()
  end

  @doc """
  setInitialMask

  ##### Positional Arguments
  - **self**: `Evision.TrackerCSRT.t()`
  - **mask**: `Evision.Mat`

  Python prototype (for reference only):
  ```python3
  setInitialMask(mask) -> None
  ```
  """
  @spec setInitialMask(Evision.TrackerCSRT.t(), Evision.Mat.maybe_mat_in()) :: Evision.TrackerCSRT.t() | {:error, String.t()}
  def setInitialMask(self, mask) when (is_struct(mask, Evision.Mat) or is_struct(mask, Nx.Tensor) or is_number(mask) or is_tuple(mask))
  do
    positional = [
      mask: Evision.Internal.Structurise.from_struct(mask)
    ]
    :evision_nif.trackerCSRT_setInitialMask(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
