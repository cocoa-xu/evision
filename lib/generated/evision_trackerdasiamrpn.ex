defmodule Evision.TrackerDaSiamRPN do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `TrackerDaSiamRPN` struct.

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
  def to_struct({:ok, %{class: Evision.TrackerDaSiamRPN, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.TrackerDaSiamRPN, ref: ref}) do
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
  Constructor
  ##### Keyword Arguments
  - **parameters**: `Evision.TrackerDaSiamRPN.Params`.

    DaSiamRPN parameters TrackerDaSiamRPN::Params

  ##### Return
  - **retval**: `Evision.TrackerDaSiamRPN.t()`

  Python prototype (for reference only):
  ```python3
  create([, parameters]) -> retval
  ```
  """
  @spec create([{:parameters, term()}] | nil) :: Evision.TrackerDaSiamRPN.t() | {:error, String.t()}
  def create(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:parameters])
    positional = [
    ]
    :evision_nif.trackerDaSiamRPN_create_static(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Constructor
  ##### Keyword Arguments
  - **parameters**: `Evision.TrackerDaSiamRPN.Params`.

    DaSiamRPN parameters TrackerDaSiamRPN::Params

  ##### Return
  - **retval**: `Evision.TrackerDaSiamRPN.t()`

  Python prototype (for reference only):
  ```python3
  create([, parameters]) -> retval
  ```
  """
  @spec create() :: Evision.TrackerDaSiamRPN.t() | {:error, String.t()}
  def create() do
    positional = [
    ]
    :evision_nif.trackerDaSiamRPN_create_static(positional)
    |> to_struct()
  end

  @doc """
  Return tracking score

  ##### Positional Arguments
  - **self**: `Evision.TrackerDaSiamRPN.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  getTrackingScore() -> retval
  ```
  """
  @spec getTrackingScore(Evision.TrackerDaSiamRPN.t()) :: number() | {:error, String.t()}
  def getTrackingScore(self) do
    positional = [
    ]
    :evision_nif.trackerDaSiamRPN_getTrackingScore(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
