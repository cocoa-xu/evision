defmodule Evision.TrackerVit do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `TrackerVit` struct.

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
  def to_struct({:ok, %{class: Evision.TrackerVit, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.TrackerVit, ref: ref}) do
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
  - **parameters**: `Evision.TrackerVit.Params`.

    vit tracker parameters TrackerVit::Params

  ##### Return
  - **retval**: `Evision.TrackerVit.t()`

  Python prototype (for reference only):
  ```python3
  create([, parameters]) -> retval
  ```
  """
  @spec create([{:parameters, term()}] | nil) :: Evision.TrackerVit | {:error, String.t()}
  def create(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:parameters])
    positional = [
    ]
    :evision_nif.trackerVit_create_static(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Constructor
  ##### Keyword Arguments
  - **parameters**: `Evision.TrackerVit.Params`.

    vit tracker parameters TrackerVit::Params

  ##### Return
  - **retval**: `Evision.TrackerVit.t()`

  Python prototype (for reference only):
  ```python3
  create([, parameters]) -> retval
  ```
  """
  @spec create() :: Evision.TrackerVit | {:error, String.t()}
  def create() do
    positional = [
    ]
    :evision_nif.trackerVit_create_static(positional)
    |> to_struct()
  end

  @doc """
  Return tracking score

  ##### Positional Arguments
  - **self**: `Evision.TrackerVit.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  getTrackingScore() -> retval
  ```
  """
  @spec getTrackingScore(Evision.TrackerVit.t()) :: number() | {:error, String.t()}
  def getTrackingScore(self) do
    positional = [
    ]
    :evision_nif.trackerVit_getTrackingScore(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
