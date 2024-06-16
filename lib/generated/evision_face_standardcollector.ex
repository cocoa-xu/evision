defmodule Evision.Face.StandardCollector do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Face.StandardCollector` struct.

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
  def to_struct({:ok, %{class: Evision.Face.StandardCollector, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Face.StandardCollector, ref: ref}) do
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
  Static constructor
  ##### Keyword Arguments
  - **threshold**: `double`.

    set threshold

  ##### Return
  - **retval**: `StandardCollector`

  Python prototype (for reference only):
  ```python3
  create([, threshold]) -> retval
  ```
  """
  @spec create([{:threshold, term()}] | nil) :: Evision.Face.StandardCollector.t() | {:error, String.t()}
  def create(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:threshold])
    positional = [
    ]
    :evision_nif.face_face_StandardCollector_create_static(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Static constructor
  ##### Keyword Arguments
  - **threshold**: `double`.

    set threshold

  ##### Return
  - **retval**: `StandardCollector`

  Python prototype (for reference only):
  ```python3
  create([, threshold]) -> retval
  ```
  """
  @spec create() :: Evision.Face.StandardCollector.t() | {:error, String.t()}
  def create() do
    positional = [
    ]
    :evision_nif.face_face_StandardCollector_create_static(positional)
    |> to_struct()
  end

  @doc """
  Returns minimal distance value

  ##### Positional Arguments
  - **self**: `Evision.Face.StandardCollector.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getMinDist() -> retval
  ```
  """
  @spec getMinDist(Evision.Face.StandardCollector.t()) :: number() | {:error, String.t()}
  def getMinDist(self) do
    positional = [
    ]
    :evision_nif.face_face_StandardCollector_getMinDist(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns label with minimal distance

  ##### Positional Arguments
  - **self**: `Evision.Face.StandardCollector.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getMinLabel() -> retval
  ```
  """
  @spec getMinLabel(Evision.Face.StandardCollector.t()) :: integer() | {:error, String.t()}
  def getMinLabel(self) do
    positional = [
    ]
    :evision_nif.face_face_StandardCollector_getMinLabel(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Return results as vector

  ##### Positional Arguments
  - **self**: `Evision.Face.StandardCollector.t()`

  ##### Keyword Arguments
  - **sorted**: `bool`.

    If set, results will be sorted by distance
    Each values is a pair of label and distance.

  ##### Return
  - **retval**: `[pair<int, double>]`

  Python prototype (for reference only):
  ```python3
  getResults([, sorted]) -> retval
  ```
  """
  @spec getResults(Evision.Face.StandardCollector.t(), [{:sorted, term()}] | nil) :: list({integer(), number()}) | {:error, String.t()}
  def getResults(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:sorted])
    positional = [
    ]
    :evision_nif.face_face_StandardCollector_getResults(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Return results as vector

  ##### Positional Arguments
  - **self**: `Evision.Face.StandardCollector.t()`

  ##### Keyword Arguments
  - **sorted**: `bool`.

    If set, results will be sorted by distance
    Each values is a pair of label and distance.

  ##### Return
  - **retval**: `[pair<int, double>]`

  Python prototype (for reference only):
  ```python3
  getResults([, sorted]) -> retval
  ```
  """
  @spec getResults(Evision.Face.StandardCollector.t()) :: list({integer(), number()}) | {:error, String.t()}
  def getResults(self) do
    positional = [
    ]
    :evision_nif.face_face_StandardCollector_getResults(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
