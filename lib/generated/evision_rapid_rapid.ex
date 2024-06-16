defmodule Evision.Rapid.Rapid do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Rapid.Rapid` struct.

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
  def to_struct({:ok, %{class: Evision.Rapid.Rapid, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Rapid.Rapid, ref: ref}) do
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
  create

  ##### Positional Arguments
  - **pts3d**: `Evision.Mat`
  - **tris**: `Evision.Mat`

  ##### Return
  - **retval**: `Rapid`

  Python prototype (for reference only):
  ```python3
  create(pts3d, tris) -> retval
  ```
  """
  @spec create(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.Rapid.Rapid.t() | {:error, String.t()}
  def create(pts3d, tris) when (is_struct(pts3d, Evision.Mat) or is_struct(pts3d, Nx.Tensor) or is_number(pts3d) or is_tuple(pts3d)) and (is_struct(tris, Evision.Mat) or is_struct(tris, Nx.Tensor) or is_number(tris) or is_tuple(tris))
  do
    positional = [
      pts3d: Evision.Internal.Structurise.from_struct(pts3d),
      tris: Evision.Internal.Structurise.from_struct(tris)
    ]
    :evision_nif.rapid_rapid_Rapid_create_static(positional)
    |> to_struct()
  end
end
