defmodule Evision.Rapid.GOSTracker do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Rapid.GOSTracker` struct.

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
  def to_struct({:ok, %{class: Evision.Rapid.GOSTracker, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Rapid.GOSTracker, ref: ref}) do
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

  ##### Keyword Arguments
  - **histBins**: `integer()`.
  - **sobelThesh**: `uchar`.

  ##### Return
  - **retval**: `OLSTracker`

  Python prototype (for reference only):
  ```python3
  create(pts3d, tris[, histBins[, sobelThesh]]) -> retval
  ```
  """
  @spec create(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:histBins, term()} | {:sobelThesh, term()}] | nil) :: Evision.Rapid.OLSTracker.t() | {:error, String.t()}
  def create(pts3d, tris, opts) when (is_struct(pts3d, Evision.Mat) or is_struct(pts3d, Nx.Tensor) or is_number(pts3d) or is_tuple(pts3d)) and (is_struct(tris, Evision.Mat) or is_struct(tris, Nx.Tensor) or is_number(tris) or is_tuple(tris)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:histBins, :sobelThesh])
    positional = [
      pts3d: Evision.Internal.Structurise.from_struct(pts3d),
      tris: Evision.Internal.Structurise.from_struct(tris)
    ]
    :evision_nif.rapid_rapid_GOSTracker_create_static(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  create

  ##### Positional Arguments
  - **pts3d**: `Evision.Mat`
  - **tris**: `Evision.Mat`

  ##### Keyword Arguments
  - **histBins**: `integer()`.
  - **sobelThesh**: `uchar`.

  ##### Return
  - **retval**: `OLSTracker`

  Python prototype (for reference only):
  ```python3
  create(pts3d, tris[, histBins[, sobelThesh]]) -> retval
  ```
  """
  @spec create(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.Rapid.OLSTracker.t() | {:error, String.t()}
  def create(pts3d, tris) when (is_struct(pts3d, Evision.Mat) or is_struct(pts3d, Nx.Tensor) or is_number(pts3d) or is_tuple(pts3d)) and (is_struct(tris, Evision.Mat) or is_struct(tris, Nx.Tensor) or is_number(tris) or is_tuple(tris))
  do
    positional = [
      pts3d: Evision.Internal.Structurise.from_struct(pts3d),
      tris: Evision.Internal.Structurise.from_struct(tris)
    ]
    :evision_nif.rapid_rapid_GOSTracker_create_static(positional)
    |> to_struct()
  end
end
