defmodule Evision.CUDA.StereoConstantSpaceBP do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `CUDA.StereoConstantSpaceBP` struct.

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
  def to_struct({:ok, %{class: Evision.CUDA.StereoConstantSpaceBP, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.CUDA.StereoConstantSpaceBP, ref: ref}) do
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
  Uses a heuristic method to compute parameters (ndisp, iters, levelsand nrplane) for the specified
  image size (widthand height).

  ##### Positional Arguments
  - **width**: `integer()`
  - **height**: `integer()`
  - **ndisp**: `integer()`
  - **iters**: `integer()`
  - **levels**: `integer()`
  - **nr_plane**: `integer()`

  Python prototype (for reference only):
  ```python3
  estimateRecommendedParams(width, height, ndisp, iters, levels, nr_plane) -> None
  ```
  """
  @spec estimateRecommendedParams(integer(), integer(), integer(), integer(), integer(), integer()) :: :ok | {:error, String.t()}
  def estimateRecommendedParams(width, height, ndisp, iters, levels, nr_plane) when is_integer(width) and is_integer(height) and is_integer(ndisp) and is_integer(iters) and is_integer(levels) and is_integer(nr_plane)
  do
    positional = [
      width: Evision.Internal.Structurise.from_struct(width),
      height: Evision.Internal.Structurise.from_struct(height),
      ndisp: Evision.Internal.Structurise.from_struct(ndisp),
      iters: Evision.Internal.Structurise.from_struct(iters),
      levels: Evision.Internal.Structurise.from_struct(levels),
      nr_plane: Evision.Internal.Structurise.from_struct(nr_plane)
    ]
    :evision_nif.cuda_cuda_StereoConstantSpaceBP_estimateRecommendedParams_static(positional)
    |> to_struct()
  end

  @doc """
  getNrPlane

  ##### Positional Arguments
  - **self**: `Evision.CUDA.StereoConstantSpaceBP.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getNrPlane() -> retval
  ```
  """
  @spec getNrPlane(Evision.CUDA.CUDA.StereoConstantSpaceBP.t()) :: integer() | {:error, String.t()}
  def getNrPlane(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_StereoConstantSpaceBP_getNrPlane(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getUseLocalInitDataCost

  ##### Positional Arguments
  - **self**: `Evision.CUDA.StereoConstantSpaceBP.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  getUseLocalInitDataCost() -> retval
  ```
  """
  @spec getUseLocalInitDataCost(Evision.CUDA.CUDA.StereoConstantSpaceBP.t()) :: boolean() | {:error, String.t()}
  def getUseLocalInitDataCost(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_StereoConstantSpaceBP_getUseLocalInitDataCost(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setNrPlane

  ##### Positional Arguments
  - **self**: `Evision.CUDA.StereoConstantSpaceBP.t()`
  - **nr_plane**: `integer()`

  Python prototype (for reference only):
  ```python3
  setNrPlane(nr_plane) -> None
  ```
  """
  @spec setNrPlane(Evision.CUDA.CUDA.StereoConstantSpaceBP.t(), integer()) :: Evision.CUDA.CUDA.StereoConstantSpaceBP.t() | {:error, String.t()}
  def setNrPlane(self, nr_plane) when is_integer(nr_plane)
  do
    positional = [
      nr_plane: Evision.Internal.Structurise.from_struct(nr_plane)
    ]
    :evision_nif.cuda_cuda_StereoConstantSpaceBP_setNrPlane(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setUseLocalInitDataCost

  ##### Positional Arguments
  - **self**: `Evision.CUDA.StereoConstantSpaceBP.t()`
  - **use_local_init_data_cost**: `bool`

  Python prototype (for reference only):
  ```python3
  setUseLocalInitDataCost(use_local_init_data_cost) -> None
  ```
  """
  @spec setUseLocalInitDataCost(Evision.CUDA.CUDA.StereoConstantSpaceBP.t(), boolean()) :: Evision.CUDA.CUDA.StereoConstantSpaceBP.t() | {:error, String.t()}
  def setUseLocalInitDataCost(self, use_local_init_data_cost) when is_boolean(use_local_init_data_cost)
  do
    positional = [
      use_local_init_data_cost: Evision.Internal.Structurise.from_struct(use_local_init_data_cost)
    ]
    :evision_nif.cuda_cuda_StereoConstantSpaceBP_setUseLocalInitDataCost(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
