defmodule Evision.Reg.MapAffine do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Reg.MapAffine` struct.

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
  def to_struct({:ok, %{class: Evision.Reg.MapAffine, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Reg.MapAffine, ref: ref}) do
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
  MapAffine

  ##### Positional Arguments
  - **linTr**: `Evision.Mat`
  - **shift**: `Evision.Mat`

  ##### Return
  - **self**: `MapAffine`

  Python prototype (for reference only):
  ```python3
  MapAffine(linTr, shift) -> <reg_MapAffine object>
  ```
  """
  @spec mapAffine(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.Reg.MapAffine.t() | {:error, String.t()}
  def mapAffine(linTr, shift) when (is_struct(linTr, Evision.Mat) or is_struct(linTr, Nx.Tensor) or is_number(linTr) or is_tuple(linTr)) and (is_struct(shift, Evision.Mat) or is_struct(shift, Nx.Tensor) or is_number(shift) or is_tuple(shift))
  do
    positional = [
      linTr: Evision.Internal.Structurise.from_struct(linTr),
      shift: Evision.Internal.Structurise.from_struct(shift)
    ]
    :evision_nif.reg_reg_MapAffine_MapAffine(positional)
    |> to_struct()
  end

  @doc """
  MapAffine
  ##### Return
  - **self**: `MapAffine`

  Python prototype (for reference only):
  ```python3
  MapAffine() -> <reg_MapAffine object>
  ```
  """
  @spec mapAffine() :: Evision.Reg.MapAffine.t() | {:error, String.t()}
  def mapAffine() do
    positional = [
    ]
    :evision_nif.reg_reg_MapAffine_MapAffine(positional)
    |> to_struct()
  end

  @doc """
  compose

  ##### Positional Arguments
  - **self**: `Evision.Reg.MapAffine.t()`
  - **map**: `Map`

  Python prototype (for reference only):
  ```python3
  compose(map) -> None
  ```
  """
  @spec compose(Evision.Reg.MapAffine.t(), Evision.Reg.Map.t()) :: Evision.Reg.MapAffine.t() | {:error, String.t()}
  def compose(self, map) when is_struct(map, Evision.Reg.Map)
  do
    positional = [
      map: Evision.Internal.Structurise.from_struct(map)
    ]
    :evision_nif.reg_reg_MapAffine_compose(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getLinTr

  ##### Positional Arguments
  - **self**: `Evision.Reg.MapAffine.t()`

  ##### Return
  - **linTr**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  getLinTr([, linTr]) -> linTr
  ```
  """
  @spec getLinTr(Evision.Reg.MapAffine.t(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def getLinTr(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    positional = [
    ]
    :evision_nif.reg_reg_MapAffine_getLinTr(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  getLinTr

  ##### Positional Arguments
  - **self**: `Evision.Reg.MapAffine.t()`

  ##### Return
  - **linTr**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  getLinTr([, linTr]) -> linTr
  ```
  """
  @spec getLinTr(Evision.Reg.MapAffine.t()) :: Evision.Mat.t() | {:error, String.t()}
  def getLinTr(self) do
    positional = [
    ]
    :evision_nif.reg_reg_MapAffine_getLinTr(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getShift

  ##### Positional Arguments
  - **self**: `Evision.Reg.MapAffine.t()`

  ##### Return
  - **shift**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  getShift([, shift]) -> shift
  ```
  """
  @spec getShift(Evision.Reg.MapAffine.t(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def getShift(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    positional = [
    ]
    :evision_nif.reg_reg_MapAffine_getShift(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  getShift

  ##### Positional Arguments
  - **self**: `Evision.Reg.MapAffine.t()`

  ##### Return
  - **shift**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  getShift([, shift]) -> shift
  ```
  """
  @spec getShift(Evision.Reg.MapAffine.t()) :: Evision.Mat.t() | {:error, String.t()}
  def getShift(self) do
    positional = [
    ]
    :evision_nif.reg_reg_MapAffine_getShift(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  inverseMap

  ##### Positional Arguments
  - **self**: `Evision.Reg.MapAffine.t()`

  ##### Return
  - **retval**: `cv::Ptr<Map>`

  Python prototype (for reference only):
  ```python3
  inverseMap() -> retval
  ```
  """
  @spec inverseMap(Evision.Reg.MapAffine.t()) :: Evision.Reg.Map.t() | {:error, String.t()}
  def inverseMap(self) do
    positional = [
    ]
    :evision_nif.reg_reg_MapAffine_inverseMap(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  inverseWarp

  ##### Positional Arguments
  - **self**: `Evision.Reg.MapAffine.t()`
  - **img1**: `Evision.Mat`

  ##### Return
  - **img2**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  inverseWarp(img1[, img2]) -> img2
  ```
  """
  @spec inverseWarp(Evision.Reg.MapAffine.t(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def inverseWarp(self, img1, opts) when (is_struct(img1, Evision.Mat) or is_struct(img1, Nx.Tensor) or is_number(img1) or is_tuple(img1)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      img1: Evision.Internal.Structurise.from_struct(img1)
    ]
    :evision_nif.reg_reg_MapAffine_inverseWarp(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  inverseWarp

  ##### Positional Arguments
  - **self**: `Evision.Reg.MapAffine.t()`
  - **img1**: `Evision.Mat`

  ##### Return
  - **img2**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  inverseWarp(img1[, img2]) -> img2
  ```
  """
  @spec inverseWarp(Evision.Reg.MapAffine.t(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def inverseWarp(self, img1) when (is_struct(img1, Evision.Mat) or is_struct(img1, Nx.Tensor) or is_number(img1) or is_tuple(img1))
  do
    positional = [
      img1: Evision.Internal.Structurise.from_struct(img1)
    ]
    :evision_nif.reg_reg_MapAffine_inverseWarp(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  scale

  ##### Positional Arguments
  - **self**: `Evision.Reg.MapAffine.t()`
  - **factor**: `double`

  Python prototype (for reference only):
  ```python3
  scale(factor) -> None
  ```
  """
  @spec scale(Evision.Reg.MapAffine.t(), number()) :: Evision.Reg.MapAffine.t() | {:error, String.t()}
  def scale(self, factor) when is_number(factor)
  do
    positional = [
      factor: Evision.Internal.Structurise.from_struct(factor)
    ]
    :evision_nif.reg_reg_MapAffine_scale(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
