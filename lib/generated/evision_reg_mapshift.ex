defmodule Evision.Reg.MapShift do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Reg.MapShift` struct.

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
  def to_struct({:ok, %{class: Evision.Reg.MapShift, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Reg.MapShift, ref: ref}) do
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
  MapShift

  ##### Positional Arguments
  - **shift**: `Evision.Mat`

  ##### Return
  - **self**: `MapShift`

  Python prototype (for reference only):
  ```python3
  MapShift(shift) -> <reg_MapShift object>
  ```
  """
  @spec mapShift(Evision.Mat.maybe_mat_in()) :: Evision.Reg.MapShift.t() | {:error, String.t()}
  def mapShift(shift) when (is_struct(shift, Evision.Mat) or is_struct(shift, Nx.Tensor) or is_number(shift) or is_tuple(shift))
  do
    positional = [
      shift: Evision.Internal.Structurise.from_struct(shift)
    ]
    :evision_nif.reg_reg_MapShift_MapShift(positional)
    |> to_struct()
  end

  @doc """
  MapShift
  ##### Return
  - **self**: `MapShift`

  Python prototype (for reference only):
  ```python3
  MapShift() -> <reg_MapShift object>
  ```
  """
  @spec mapShift() :: Evision.Reg.MapShift.t() | {:error, String.t()}
  def mapShift() do
    positional = [
    ]
    :evision_nif.reg_reg_MapShift_MapShift(positional)
    |> to_struct()
  end

  @doc """
  compose

  ##### Positional Arguments
  - **self**: `Evision.Reg.MapShift.t()`
  - **map**: `Map`

  Python prototype (for reference only):
  ```python3
  compose(map) -> None
  ```
  """
  @spec compose(Evision.Reg.MapShift.t(), Evision.Reg.Map.t()) :: Evision.Reg.MapShift.t() | {:error, String.t()}
  def compose(self, map) when is_struct(map, Evision.Reg.Map)
  do
    positional = [
      map: Evision.Internal.Structurise.from_struct(map)
    ]
    :evision_nif.reg_reg_MapShift_compose(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getShift

  ##### Positional Arguments
  - **self**: `Evision.Reg.MapShift.t()`

  ##### Return
  - **shift**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  getShift([, shift]) -> shift
  ```
  """
  @spec getShift(Evision.Reg.MapShift.t(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def getShift(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    positional = [
    ]
    :evision_nif.reg_reg_MapShift_getShift(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  getShift

  ##### Positional Arguments
  - **self**: `Evision.Reg.MapShift.t()`

  ##### Return
  - **shift**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  getShift([, shift]) -> shift
  ```
  """
  @spec getShift(Evision.Reg.MapShift.t()) :: Evision.Mat.t() | {:error, String.t()}
  def getShift(self) do
    positional = [
    ]
    :evision_nif.reg_reg_MapShift_getShift(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  inverseMap

  ##### Positional Arguments
  - **self**: `Evision.Reg.MapShift.t()`

  ##### Return
  - **retval**: `cv::Ptr<Map>`

  Python prototype (for reference only):
  ```python3
  inverseMap() -> retval
  ```
  """
  @spec inverseMap(Evision.Reg.MapShift.t()) :: Evision.Reg.Map.t() | {:error, String.t()}
  def inverseMap(self) do
    positional = [
    ]
    :evision_nif.reg_reg_MapShift_inverseMap(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  inverseWarp

  ##### Positional Arguments
  - **self**: `Evision.Reg.MapShift.t()`
  - **img1**: `Evision.Mat`

  ##### Return
  - **img2**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  inverseWarp(img1[, img2]) -> img2
  ```
  """
  @spec inverseWarp(Evision.Reg.MapShift.t(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def inverseWarp(self, img1, opts) when (is_struct(img1, Evision.Mat) or is_struct(img1, Nx.Tensor) or is_number(img1) or is_tuple(img1)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      img1: Evision.Internal.Structurise.from_struct(img1)
    ]
    :evision_nif.reg_reg_MapShift_inverseWarp(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  inverseWarp

  ##### Positional Arguments
  - **self**: `Evision.Reg.MapShift.t()`
  - **img1**: `Evision.Mat`

  ##### Return
  - **img2**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  inverseWarp(img1[, img2]) -> img2
  ```
  """
  @spec inverseWarp(Evision.Reg.MapShift.t(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def inverseWarp(self, img1) when (is_struct(img1, Evision.Mat) or is_struct(img1, Nx.Tensor) or is_number(img1) or is_tuple(img1))
  do
    positional = [
      img1: Evision.Internal.Structurise.from_struct(img1)
    ]
    :evision_nif.reg_reg_MapShift_inverseWarp(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  scale

  ##### Positional Arguments
  - **self**: `Evision.Reg.MapShift.t()`
  - **factor**: `double`

  Python prototype (for reference only):
  ```python3
  scale(factor) -> None
  ```
  """
  @spec scale(Evision.Reg.MapShift.t(), number()) :: Evision.Reg.MapShift.t() | {:error, String.t()}
  def scale(self, factor) when is_number(factor)
  do
    positional = [
      factor: Evision.Internal.Structurise.from_struct(factor)
    ]
    :evision_nif.reg_reg_MapShift_scale(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
