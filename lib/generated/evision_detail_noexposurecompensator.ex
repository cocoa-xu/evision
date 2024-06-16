defmodule Evision.Detail.NoExposureCompensator do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Detail.NoExposureCompensator` struct.

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
  def to_struct({:ok, %{class: Evision.Detail.NoExposureCompensator, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Detail.NoExposureCompensator, ref: ref}) do
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
  apply

  ##### Positional Arguments
  - **self**: `Evision.Detail.NoExposureCompensator.t()`
  - **arg1**: `integer()`
  - **arg2**: `Point`
  - **arg4**: `Evision.Mat`

  ##### Return
  - **arg3**: `Evision.Mat.t()`

  Python prototype (for reference only):
  ```python3
  apply(arg1, arg2, arg3, arg4) -> arg3
  ```
  """
  @spec apply(Evision.Detail.NoExposureCompensator.t(), integer(), {number(), number()}, Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def apply(self, arg1, arg2, arg3, arg4) when is_integer(arg1) and is_tuple(arg2) and (is_struct(arg3, Evision.Mat) or is_struct(arg3, Nx.Tensor) or is_number(arg3) or is_tuple(arg3)) and (is_struct(arg4, Evision.Mat) or is_struct(arg4, Nx.Tensor) or is_number(arg4) or is_tuple(arg4))
  do
    positional = [
      arg1: Evision.Internal.Structurise.from_struct(arg1),
      arg2: Evision.Internal.Structurise.from_struct(arg2),
      arg3: Evision.Internal.Structurise.from_struct(arg3),
      arg4: Evision.Internal.Structurise.from_struct(arg4)
    ]
    :evision_nif.detail_detail_NoExposureCompensator_apply(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMatGains

  ##### Positional Arguments
  - **self**: `Evision.Detail.NoExposureCompensator.t()`

  ##### Return
  - **umv**: `[Evision.Mat]`.

  Python prototype (for reference only):
  ```python3
  getMatGains([, umv]) -> umv
  ```
  """
  @spec getMatGains(Evision.Detail.NoExposureCompensator.t(), [{atom(), term()},...] | nil) :: list(Evision.Mat.t()) | {:error, String.t()}
  def getMatGains(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    positional = [
    ]
    :evision_nif.detail_detail_NoExposureCompensator_getMatGains(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  getMatGains

  ##### Positional Arguments
  - **self**: `Evision.Detail.NoExposureCompensator.t()`

  ##### Return
  - **umv**: `[Evision.Mat]`.

  Python prototype (for reference only):
  ```python3
  getMatGains([, umv]) -> umv
  ```
  """
  @spec getMatGains(Evision.Detail.NoExposureCompensator.t()) :: list(Evision.Mat.t()) | {:error, String.t()}
  def getMatGains(self) do
    positional = [
    ]
    :evision_nif.detail_detail_NoExposureCompensator_getMatGains(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMatGains

  ##### Positional Arguments
  - **self**: `Evision.Detail.NoExposureCompensator.t()`
  - **umv**: `[Evision.Mat]`

  Python prototype (for reference only):
  ```python3
  setMatGains(umv) -> None
  ```
  """
  @spec setMatGains(Evision.Detail.NoExposureCompensator.t(), list(Evision.Mat.maybe_mat_in())) :: Evision.Detail.NoExposureCompensator.t() | {:error, String.t()}
  def setMatGains(self, umv) when is_list(umv)
  do
    positional = [
      umv: Evision.Internal.Structurise.from_struct(umv)
    ]
    :evision_nif.detail_detail_NoExposureCompensator_setMatGains(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
