defmodule Evision.CUDA.StereoBM do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `CUDA.StereoBM` struct.

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
  def to_struct({:ok, %{class: Evision.CUDA.StereoBM, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.CUDA.StereoBM, ref: ref}) do
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
  #### Variant 1:
  compute

  ##### Positional Arguments
  - **self**: `Evision.CUDA.StereoBM.t()`
  - **left**: `Evision.Mat`
  - **right**: `Evision.Mat`
  - **stream**: `Evision.CUDA.Stream.t()`

  ##### Return
  - **disparity**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  compute(left, right, stream[, disparity]) -> disparity
  ```
  #### Variant 2:
  compute

  ##### Positional Arguments
  - **self**: `Evision.CUDA.StereoBM.t()`
  - **left**: `Evision.CUDA.GpuMat.t()`
  - **right**: `Evision.CUDA.GpuMat.t()`
  - **stream**: `Evision.CUDA.Stream.t()`

  ##### Return
  - **disparity**: `Evision.CUDA.GpuMat.t()`.

  Python prototype (for reference only):
  ```python3
  compute(left, right, stream[, disparity]) -> disparity
  ```

  """
  @spec compute(Evision.CUDA.CUDA.StereoBM.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.CUDA.Stream.t(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def compute(self, left, right, stream, opts) when (is_struct(left, Evision.Mat) or is_struct(left, Nx.Tensor) or is_number(left) or is_tuple(left)) and (is_struct(right, Evision.Mat) or is_struct(right, Nx.Tensor) or is_number(right) or is_tuple(right)) and is_struct(stream, Evision.CUDA.Stream) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      left: Evision.Internal.Structurise.from_struct(left),
      right: Evision.Internal.Structurise.from_struct(right),
      stream: Evision.Internal.Structurise.from_struct(stream)
    ]
    :evision_nif.cuda_cuda_StereoBM_compute(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec compute(Evision.CUDA.CUDA.StereoBM.t(), Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t(), Evision.CUDA.Stream.t(), [{atom(), term()},...] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def compute(self, left, right, stream, opts) when is_struct(left, Evision.CUDA.GpuMat) and is_struct(right, Evision.CUDA.GpuMat) and is_struct(stream, Evision.CUDA.Stream) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      left: Evision.Internal.Structurise.from_struct(left),
      right: Evision.Internal.Structurise.from_struct(right),
      stream: Evision.Internal.Structurise.from_struct(stream)
    ]
    :evision_nif.cuda_cuda_StereoBM_compute(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  compute

  ##### Positional Arguments
  - **self**: `Evision.CUDA.StereoBM.t()`
  - **left**: `Evision.Mat`
  - **right**: `Evision.Mat`
  - **stream**: `Evision.CUDA.Stream.t()`

  ##### Return
  - **disparity**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  compute(left, right, stream[, disparity]) -> disparity
  ```
  #### Variant 2:
  compute

  ##### Positional Arguments
  - **self**: `Evision.CUDA.StereoBM.t()`
  - **left**: `Evision.CUDA.GpuMat.t()`
  - **right**: `Evision.CUDA.GpuMat.t()`
  - **stream**: `Evision.CUDA.Stream.t()`

  ##### Return
  - **disparity**: `Evision.CUDA.GpuMat.t()`.

  Python prototype (for reference only):
  ```python3
  compute(left, right, stream[, disparity]) -> disparity
  ```

  """
  @spec compute(Evision.CUDA.CUDA.StereoBM.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.CUDA.Stream.t()) :: Evision.Mat.t() | {:error, String.t()}
  def compute(self, left, right, stream) when (is_struct(left, Evision.Mat) or is_struct(left, Nx.Tensor) or is_number(left) or is_tuple(left)) and (is_struct(right, Evision.Mat) or is_struct(right, Nx.Tensor) or is_number(right) or is_tuple(right)) and is_struct(stream, Evision.CUDA.Stream)
  do
    positional = [
      left: Evision.Internal.Structurise.from_struct(left),
      right: Evision.Internal.Structurise.from_struct(right),
      stream: Evision.Internal.Structurise.from_struct(stream)
    ]
    :evision_nif.cuda_cuda_StereoBM_compute(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec compute(Evision.CUDA.CUDA.StereoBM.t(), Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t(), Evision.CUDA.Stream.t()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def compute(self, left, right, stream) when is_struct(left, Evision.CUDA.GpuMat) and is_struct(right, Evision.CUDA.GpuMat) and is_struct(stream, Evision.CUDA.Stream)
  do
    positional = [
      left: Evision.Internal.Structurise.from_struct(left),
      right: Evision.Internal.Structurise.from_struct(right),
      stream: Evision.Internal.Structurise.from_struct(stream)
    ]
    :evision_nif.cuda_cuda_StereoBM_compute(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
