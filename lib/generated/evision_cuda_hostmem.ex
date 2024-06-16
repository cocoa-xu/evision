defmodule Evision.CUDA.HostMem do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `CUDA.HostMem` struct.

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
  def to_struct({:ok, %{class: Evision.CUDA.HostMem, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.CUDA.HostMem, ref: ref}) do
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
  HostMem

  ##### Positional Arguments
  - **rows**: `integer()`
  - **cols**: `integer()`
  - **type**: `integer()`

  ##### Keyword Arguments
  - **alloc_type**: `HostMem_AllocType`.

  ##### Return
  - **self**: `Evision.CUDA.HostMem.t()`

  Python prototype (for reference only):
  ```python3
  HostMem(rows, cols, type[, alloc_type]) -> <cuda_HostMem object>
  ```
  """
  @spec hostMem(integer(), integer(), integer(), [{:alloc_type, term()}] | nil) :: Evision.CUDA.HostMem.t() | {:error, String.t()}
  def hostMem(rows, cols, type, opts) when is_integer(rows) and is_integer(cols) and is_integer(type) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:alloc_type])
    positional = [
      rows: Evision.Internal.Structurise.from_struct(rows),
      cols: Evision.Internal.Structurise.from_struct(cols),
      type: Evision.Internal.Structurise.from_struct(type)
    ]
    :evision_nif.cuda_cuda_HostMem_HostMem(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  HostMem

  ##### Positional Arguments
  - **rows**: `integer()`
  - **cols**: `integer()`
  - **type**: `integer()`

  ##### Keyword Arguments
  - **alloc_type**: `HostMem_AllocType`.

  ##### Return
  - **self**: `Evision.CUDA.HostMem.t()`

  Python prototype (for reference only):
  ```python3
  HostMem(rows, cols, type[, alloc_type]) -> <cuda_HostMem object>
  ```
  #### Variant 2:
  HostMem

  ##### Positional Arguments
  - **size**: `Size`
  - **type**: `integer()`

  ##### Keyword Arguments
  - **alloc_type**: `HostMem_AllocType`.

  ##### Return
  - **self**: `Evision.CUDA.HostMem.t()`

  Python prototype (for reference only):
  ```python3
  HostMem(size, type[, alloc_type]) -> <cuda_HostMem object>
  ```

  """
  @spec hostMem({number(), number()}, integer(), [{:alloc_type, term()}] | nil) :: Evision.CUDA.HostMem.t() | {:error, String.t()}
  def hostMem(size, type, opts) when is_tuple(size) and is_integer(type) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:alloc_type])
    positional = [
      size: Evision.Internal.Structurise.from_struct(size),
      type: Evision.Internal.Structurise.from_struct(type)
    ]
    :evision_nif.cuda_cuda_HostMem_HostMem(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec hostMem(integer(), integer(), integer()) :: Evision.CUDA.HostMem.t() | {:error, String.t()}
  def hostMem(rows, cols, type) when is_integer(rows) and is_integer(cols) and is_integer(type)
  do
    positional = [
      rows: Evision.Internal.Structurise.from_struct(rows),
      cols: Evision.Internal.Structurise.from_struct(cols),
      type: Evision.Internal.Structurise.from_struct(type)
    ]
    :evision_nif.cuda_cuda_HostMem_HostMem(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  HostMem

  ##### Positional Arguments
  - **size**: `Size`
  - **type**: `integer()`

  ##### Keyword Arguments
  - **alloc_type**: `HostMem_AllocType`.

  ##### Return
  - **self**: `Evision.CUDA.HostMem.t()`

  Python prototype (for reference only):
  ```python3
  HostMem(size, type[, alloc_type]) -> <cuda_HostMem object>
  ```
  #### Variant 2:
  HostMem

  ##### Positional Arguments
  - **arr**: `Evision.Mat`

  ##### Keyword Arguments
  - **alloc_type**: `HostMem_AllocType`.

  ##### Return
  - **self**: `Evision.CUDA.HostMem.t()`

  Python prototype (for reference only):
  ```python3
  HostMem(arr[, alloc_type]) -> <cuda_HostMem object>
  ```
  #### Variant 3:
  HostMem

  ##### Positional Arguments
  - **arr**: `Evision.CUDA.GpuMat.t()`

  ##### Keyword Arguments
  - **alloc_type**: `HostMem_AllocType`.

  ##### Return
  - **self**: `Evision.CUDA.HostMem.t()`

  Python prototype (for reference only):
  ```python3
  HostMem(arr[, alloc_type]) -> <cuda_HostMem object>
  ```

  """
  @spec hostMem(Evision.Mat.maybe_mat_in(), [{:alloc_type, term()}] | nil) :: Evision.CUDA.HostMem.t() | {:error, String.t()}
  def hostMem(arr, opts) when (is_struct(arr, Evision.Mat) or is_struct(arr, Nx.Tensor) or is_number(arr) or is_tuple(arr)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:alloc_type])
    positional = [
      arr: Evision.Internal.Structurise.from_struct(arr)
    ]
    :evision_nif.cuda_cuda_HostMem_HostMem(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec hostMem(Evision.CUDA.GpuMat.t(), [{:alloc_type, term()}] | nil) :: Evision.CUDA.HostMem.t() | {:error, String.t()}
  def hostMem(arr, opts) when is_struct(arr, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:alloc_type])
    positional = [
      arr: Evision.Internal.Structurise.from_struct(arr)
    ]
    :evision_nif.cuda_cuda_HostMem_HostMem(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec hostMem({number(), number()}, integer()) :: Evision.CUDA.HostMem.t() | {:error, String.t()}
  def hostMem(size, type) when is_tuple(size) and is_integer(type)
  do
    positional = [
      size: Evision.Internal.Structurise.from_struct(size),
      type: Evision.Internal.Structurise.from_struct(type)
    ]
    :evision_nif.cuda_cuda_HostMem_HostMem(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  HostMem

  ##### Positional Arguments
  - **arr**: `Evision.Mat`

  ##### Keyword Arguments
  - **alloc_type**: `HostMem_AllocType`.

  ##### Return
  - **self**: `Evision.CUDA.HostMem.t()`

  Python prototype (for reference only):
  ```python3
  HostMem(arr[, alloc_type]) -> <cuda_HostMem object>
  ```
  #### Variant 2:
  HostMem

  ##### Positional Arguments
  - **arr**: `Evision.CUDA.GpuMat.t()`

  ##### Keyword Arguments
  - **alloc_type**: `HostMem_AllocType`.

  ##### Return
  - **self**: `Evision.CUDA.HostMem.t()`

  Python prototype (for reference only):
  ```python3
  HostMem(arr[, alloc_type]) -> <cuda_HostMem object>
  ```
  #### Variant 3:
  HostMem
  ##### Keyword Arguments
  - **alloc_type**: `HostMem_AllocType`.

  ##### Return
  - **self**: `Evision.CUDA.HostMem.t()`

  Python prototype (for reference only):
  ```python3
  HostMem([, alloc_type]) -> <cuda_HostMem object>
  ```

  """
  @spec hostMem([{:alloc_type, term()}] | nil) :: Evision.CUDA.HostMem.t() | {:error, String.t()}
  def hostMem(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:alloc_type])
    positional = [
    ]
    :evision_nif.cuda_cuda_HostMem_HostMem(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec hostMem(Evision.Mat.maybe_mat_in()) :: Evision.CUDA.HostMem.t() | {:error, String.t()}
  def hostMem(arr) when (is_struct(arr, Evision.Mat) or is_struct(arr, Nx.Tensor) or is_number(arr) or is_tuple(arr))
  do
    positional = [
      arr: Evision.Internal.Structurise.from_struct(arr)
    ]
    :evision_nif.cuda_cuda_HostMem_HostMem(positional)
    |> to_struct()
  end
  @spec hostMem(Evision.CUDA.GpuMat.t()) :: Evision.CUDA.HostMem.t() | {:error, String.t()}
  def hostMem(arr) when is_struct(arr, Evision.CUDA.GpuMat)
  do
    positional = [
      arr: Evision.Internal.Structurise.from_struct(arr)
    ]
    :evision_nif.cuda_cuda_HostMem_HostMem(positional)
    |> to_struct()
  end

  @doc """
  HostMem
  ##### Keyword Arguments
  - **alloc_type**: `HostMem_AllocType`.

  ##### Return
  - **self**: `Evision.CUDA.HostMem.t()`

  Python prototype (for reference only):
  ```python3
  HostMem([, alloc_type]) -> <cuda_HostMem object>
  ```
  """
  @spec hostMem() :: Evision.CUDA.HostMem.t() | {:error, String.t()}
  def hostMem() do
    positional = [
    ]
    :evision_nif.cuda_cuda_HostMem_HostMem(positional)
    |> to_struct()
  end

  @doc """
  channels

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HostMem.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  channels() -> retval
  ```
  """
  @spec channels(Evision.CUDA.HostMem.t()) :: integer() | {:error, String.t()}
  def channels(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_HostMem_channels(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  clone

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HostMem.t()`

  ##### Return
  - **retval**: `Evision.CUDA.HostMem.t()`

  Python prototype (for reference only):
  ```python3
  clone() -> retval
  ```
  """
  @spec clone(Evision.CUDA.HostMem.t()) :: Evision.CUDA.HostMem.t() | {:error, String.t()}
  def clone(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_HostMem_clone(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  create

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HostMem.t()`
  - **rows**: `integer()`
  - **cols**: `integer()`
  - **type**: `integer()`

  Python prototype (for reference only):
  ```python3
  create(rows, cols, type) -> None
  ```
  """
  @spec create(Evision.CUDA.HostMem.t(), integer(), integer(), integer()) :: Evision.CUDA.HostMem.t() | {:error, String.t()}
  def create(self, rows, cols, type) when is_integer(rows) and is_integer(cols) and is_integer(type)
  do
    positional = [
      rows: Evision.Internal.Structurise.from_struct(rows),
      cols: Evision.Internal.Structurise.from_struct(cols),
      type: Evision.Internal.Structurise.from_struct(type)
    ]
    :evision_nif.cuda_cuda_HostMem_create(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  createMatHeader

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HostMem.t()`

  ##### Return
  - **retval**: `Evision.Mat.t()`

  Python prototype (for reference only):
  ```python3
  createMatHeader() -> retval
  ```
  """
  @spec createMatHeader(Evision.CUDA.HostMem.t()) :: Evision.Mat.t() | {:error, String.t()}
  def createMatHeader(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_HostMem_createMatHeader(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  depth

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HostMem.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  depth() -> retval
  ```
  """
  @spec depth(Evision.CUDA.HostMem.t()) :: integer() | {:error, String.t()}
  def depth(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_HostMem_depth(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  elemSize

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HostMem.t()`

  ##### Return
  - **retval**: `size_t`

  Python prototype (for reference only):
  ```python3
  elemSize() -> retval
  ```
  """
  @spec elemSize(Evision.CUDA.HostMem.t()) :: integer() | {:error, String.t()}
  def elemSize(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_HostMem_elemSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  elemSize1

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HostMem.t()`

  ##### Return
  - **retval**: `size_t`

  Python prototype (for reference only):
  ```python3
  elemSize1() -> retval
  ```
  """
  @spec elemSize1(Evision.CUDA.HostMem.t()) :: integer() | {:error, String.t()}
  def elemSize1(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_HostMem_elemSize1(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  empty

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HostMem.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.CUDA.HostMem.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_HostMem_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Maps CPU memory to GPU address space and creates the cuda::GpuMat header without reference counting
  for it.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HostMem.t()`

  ##### Return
  - **retval**: `bool`

  This can be done only if memory was allocated with the SHARED flag and if it is supported by the
  hardware. Laptops often share video and CPU memory, so address spaces can be mapped, which
  eliminates an extra copy.

  Python prototype (for reference only):
  ```python3
  isContinuous() -> retval
  ```
  """
  @spec isContinuous(Evision.CUDA.HostMem.t()) :: boolean() | {:error, String.t()}
  def isContinuous(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_HostMem_isContinuous(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  reshape

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HostMem.t()`
  - **cn**: `integer()`

  ##### Keyword Arguments
  - **rows**: `integer()`.

  ##### Return
  - **retval**: `Evision.CUDA.HostMem.t()`

  Python prototype (for reference only):
  ```python3
  reshape(cn[, rows]) -> retval
  ```
  """
  @spec reshape(Evision.CUDA.HostMem.t(), integer(), [{:rows, term()}] | nil) :: Evision.CUDA.HostMem.t() | {:error, String.t()}
  def reshape(self, cn, opts) when is_integer(cn) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:rows])
    positional = [
      cn: Evision.Internal.Structurise.from_struct(cn)
    ]
    :evision_nif.cuda_cuda_HostMem_reshape(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  reshape

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HostMem.t()`
  - **cn**: `integer()`

  ##### Keyword Arguments
  - **rows**: `integer()`.

  ##### Return
  - **retval**: `Evision.CUDA.HostMem.t()`

  Python prototype (for reference only):
  ```python3
  reshape(cn[, rows]) -> retval
  ```
  """
  @spec reshape(Evision.CUDA.HostMem.t(), integer()) :: Evision.CUDA.HostMem.t() | {:error, String.t()}
  def reshape(self, cn) when is_integer(cn)
  do
    positional = [
      cn: Evision.Internal.Structurise.from_struct(cn)
    ]
    :evision_nif.cuda_cuda_HostMem_reshape(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  size

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HostMem.t()`

  ##### Return
  - **retval**: `Size`

  Python prototype (for reference only):
  ```python3
  size() -> retval
  ```
  """
  @spec size(Evision.CUDA.HostMem.t()) :: {number(), number()} | {:error, String.t()}
  def size(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_HostMem_size(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  step1

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HostMem.t()`

  ##### Return
  - **retval**: `size_t`

  Python prototype (for reference only):
  ```python3
  step1() -> retval
  ```
  """
  @spec step1(Evision.CUDA.HostMem.t()) :: integer() | {:error, String.t()}
  def step1(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_HostMem_step1(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  swap

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HostMem.t()`
  - **b**: `Evision.CUDA.HostMem.t()`

  Python prototype (for reference only):
  ```python3
  swap(b) -> None
  ```
  """
  @spec swap(Evision.CUDA.HostMem.t(), Evision.CUDA.HostMem.t()) :: Evision.CUDA.HostMem.t() | {:error, String.t()}
  def swap(self, b) when is_struct(b, Evision.CUDA.HostMem)
  do
    positional = [
      b: Evision.Internal.Structurise.from_struct(b)
    ]
    :evision_nif.cuda_cuda_HostMem_swap(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  type

  ##### Positional Arguments
  - **self**: `Evision.CUDA.HostMem.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  type() -> retval
  ```
  """
  @spec type(Evision.CUDA.HostMem.t()) :: integer() | {:error, String.t()}
  def type(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_HostMem_type(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec get_step(Evision.CUDA.HostMem.t()) :: integer()
  def get_step(self) do
    :evision_nif.cuda_HostMem_get_step(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
end
