defmodule Evision.CUDA.BackgroundSubtractorMOG2 do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `CUDA.BackgroundSubtractorMOG2` struct.

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
  def to_struct({:ok, %{class: Evision.CUDA.BackgroundSubtractorMOG2, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.CUDA.BackgroundSubtractorMOG2, ref: ref}) do
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
  apply

  ##### Positional Arguments
  - **self**: `Evision.CUDA.BackgroundSubtractorMOG2.t()`
  - **image**: `Evision.Mat`
  - **learningRate**: `double`
  - **stream**: `Evision.CUDA.Stream.t()`

  ##### Return
  - **fgmask**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  apply(image, learningRate, stream[, fgmask]) -> fgmask
  ```
  #### Variant 2:
  apply

  ##### Positional Arguments
  - **self**: `Evision.CUDA.BackgroundSubtractorMOG2.t()`
  - **image**: `Evision.CUDA.GpuMat.t()`
  - **learningRate**: `double`
  - **stream**: `Evision.CUDA.Stream.t()`

  ##### Return
  - **fgmask**: `Evision.CUDA.GpuMat.t()`.

  Python prototype (for reference only):
  ```python3
  apply(image, learningRate, stream[, fgmask]) -> fgmask
  ```

  """
  @spec apply(Evision.CUDA.CUDA.BackgroundSubtractorMOG2.t(), Evision.Mat.maybe_mat_in(), number(), Evision.CUDA.Stream.t(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def apply(self, image, learningRate, stream, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and is_number(learningRate) and is_struct(stream, Evision.CUDA.Stream) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      learningRate: Evision.Internal.Structurise.from_struct(learningRate),
      stream: Evision.Internal.Structurise.from_struct(stream)
    ]
    :evision_nif.cuda_cuda_BackgroundSubtractorMOG2_apply(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec apply(Evision.CUDA.CUDA.BackgroundSubtractorMOG2.t(), Evision.CUDA.GpuMat.t(), number(), Evision.CUDA.Stream.t(), [{atom(), term()},...] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def apply(self, image, learningRate, stream, opts) when is_struct(image, Evision.CUDA.GpuMat) and is_number(learningRate) and is_struct(stream, Evision.CUDA.Stream) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      learningRate: Evision.Internal.Structurise.from_struct(learningRate),
      stream: Evision.Internal.Structurise.from_struct(stream)
    ]
    :evision_nif.cuda_cuda_BackgroundSubtractorMOG2_apply(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  apply

  ##### Positional Arguments
  - **self**: `Evision.CUDA.BackgroundSubtractorMOG2.t()`
  - **image**: `Evision.Mat`
  - **learningRate**: `double`
  - **stream**: `Evision.CUDA.Stream.t()`

  ##### Return
  - **fgmask**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  apply(image, learningRate, stream[, fgmask]) -> fgmask
  ```
  #### Variant 2:
  apply

  ##### Positional Arguments
  - **self**: `Evision.CUDA.BackgroundSubtractorMOG2.t()`
  - **image**: `Evision.CUDA.GpuMat.t()`
  - **learningRate**: `double`
  - **stream**: `Evision.CUDA.Stream.t()`

  ##### Return
  - **fgmask**: `Evision.CUDA.GpuMat.t()`.

  Python prototype (for reference only):
  ```python3
  apply(image, learningRate, stream[, fgmask]) -> fgmask
  ```

  """
  @spec apply(Evision.CUDA.CUDA.BackgroundSubtractorMOG2.t(), Evision.Mat.maybe_mat_in(), number(), Evision.CUDA.Stream.t()) :: Evision.Mat.t() | {:error, String.t()}
  def apply(self, image, learningRate, stream) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and is_number(learningRate) and is_struct(stream, Evision.CUDA.Stream)
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      learningRate: Evision.Internal.Structurise.from_struct(learningRate),
      stream: Evision.Internal.Structurise.from_struct(stream)
    ]
    :evision_nif.cuda_cuda_BackgroundSubtractorMOG2_apply(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec apply(Evision.CUDA.CUDA.BackgroundSubtractorMOG2.t(), Evision.CUDA.GpuMat.t(), number(), Evision.CUDA.Stream.t()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def apply(self, image, learningRate, stream) when is_struct(image, Evision.CUDA.GpuMat) and is_number(learningRate) and is_struct(stream, Evision.CUDA.Stream)
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      learningRate: Evision.Internal.Structurise.from_struct(learningRate),
      stream: Evision.Internal.Structurise.from_struct(stream)
    ]
    :evision_nif.cuda_cuda_BackgroundSubtractorMOG2_apply(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getBackgroundImage

  ##### Positional Arguments
  - **self**: `Evision.CUDA.BackgroundSubtractorMOG2.t()`
  - **stream**: `Evision.CUDA.Stream.t()`

  ##### Return
  - **backgroundImage**: `Evision.CUDA.GpuMat.t()`.

  Python prototype (for reference only):
  ```python3
  getBackgroundImage(stream[, backgroundImage]) -> backgroundImage
  ```
  """
  @spec getBackgroundImage(Evision.CUDA.CUDA.BackgroundSubtractorMOG2.t(), Evision.CUDA.Stream.t(), [{atom(), term()},...] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def getBackgroundImage(self, stream, opts) when is_struct(stream, Evision.CUDA.Stream) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      stream: Evision.Internal.Structurise.from_struct(stream)
    ]
    :evision_nif.cuda_cuda_BackgroundSubtractorMOG2_getBackgroundImage(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  getBackgroundImage

  ##### Positional Arguments
  - **self**: `Evision.CUDA.BackgroundSubtractorMOG2.t()`
  - **stream**: `Evision.CUDA.Stream.t()`

  ##### Return
  - **backgroundImage**: `Evision.CUDA.GpuMat.t()`.

  Python prototype (for reference only):
  ```python3
  getBackgroundImage(stream[, backgroundImage]) -> backgroundImage
  ```
  """
  @spec getBackgroundImage(Evision.CUDA.CUDA.BackgroundSubtractorMOG2.t(), Evision.CUDA.Stream.t()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def getBackgroundImage(self, stream) when is_struct(stream, Evision.CUDA.Stream)
  do
    positional = [
      stream: Evision.Internal.Structurise.from_struct(stream)
    ]
    :evision_nif.cuda_cuda_BackgroundSubtractorMOG2_getBackgroundImage(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
