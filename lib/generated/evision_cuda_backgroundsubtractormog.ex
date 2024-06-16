defmodule Evision.CUDA.BackgroundSubtractorMOG do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `CUDA.BackgroundSubtractorMOG` struct.

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
  def to_struct({:ok, %{class: Evision.CUDA.BackgroundSubtractorMOG, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.CUDA.BackgroundSubtractorMOG, ref: ref}) do
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
  - **self**: `Evision.CUDA.BackgroundSubtractorMOG.t()`
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
  - **self**: `Evision.CUDA.BackgroundSubtractorMOG.t()`
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
  @spec apply(Evision.CUDA.CUDA.BackgroundSubtractorMOG.t(), Evision.Mat.maybe_mat_in(), number(), Evision.CUDA.Stream.t(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def apply(self, image, learningRate, stream, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and is_number(learningRate) and is_struct(stream, Evision.CUDA.Stream) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      learningRate: Evision.Internal.Structurise.from_struct(learningRate),
      stream: Evision.Internal.Structurise.from_struct(stream)
    ]
    :evision_nif.cuda_cuda_BackgroundSubtractorMOG_apply(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec apply(Evision.CUDA.CUDA.BackgroundSubtractorMOG.t(), Evision.CUDA.GpuMat.t(), number(), Evision.CUDA.Stream.t(), [{atom(), term()},...] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def apply(self, image, learningRate, stream, opts) when is_struct(image, Evision.CUDA.GpuMat) and is_number(learningRate) and is_struct(stream, Evision.CUDA.Stream) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      learningRate: Evision.Internal.Structurise.from_struct(learningRate),
      stream: Evision.Internal.Structurise.from_struct(stream)
    ]
    :evision_nif.cuda_cuda_BackgroundSubtractorMOG_apply(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  apply

  ##### Positional Arguments
  - **self**: `Evision.CUDA.BackgroundSubtractorMOG.t()`
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
  - **self**: `Evision.CUDA.BackgroundSubtractorMOG.t()`
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
  @spec apply(Evision.CUDA.CUDA.BackgroundSubtractorMOG.t(), Evision.Mat.maybe_mat_in(), number(), Evision.CUDA.Stream.t()) :: Evision.Mat.t() | {:error, String.t()}
  def apply(self, image, learningRate, stream) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and is_number(learningRate) and is_struct(stream, Evision.CUDA.Stream)
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      learningRate: Evision.Internal.Structurise.from_struct(learningRate),
      stream: Evision.Internal.Structurise.from_struct(stream)
    ]
    :evision_nif.cuda_cuda_BackgroundSubtractorMOG_apply(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec apply(Evision.CUDA.CUDA.BackgroundSubtractorMOG.t(), Evision.CUDA.GpuMat.t(), number(), Evision.CUDA.Stream.t()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def apply(self, image, learningRate, stream) when is_struct(image, Evision.CUDA.GpuMat) and is_number(learningRate) and is_struct(stream, Evision.CUDA.Stream)
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      learningRate: Evision.Internal.Structurise.from_struct(learningRate),
      stream: Evision.Internal.Structurise.from_struct(stream)
    ]
    :evision_nif.cuda_cuda_BackgroundSubtractorMOG_apply(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getBackgroundImage

  ##### Positional Arguments
  - **self**: `Evision.CUDA.BackgroundSubtractorMOG.t()`
  - **stream**: `Evision.CUDA.Stream.t()`

  ##### Return
  - **backgroundImage**: `Evision.CUDA.GpuMat.t()`.

  Python prototype (for reference only):
  ```python3
  getBackgroundImage(stream[, backgroundImage]) -> backgroundImage
  ```
  """
  @spec getBackgroundImage(Evision.CUDA.CUDA.BackgroundSubtractorMOG.t(), Evision.CUDA.Stream.t(), [{atom(), term()},...] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def getBackgroundImage(self, stream, opts) when is_struct(stream, Evision.CUDA.Stream) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      stream: Evision.Internal.Structurise.from_struct(stream)
    ]
    :evision_nif.cuda_cuda_BackgroundSubtractorMOG_getBackgroundImage(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  getBackgroundImage

  ##### Positional Arguments
  - **self**: `Evision.CUDA.BackgroundSubtractorMOG.t()`
  - **stream**: `Evision.CUDA.Stream.t()`

  ##### Return
  - **backgroundImage**: `Evision.CUDA.GpuMat.t()`.

  Python prototype (for reference only):
  ```python3
  getBackgroundImage(stream[, backgroundImage]) -> backgroundImage
  ```
  """
  @spec getBackgroundImage(Evision.CUDA.CUDA.BackgroundSubtractorMOG.t(), Evision.CUDA.Stream.t()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def getBackgroundImage(self, stream) when is_struct(stream, Evision.CUDA.Stream)
  do
    positional = [
      stream: Evision.Internal.Structurise.from_struct(stream)
    ]
    :evision_nif.cuda_cuda_BackgroundSubtractorMOG_getBackgroundImage(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getBackgroundRatio

  ##### Positional Arguments
  - **self**: `Evision.CUDA.BackgroundSubtractorMOG.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getBackgroundRatio() -> retval
  ```
  """
  @spec getBackgroundRatio(Evision.CUDA.CUDA.BackgroundSubtractorMOG.t()) :: number() | {:error, String.t()}
  def getBackgroundRatio(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_BackgroundSubtractorMOG_getBackgroundRatio(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getHistory

  ##### Positional Arguments
  - **self**: `Evision.CUDA.BackgroundSubtractorMOG.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getHistory() -> retval
  ```
  """
  @spec getHistory(Evision.CUDA.CUDA.BackgroundSubtractorMOG.t()) :: integer() | {:error, String.t()}
  def getHistory(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_BackgroundSubtractorMOG_getHistory(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getNMixtures

  ##### Positional Arguments
  - **self**: `Evision.CUDA.BackgroundSubtractorMOG.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getNMixtures() -> retval
  ```
  """
  @spec getNMixtures(Evision.CUDA.CUDA.BackgroundSubtractorMOG.t()) :: integer() | {:error, String.t()}
  def getNMixtures(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_BackgroundSubtractorMOG_getNMixtures(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getNoiseSigma

  ##### Positional Arguments
  - **self**: `Evision.CUDA.BackgroundSubtractorMOG.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getNoiseSigma() -> retval
  ```
  """
  @spec getNoiseSigma(Evision.CUDA.CUDA.BackgroundSubtractorMOG.t()) :: number() | {:error, String.t()}
  def getNoiseSigma(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_BackgroundSubtractorMOG_getNoiseSigma(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setBackgroundRatio

  ##### Positional Arguments
  - **self**: `Evision.CUDA.BackgroundSubtractorMOG.t()`
  - **backgroundRatio**: `double`

  Python prototype (for reference only):
  ```python3
  setBackgroundRatio(backgroundRatio) -> None
  ```
  """
  @spec setBackgroundRatio(Evision.CUDA.CUDA.BackgroundSubtractorMOG.t(), number()) :: Evision.CUDA.CUDA.BackgroundSubtractorMOG.t() | {:error, String.t()}
  def setBackgroundRatio(self, backgroundRatio) when is_number(backgroundRatio)
  do
    positional = [
      backgroundRatio: Evision.Internal.Structurise.from_struct(backgroundRatio)
    ]
    :evision_nif.cuda_cuda_BackgroundSubtractorMOG_setBackgroundRatio(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setHistory

  ##### Positional Arguments
  - **self**: `Evision.CUDA.BackgroundSubtractorMOG.t()`
  - **nframes**: `integer()`

  Python prototype (for reference only):
  ```python3
  setHistory(nframes) -> None
  ```
  """
  @spec setHistory(Evision.CUDA.CUDA.BackgroundSubtractorMOG.t(), integer()) :: Evision.CUDA.CUDA.BackgroundSubtractorMOG.t() | {:error, String.t()}
  def setHistory(self, nframes) when is_integer(nframes)
  do
    positional = [
      nframes: Evision.Internal.Structurise.from_struct(nframes)
    ]
    :evision_nif.cuda_cuda_BackgroundSubtractorMOG_setHistory(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setNMixtures

  ##### Positional Arguments
  - **self**: `Evision.CUDA.BackgroundSubtractorMOG.t()`
  - **nmix**: `integer()`

  Python prototype (for reference only):
  ```python3
  setNMixtures(nmix) -> None
  ```
  """
  @spec setNMixtures(Evision.CUDA.CUDA.BackgroundSubtractorMOG.t(), integer()) :: Evision.CUDA.CUDA.BackgroundSubtractorMOG.t() | {:error, String.t()}
  def setNMixtures(self, nmix) when is_integer(nmix)
  do
    positional = [
      nmix: Evision.Internal.Structurise.from_struct(nmix)
    ]
    :evision_nif.cuda_cuda_BackgroundSubtractorMOG_setNMixtures(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setNoiseSigma

  ##### Positional Arguments
  - **self**: `Evision.CUDA.BackgroundSubtractorMOG.t()`
  - **noiseSigma**: `double`

  Python prototype (for reference only):
  ```python3
  setNoiseSigma(noiseSigma) -> None
  ```
  """
  @spec setNoiseSigma(Evision.CUDA.CUDA.BackgroundSubtractorMOG.t(), number()) :: Evision.CUDA.CUDA.BackgroundSubtractorMOG.t() | {:error, String.t()}
  def setNoiseSigma(self, noiseSigma) when is_number(noiseSigma)
  do
    positional = [
      noiseSigma: Evision.Internal.Structurise.from_struct(noiseSigma)
    ]
    :evision_nif.cuda_cuda_BackgroundSubtractorMOG_setNoiseSigma(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
