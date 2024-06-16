defmodule Evision.Detail.BlocksCompensator do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Detail.BlocksCompensator` struct.

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
  def to_struct({:ok, %{class: Evision.Detail.BlocksCompensator, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Detail.BlocksCompensator, ref: ref}) do
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
  - **self**: `Evision.Detail.BlocksCompensator.t()`
  - **index**: `integer()`
  - **corner**: `Point`
  - **mask**: `Evision.Mat`

  ##### Return
  - **image**: `Evision.Mat.t()`

  Python prototype (for reference only):
  ```python3
  apply(index, corner, image, mask) -> image
  ```
  """
  @spec apply(Evision.Detail.BlocksCompensator.t(), integer(), {number(), number()}, Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def apply(self, index, corner, image, mask) when is_integer(index) and is_tuple(corner) and (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (is_struct(mask, Evision.Mat) or is_struct(mask, Nx.Tensor) or is_number(mask) or is_tuple(mask))
  do
    positional = [
      index: Evision.Internal.Structurise.from_struct(index),
      corner: Evision.Internal.Structurise.from_struct(corner),
      image: Evision.Internal.Structurise.from_struct(image),
      mask: Evision.Internal.Structurise.from_struct(mask)
    ]
    :evision_nif.detail_detail_BlocksCompensator_apply(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getBlockSize

  ##### Positional Arguments
  - **self**: `Evision.Detail.BlocksCompensator.t()`

  ##### Return
  - **retval**: `Size`

  Python prototype (for reference only):
  ```python3
  getBlockSize() -> retval
  ```
  """
  @spec getBlockSize(Evision.Detail.BlocksCompensator.t()) :: {number(), number()} | {:error, String.t()}
  def getBlockSize(self) do
    positional = [
    ]
    :evision_nif.detail_detail_BlocksCompensator_getBlockSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMatGains

  ##### Positional Arguments
  - **self**: `Evision.Detail.BlocksCompensator.t()`

  ##### Return
  - **umv**: `[Evision.Mat]`.

  Python prototype (for reference only):
  ```python3
  getMatGains([, umv]) -> umv
  ```
  """
  @spec getMatGains(Evision.Detail.BlocksCompensator.t(), [{atom(), term()},...] | nil) :: list(Evision.Mat.t()) | {:error, String.t()}
  def getMatGains(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    positional = [
    ]
    :evision_nif.detail_detail_BlocksCompensator_getMatGains(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  getMatGains

  ##### Positional Arguments
  - **self**: `Evision.Detail.BlocksCompensator.t()`

  ##### Return
  - **umv**: `[Evision.Mat]`.

  Python prototype (for reference only):
  ```python3
  getMatGains([, umv]) -> umv
  ```
  """
  @spec getMatGains(Evision.Detail.BlocksCompensator.t()) :: list(Evision.Mat.t()) | {:error, String.t()}
  def getMatGains(self) do
    positional = [
    ]
    :evision_nif.detail_detail_BlocksCompensator_getMatGains(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getNrFeeds

  ##### Positional Arguments
  - **self**: `Evision.Detail.BlocksCompensator.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getNrFeeds() -> retval
  ```
  """
  @spec getNrFeeds(Evision.Detail.BlocksCompensator.t()) :: integer() | {:error, String.t()}
  def getNrFeeds(self) do
    positional = [
    ]
    :evision_nif.detail_detail_BlocksCompensator_getNrFeeds(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getNrGainsFilteringIterations

  ##### Positional Arguments
  - **self**: `Evision.Detail.BlocksCompensator.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getNrGainsFilteringIterations() -> retval
  ```
  """
  @spec getNrGainsFilteringIterations(Evision.Detail.BlocksCompensator.t()) :: integer() | {:error, String.t()}
  def getNrGainsFilteringIterations(self) do
    positional = [
    ]
    :evision_nif.detail_detail_BlocksCompensator_getNrGainsFilteringIterations(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getSimilarityThreshold

  ##### Positional Arguments
  - **self**: `Evision.Detail.BlocksCompensator.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getSimilarityThreshold() -> retval
  ```
  """
  @spec getSimilarityThreshold(Evision.Detail.BlocksCompensator.t()) :: number() | {:error, String.t()}
  def getSimilarityThreshold(self) do
    positional = [
    ]
    :evision_nif.detail_detail_BlocksCompensator_getSimilarityThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setBlockSize

  ##### Positional Arguments
  - **self**: `Evision.Detail.BlocksCompensator.t()`
  - **width**: `integer()`
  - **height**: `integer()`

  Python prototype (for reference only):
  ```python3
  setBlockSize(width, height) -> None
  ```
  """
  @spec setBlockSize(Evision.Detail.BlocksCompensator.t(), integer(), integer()) :: Evision.Detail.BlocksCompensator.t() | {:error, String.t()}
  def setBlockSize(self, width, height) when is_integer(width) and is_integer(height)
  do
    positional = [
      width: Evision.Internal.Structurise.from_struct(width),
      height: Evision.Internal.Structurise.from_struct(height)
    ]
    :evision_nif.detail_detail_BlocksCompensator_setBlockSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setBlockSize

  ##### Positional Arguments
  - **self**: `Evision.Detail.BlocksCompensator.t()`
  - **size**: `Size`

  Python prototype (for reference only):
  ```python3
  setBlockSize(size) -> None
  ```
  """
  @spec setBlockSize(Evision.Detail.BlocksCompensator.t(), {number(), number()}) :: Evision.Detail.BlocksCompensator.t() | {:error, String.t()}
  def setBlockSize(self, size) when is_tuple(size)
  do
    positional = [
      size: Evision.Internal.Structurise.from_struct(size)
    ]
    :evision_nif.detail_detail_BlocksCompensator_setBlockSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMatGains

  ##### Positional Arguments
  - **self**: `Evision.Detail.BlocksCompensator.t()`
  - **umv**: `[Evision.Mat]`

  Python prototype (for reference only):
  ```python3
  setMatGains(umv) -> None
  ```
  """
  @spec setMatGains(Evision.Detail.BlocksCompensator.t(), list(Evision.Mat.maybe_mat_in())) :: Evision.Detail.BlocksCompensator.t() | {:error, String.t()}
  def setMatGains(self, umv) when is_list(umv)
  do
    positional = [
      umv: Evision.Internal.Structurise.from_struct(umv)
    ]
    :evision_nif.detail_detail_BlocksCompensator_setMatGains(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setNrFeeds

  ##### Positional Arguments
  - **self**: `Evision.Detail.BlocksCompensator.t()`
  - **nr_feeds**: `integer()`

  Python prototype (for reference only):
  ```python3
  setNrFeeds(nr_feeds) -> None
  ```
  """
  @spec setNrFeeds(Evision.Detail.BlocksCompensator.t(), integer()) :: Evision.Detail.BlocksCompensator.t() | {:error, String.t()}
  def setNrFeeds(self, nr_feeds) when is_integer(nr_feeds)
  do
    positional = [
      nr_feeds: Evision.Internal.Structurise.from_struct(nr_feeds)
    ]
    :evision_nif.detail_detail_BlocksCompensator_setNrFeeds(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setNrGainsFilteringIterations

  ##### Positional Arguments
  - **self**: `Evision.Detail.BlocksCompensator.t()`
  - **nr_iterations**: `integer()`

  Python prototype (for reference only):
  ```python3
  setNrGainsFilteringIterations(nr_iterations) -> None
  ```
  """
  @spec setNrGainsFilteringIterations(Evision.Detail.BlocksCompensator.t(), integer()) :: Evision.Detail.BlocksCompensator.t() | {:error, String.t()}
  def setNrGainsFilteringIterations(self, nr_iterations) when is_integer(nr_iterations)
  do
    positional = [
      nr_iterations: Evision.Internal.Structurise.from_struct(nr_iterations)
    ]
    :evision_nif.detail_detail_BlocksCompensator_setNrGainsFilteringIterations(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setSimilarityThreshold

  ##### Positional Arguments
  - **self**: `Evision.Detail.BlocksCompensator.t()`
  - **similarity_threshold**: `double`

  Python prototype (for reference only):
  ```python3
  setSimilarityThreshold(similarity_threshold) -> None
  ```
  """
  @spec setSimilarityThreshold(Evision.Detail.BlocksCompensator.t(), number()) :: Evision.Detail.BlocksCompensator.t() | {:error, String.t()}
  def setSimilarityThreshold(self, similarity_threshold) when is_number(similarity_threshold)
  do
    positional = [
      similarity_threshold: Evision.Internal.Structurise.from_struct(similarity_threshold)
    ]
    :evision_nif.detail_detail_BlocksCompensator_setSimilarityThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
