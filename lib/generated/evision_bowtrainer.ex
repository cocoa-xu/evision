defmodule Evision.BOWTrainer do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `BOWTrainer` struct.

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
  def to_struct({:ok, %{class: Evision.BOWTrainer, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.BOWTrainer, ref: ref}) do
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
  Adds descriptors to a training set.

  ##### Positional Arguments
  - **self**: `Evision.BOWTrainer.t()`
  - **descriptors**: `Evision.Mat`.

    Descriptors to add to a training set. Each row of the descriptors matrix is a
    descriptor.

  The training set is clustered using clustermethod to construct the vocabulary.

  Python prototype (for reference only):
  ```python3
  add(descriptors) -> None
  ```
  """
  @spec add(Evision.BOWTrainer.t(), Evision.Mat.maybe_mat_in()) :: Evision.BOWTrainer.t() | {:error, String.t()}
  def add(self, descriptors) when (is_struct(descriptors, Evision.Mat) or is_struct(descriptors, Nx.Tensor) or is_number(descriptors) or is_tuple(descriptors))
  do
    positional = [
      descriptors: Evision.Internal.Structurise.from_struct(descriptors)
    ]
    :evision_nif.bowTrainer_add(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  clear

  ##### Positional Arguments
  - **self**: `Evision.BOWTrainer.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.BOWTrainer.t()) :: Evision.BOWTrainer.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.bowTrainer_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Clusters train descriptors.

  ##### Positional Arguments
  - **self**: `Evision.BOWTrainer.t()`
  - **descriptors**: `Evision.Mat`.

    Descriptors to cluster. Each row of the descriptors matrix is a descriptor.
    Descriptors are not added to the inner train descriptor set.

  ##### Return
  - **retval**: `Evision.Mat.t()`

  The vocabulary consists of cluster centers. So, this method returns the vocabulary. In the first
  variant of the method, train descriptors stored in the object are clustered. In the second variant,
  input descriptors are clustered.

  Python prototype (for reference only):
  ```python3
  cluster(descriptors) -> retval
  ```
  """
  @spec cluster(Evision.BOWTrainer.t(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def cluster(self, descriptors) when (is_struct(descriptors, Evision.Mat) or is_struct(descriptors, Nx.Tensor) or is_number(descriptors) or is_tuple(descriptors))
  do
    positional = [
      descriptors: Evision.Internal.Structurise.from_struct(descriptors)
    ]
    :evision_nif.bowTrainer_cluster(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  cluster

  ##### Positional Arguments
  - **self**: `Evision.BOWTrainer.t()`

  ##### Return
  - **retval**: `Evision.Mat.t()`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  cluster() -> retval
  ```
  """
  @spec cluster(Evision.BOWTrainer.t()) :: Evision.Mat.t() | {:error, String.t()}
  def cluster(self) do
    positional = [
    ]
    :evision_nif.bowTrainer_cluster(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns the count of all descriptors stored in the training set.

  ##### Positional Arguments
  - **self**: `Evision.BOWTrainer.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  descriptorsCount() -> retval
  ```
  """
  @spec descriptorsCount(Evision.BOWTrainer.t()) :: integer() | {:error, String.t()}
  def descriptorsCount(self) do
    positional = [
    ]
    :evision_nif.bowTrainer_descriptorsCount(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns a training set of descriptors.

  ##### Positional Arguments
  - **self**: `Evision.BOWTrainer.t()`

  ##### Return
  - **retval**: `[Evision.Mat]`

  Python prototype (for reference only):
  ```python3
  getDescriptors() -> retval
  ```
  """
  @spec getDescriptors(Evision.BOWTrainer.t()) :: list(Evision.Mat.t()) | {:error, String.t()}
  def getDescriptors(self) do
    positional = [
    ]
    :evision_nif.bowTrainer_getDescriptors(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
