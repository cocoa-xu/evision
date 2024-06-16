defmodule Evision.BOWKMeansTrainer do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `BOWKMeansTrainer` struct.

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
  def to_struct({:ok, %{class: Evision.BOWKMeansTrainer, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.BOWKMeansTrainer, ref: ref}) do
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
  The constructor.

  ##### Positional Arguments
  - **clusterCount**: `integer()`

  ##### Keyword Arguments
  - **termcrit**: `TermCriteria`.
  - **attempts**: `integer()`.
  - **flags**: `integer()`.

  ##### Return
  - **self**: `Evision.BOWKMeansTrainer.t()`

  @see cv::kmeans

  Python prototype (for reference only):
  ```python3
  BOWKMeansTrainer(clusterCount[, termcrit[, attempts[, flags]]]) -> <BOWKMeansTrainer object>
  ```
  """
  @spec bowKMeansTrainer(integer(), [{:attempts, term()} | {:flags, term()} | {:termcrit, term()}] | nil) :: Evision.BOWKMeansTrainer.t() | {:error, String.t()}
  def bowKMeansTrainer(clusterCount, opts) when is_integer(clusterCount) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:attempts, :flags, :termcrit])
    positional = [
      clusterCount: Evision.Internal.Structurise.from_struct(clusterCount)
    ]
    :evision_nif.bowKMeansTrainer_BOWKMeansTrainer(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  The constructor.

  ##### Positional Arguments
  - **clusterCount**: `integer()`

  ##### Keyword Arguments
  - **termcrit**: `TermCriteria`.
  - **attempts**: `integer()`.
  - **flags**: `integer()`.

  ##### Return
  - **self**: `Evision.BOWKMeansTrainer.t()`

  @see cv::kmeans

  Python prototype (for reference only):
  ```python3
  BOWKMeansTrainer(clusterCount[, termcrit[, attempts[, flags]]]) -> <BOWKMeansTrainer object>
  ```
  """
  @spec bowKMeansTrainer(integer()) :: Evision.BOWKMeansTrainer.t() | {:error, String.t()}
  def bowKMeansTrainer(clusterCount) when is_integer(clusterCount)
  do
    positional = [
      clusterCount: Evision.Internal.Structurise.from_struct(clusterCount)
    ]
    :evision_nif.bowKMeansTrainer_BOWKMeansTrainer(positional)
    |> to_struct()
  end

  @doc """
  cluster

  ##### Positional Arguments
  - **self**: `Evision.BOWKMeansTrainer.t()`
  - **descriptors**: `Evision.Mat`

  ##### Return
  - **retval**: `Evision.Mat.t()`

  Python prototype (for reference only):
  ```python3
  cluster(descriptors) -> retval
  ```
  """
  @spec cluster(Evision.BOWKMeansTrainer.t(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def cluster(self, descriptors) when (is_struct(descriptors, Evision.Mat) or is_struct(descriptors, Nx.Tensor) or is_number(descriptors) or is_tuple(descriptors))
  do
    positional = [
      descriptors: Evision.Internal.Structurise.from_struct(descriptors)
    ]
    :evision_nif.bowKMeansTrainer_cluster(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  cluster

  ##### Positional Arguments
  - **self**: `Evision.BOWKMeansTrainer.t()`

  ##### Return
  - **retval**: `Evision.Mat.t()`

  Python prototype (for reference only):
  ```python3
  cluster() -> retval
  ```
  """
  @spec cluster(Evision.BOWKMeansTrainer.t()) :: Evision.Mat.t() | {:error, String.t()}
  def cluster(self) do
    positional = [
    ]
    :evision_nif.bowKMeansTrainer_cluster(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
