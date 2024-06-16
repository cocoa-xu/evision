defmodule Evision.Saliency.StaticSaliencyFineGrained do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Saliency.StaticSaliencyFineGrained` struct.

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
  def to_struct({:ok, %{class: Evision.Saliency.StaticSaliencyFineGrained, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Saliency.StaticSaliencyFineGrained, ref: ref}) do
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
  computeSaliency

  ##### Positional Arguments
  - **self**: `Evision.Saliency.StaticSaliencyFineGrained.t()`
  - **image**: `Evision.Mat`

  ##### Return
  - **retval**: `bool`
  - **saliencyMap**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  computeSaliency(image[, saliencyMap]) -> retval, saliencyMap
  ```
  """
  @spec computeSaliency(Evision.Saliency.StaticSaliencyFineGrained.t(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | false | {:error, String.t()}
  def computeSaliency(self, image, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.saliency_saliency_StaticSaliencyFineGrained_computeSaliency(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  computeSaliency

  ##### Positional Arguments
  - **self**: `Evision.Saliency.StaticSaliencyFineGrained.t()`
  - **image**: `Evision.Mat`

  ##### Return
  - **retval**: `bool`
  - **saliencyMap**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  computeSaliency(image[, saliencyMap]) -> retval, saliencyMap
  ```
  """
  @spec computeSaliency(Evision.Saliency.StaticSaliencyFineGrained.t(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | false | {:error, String.t()}
  def computeSaliency(self, image) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.saliency_saliency_StaticSaliencyFineGrained_computeSaliency(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  create
  ##### Return
  - **retval**: `StaticSaliencyFineGrained`

  Python prototype (for reference only):
  ```python3
  create() -> retval
  ```
  """
  @spec create() :: Evision.Saliency.StaticSaliencyFineGrained.t() | {:error, String.t()}
  def create() do
    positional = [
    ]
    :evision_nif.saliency_saliency_StaticSaliencyFineGrained_create_static(positional)
    |> to_struct()
  end
end
