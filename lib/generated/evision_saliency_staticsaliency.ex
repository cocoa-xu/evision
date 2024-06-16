defmodule Evision.Saliency.StaticSaliency do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Saliency.StaticSaliency` struct.

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
  def to_struct({:ok, %{class: Evision.Saliency.StaticSaliency, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Saliency.StaticSaliency, ref: ref}) do
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
  This function perform a binary map of given saliency map. This is obtained in this
  way:

  ##### Positional Arguments
  - **self**: `Evision.Saliency.StaticSaliency.t()`
  - **saliencyMap**: `Evision.Mat`.

    the saliency map obtained through one of the specialized algorithms

  ##### Return
  - **retval**: `bool`
  - **binaryMap**: `Evision.Mat.t()`.

    the binary map

  In a first step, to improve the definition of interest areas and facilitate identification of
  targets, a segmentation by clustering is performed, using *K-means algorithm*. Then, to gain a
  binary representation of clustered saliency map, since values of the map can vary according to
  the characteristics of frame under analysis, it is not convenient to use a fixed threshold. So,
  Otsu's algorithm* is used, which assumes that the image to be thresholded contains two classes
  of pixels or bi-modal histograms (e.g. foreground and back-ground pixels); later on, the
  algorithm calculates the optimal threshold separating those two classes, so that their
  intra-class variance is minimal.

  Python prototype (for reference only):
  ```python3
  computeBinaryMap(_saliencyMap[, _binaryMap]) -> retval, _binaryMap
  ```
  """
  @spec computeBinaryMap(Evision.Saliency.StaticSaliency.t(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | false | {:error, String.t()}
  def computeBinaryMap(self, saliencyMap, opts) when (is_struct(saliencyMap, Evision.Mat) or is_struct(saliencyMap, Nx.Tensor) or is_number(saliencyMap) or is_tuple(saliencyMap)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      saliencyMap: Evision.Internal.Structurise.from_struct(saliencyMap)
    ]
    :evision_nif.saliency_saliency_StaticSaliency_computeBinaryMap(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  This function perform a binary map of given saliency map. This is obtained in this
  way:

  ##### Positional Arguments
  - **self**: `Evision.Saliency.StaticSaliency.t()`
  - **saliencyMap**: `Evision.Mat`.

    the saliency map obtained through one of the specialized algorithms

  ##### Return
  - **retval**: `bool`
  - **binaryMap**: `Evision.Mat.t()`.

    the binary map

  In a first step, to improve the definition of interest areas and facilitate identification of
  targets, a segmentation by clustering is performed, using *K-means algorithm*. Then, to gain a
  binary representation of clustered saliency map, since values of the map can vary according to
  the characteristics of frame under analysis, it is not convenient to use a fixed threshold. So,
  Otsu's algorithm* is used, which assumes that the image to be thresholded contains two classes
  of pixels or bi-modal histograms (e.g. foreground and back-ground pixels); later on, the
  algorithm calculates the optimal threshold separating those two classes, so that their
  intra-class variance is minimal.

  Python prototype (for reference only):
  ```python3
  computeBinaryMap(_saliencyMap[, _binaryMap]) -> retval, _binaryMap
  ```
  """
  @spec computeBinaryMap(Evision.Saliency.StaticSaliency.t(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | false | {:error, String.t()}
  def computeBinaryMap(self, saliencyMap) when (is_struct(saliencyMap, Evision.Mat) or is_struct(saliencyMap, Nx.Tensor) or is_number(saliencyMap) or is_tuple(saliencyMap))
  do
    positional = [
      saliencyMap: Evision.Internal.Structurise.from_struct(saliencyMap)
    ]
    :evision_nif.saliency_saliency_StaticSaliency_computeBinaryMap(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
