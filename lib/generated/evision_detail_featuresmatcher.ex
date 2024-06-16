defmodule Evision.Detail.FeaturesMatcher do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Detail.FeaturesMatcher` struct.

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
  def to_struct({:ok, %{class: Evision.Detail.FeaturesMatcher, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Detail.FeaturesMatcher, ref: ref}) do
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
  - **self**: `Evision.Detail.FeaturesMatcher.t()`
  - **features1**: `Evision.Detail.ImageFeatures`.

    First image features

  - **features2**: `Evision.Detail.ImageFeatures`.

    Second image features

  ##### Return
  - **matches_info**: `Evision.Detail.MatchesInfo.t()`.

    Found matches

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  apply(features1, features2) -> matches_info
  ```
  """
  @spec apply(Evision.Detail.FeaturesMatcher.t(), Evision.Detail.ImageFeatures.t(), Evision.Detail.ImageFeatures.t()) :: Evision.Detail.MatchesInfo.t() | {:error, String.t()}
  def apply(self, features1, features2) when is_struct(features1, Evision.Detail.ImageFeatures) and is_struct(features2, Evision.Detail.ImageFeatures)
  do
    positional = [
      features1: Evision.Internal.Structurise.from_struct(features1),
      features2: Evision.Internal.Structurise.from_struct(features2)
    ]
    :evision_nif.detail_detail_FeaturesMatcher_apply(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Performs images matching.

  ##### Positional Arguments
  - **self**: `Evision.Detail.FeaturesMatcher.t()`
  - **features**: `[Evision.Detail.ImageFeatures]`.

    Features of the source images

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.

    Mask indicating which image pairs must be matched

  ##### Return
  - **pairwise_matches**: `[Evision.Detail.MatchesInfo]`.

    Found pairwise matches

  The function is parallelized with the TBB library.
  @sa detail::MatchesInfo

  Python prototype (for reference only):
  ```python3
  apply2(features[, mask]) -> pairwise_matches
  ```
  """
  @spec apply2(Evision.Detail.FeaturesMatcher.t(), list(Evision.Detail.ImageFeatures.t()), [{:mask, term()}] | nil) :: list(Evision.Detail.MatchesInfo.t()) | {:error, String.t()}
  def apply2(self, features, opts) when is_list(features) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:mask])
    positional = [
      features: Evision.Internal.Structurise.from_struct(features)
    ]
    :evision_nif.detail_detail_FeaturesMatcher_apply2(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Performs images matching.

  ##### Positional Arguments
  - **self**: `Evision.Detail.FeaturesMatcher.t()`
  - **features**: `[Evision.Detail.ImageFeatures]`.

    Features of the source images

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.

    Mask indicating which image pairs must be matched

  ##### Return
  - **pairwise_matches**: `[Evision.Detail.MatchesInfo]`.

    Found pairwise matches

  The function is parallelized with the TBB library.
  @sa detail::MatchesInfo

  Python prototype (for reference only):
  ```python3
  apply2(features[, mask]) -> pairwise_matches
  ```
  """
  @spec apply2(Evision.Detail.FeaturesMatcher.t(), list(Evision.Detail.ImageFeatures.t())) :: list(Evision.Detail.MatchesInfo.t()) | {:error, String.t()}
  def apply2(self, features) when is_list(features)
  do
    positional = [
      features: Evision.Internal.Structurise.from_struct(features)
    ]
    :evision_nif.detail_detail_FeaturesMatcher_apply2(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Frees unused memory allocated before if there is any.

  ##### Positional Arguments
  - **self**: `Evision.Detail.FeaturesMatcher.t()`

  Python prototype (for reference only):
  ```python3
  collectGarbage() -> None
  ```
  """
  @spec collectGarbage(Evision.Detail.FeaturesMatcher.t()) :: Evision.Detail.FeaturesMatcher.t() | {:error, String.t()}
  def collectGarbage(self) do
    positional = [
    ]
    :evision_nif.detail_detail_FeaturesMatcher_collectGarbage(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  isThreadSafe

  ##### Positional Arguments
  - **self**: `Evision.Detail.FeaturesMatcher.t()`

  ##### Return
  - **retval**: `bool`

  @return True, if it's possible to use the same matcher instance in parallel, false otherwise

  Python prototype (for reference only):
  ```python3
  isThreadSafe() -> retval
  ```
  """
  @spec isThreadSafe(Evision.Detail.FeaturesMatcher.t()) :: boolean() | {:error, String.t()}
  def isThreadSafe(self) do
    positional = [
    ]
    :evision_nif.detail_detail_FeaturesMatcher_isThreadSafe(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
