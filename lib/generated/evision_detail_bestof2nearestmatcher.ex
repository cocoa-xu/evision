defmodule Evision.Detail.BestOf2NearestMatcher do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Detail.BestOf2NearestMatcher` struct.

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
  def to_struct({:ok, %{class: Evision.Detail.BestOf2NearestMatcher, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Detail.BestOf2NearestMatcher, ref: ref}) do
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
  Constructs a "best of 2 nearest" matcher.
  ##### Keyword Arguments
  - **try_use_gpu**: `bool`.

    Should try to use GPU or not

  - **match_conf**: `float`.

    Match distances ration threshold

  - **num_matches_thresh1**: `integer()`.

    Minimum number of matches required for the 2D projective transform
    estimation used in the inliers classification step

  - **num_matches_thresh2**: `integer()`.

    Minimum number of matches required for the 2D projective transform
    re-estimation on inliers

  - **matches_confindece_thresh**: `double`.

    Matching confidence threshold to take the match into account.
    The threshold was determined experimentally and set to 3 by default.

  ##### Return
  - **self**: `Evision.Detail.BestOf2NearestMatcher.t()`

  Python prototype (for reference only):
  ```python3
  BestOf2NearestMatcher([, try_use_gpu[, match_conf[, num_matches_thresh1[, num_matches_thresh2[, matches_confindece_thresh]]]]]) -> <detail_BestOf2NearestMatcher object>
  ```
  """
  @spec bestOf2NearestMatcher([{:match_conf, term()} | {:matches_confindece_thresh, term()} | {:num_matches_thresh1, term()} | {:num_matches_thresh2, term()} | {:try_use_gpu, term()}] | nil) :: Evision.Detail.BestOf2NearestMatcher.t() | {:error, String.t()}
  def bestOf2NearestMatcher(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:match_conf, :matches_confindece_thresh, :num_matches_thresh1, :num_matches_thresh2, :try_use_gpu])
    positional = [
    ]
    :evision_nif.detail_detail_BestOf2NearestMatcher_BestOf2NearestMatcher(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Constructs a "best of 2 nearest" matcher.
  ##### Keyword Arguments
  - **try_use_gpu**: `bool`.

    Should try to use GPU or not

  - **match_conf**: `float`.

    Match distances ration threshold

  - **num_matches_thresh1**: `integer()`.

    Minimum number of matches required for the 2D projective transform
    estimation used in the inliers classification step

  - **num_matches_thresh2**: `integer()`.

    Minimum number of matches required for the 2D projective transform
    re-estimation on inliers

  - **matches_confindece_thresh**: `double`.

    Matching confidence threshold to take the match into account.
    The threshold was determined experimentally and set to 3 by default.

  ##### Return
  - **self**: `Evision.Detail.BestOf2NearestMatcher.t()`

  Python prototype (for reference only):
  ```python3
  BestOf2NearestMatcher([, try_use_gpu[, match_conf[, num_matches_thresh1[, num_matches_thresh2[, matches_confindece_thresh]]]]]) -> <detail_BestOf2NearestMatcher object>
  ```
  """
  @spec bestOf2NearestMatcher() :: Evision.Detail.BestOf2NearestMatcher.t() | {:error, String.t()}
  def bestOf2NearestMatcher() do
    positional = [
    ]
    :evision_nif.detail_detail_BestOf2NearestMatcher_BestOf2NearestMatcher(positional)
    |> to_struct()
  end

  @doc """
  apply

  ##### Positional Arguments
  - **self**: `Evision.Detail.BestOf2NearestMatcher.t()`
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
  @spec apply(Evision.Detail.BestOf2NearestMatcher.t(), Evision.Detail.ImageFeatures.t(), Evision.Detail.ImageFeatures.t()) :: Evision.Detail.MatchesInfo.t() | {:error, String.t()}
  def apply(self, features1, features2) when is_struct(features1, Evision.Detail.ImageFeatures) and is_struct(features2, Evision.Detail.ImageFeatures)
  do
    positional = [
      features1: Evision.Internal.Structurise.from_struct(features1),
      features2: Evision.Internal.Structurise.from_struct(features2)
    ]
    :evision_nif.detail_detail_BestOf2NearestMatcher_apply(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Performs images matching.

  ##### Positional Arguments
  - **self**: `Evision.Detail.BestOf2NearestMatcher.t()`
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
  @spec apply2(Evision.Detail.BestOf2NearestMatcher.t(), list(Evision.Detail.ImageFeatures.t()), [{:mask, term()}] | nil) :: list(Evision.Detail.MatchesInfo.t()) | {:error, String.t()}
  def apply2(self, features, opts) when is_list(features) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:mask])
    positional = [
      features: Evision.Internal.Structurise.from_struct(features)
    ]
    :evision_nif.detail_detail_BestOf2NearestMatcher_apply2(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Performs images matching.

  ##### Positional Arguments
  - **self**: `Evision.Detail.BestOf2NearestMatcher.t()`
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
  @spec apply2(Evision.Detail.BestOf2NearestMatcher.t(), list(Evision.Detail.ImageFeatures.t())) :: list(Evision.Detail.MatchesInfo.t()) | {:error, String.t()}
  def apply2(self, features) when is_list(features)
  do
    positional = [
      features: Evision.Internal.Structurise.from_struct(features)
    ]
    :evision_nif.detail_detail_BestOf2NearestMatcher_apply2(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  collectGarbage

  ##### Positional Arguments
  - **self**: `Evision.Detail.BestOf2NearestMatcher.t()`

  Python prototype (for reference only):
  ```python3
  collectGarbage() -> None
  ```
  """
  @spec collectGarbage(Evision.Detail.BestOf2NearestMatcher.t()) :: Evision.Detail.BestOf2NearestMatcher.t() | {:error, String.t()}
  def collectGarbage(self) do
    positional = [
    ]
    :evision_nif.detail_detail_BestOf2NearestMatcher_collectGarbage(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  create
  ##### Keyword Arguments
  - **try_use_gpu**: `bool`.
  - **match_conf**: `float`.
  - **num_matches_thresh1**: `integer()`.
  - **num_matches_thresh2**: `integer()`.
  - **matches_confindece_thresh**: `double`.

  ##### Return
  - **retval**: `Evision.Detail.BestOf2NearestMatcher.t()`

  Python prototype (for reference only):
  ```python3
  create([, try_use_gpu[, match_conf[, num_matches_thresh1[, num_matches_thresh2[, matches_confindece_thresh]]]]]) -> retval
  ```
  """
  @spec create([{:match_conf, term()} | {:matches_confindece_thresh, term()} | {:num_matches_thresh1, term()} | {:num_matches_thresh2, term()} | {:try_use_gpu, term()}] | nil) :: Evision.Detail.BestOf2NearestMatcher.t() | {:error, String.t()}
  def create(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:match_conf, :matches_confindece_thresh, :num_matches_thresh1, :num_matches_thresh2, :try_use_gpu])
    positional = [
    ]
    :evision_nif.detail_detail_BestOf2NearestMatcher_create_static(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  create
  ##### Keyword Arguments
  - **try_use_gpu**: `bool`.
  - **match_conf**: `float`.
  - **num_matches_thresh1**: `integer()`.
  - **num_matches_thresh2**: `integer()`.
  - **matches_confindece_thresh**: `double`.

  ##### Return
  - **retval**: `Evision.Detail.BestOf2NearestMatcher.t()`

  Python prototype (for reference only):
  ```python3
  create([, try_use_gpu[, match_conf[, num_matches_thresh1[, num_matches_thresh2[, matches_confindece_thresh]]]]]) -> retval
  ```
  """
  @spec create() :: Evision.Detail.BestOf2NearestMatcher.t() | {:error, String.t()}
  def create() do
    positional = [
    ]
    :evision_nif.detail_detail_BestOf2NearestMatcher_create_static(positional)
    |> to_struct()
  end

  @doc """
  isThreadSafe

  ##### Positional Arguments
  - **self**: `Evision.Detail.BestOf2NearestMatcher.t()`

  ##### Return
  - **retval**: `bool`

  @return True, if it's possible to use the same matcher instance in parallel, false otherwise

  Python prototype (for reference only):
  ```python3
  isThreadSafe() -> retval
  ```
  """
  @spec isThreadSafe(Evision.Detail.BestOf2NearestMatcher.t()) :: boolean() | {:error, String.t()}
  def isThreadSafe(self) do
    positional = [
    ]
    :evision_nif.detail_detail_BestOf2NearestMatcher_isThreadSafe(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
