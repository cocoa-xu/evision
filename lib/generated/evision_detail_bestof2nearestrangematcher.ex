defmodule Evision.Detail.BestOf2NearestRangeMatcher do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Detail.BestOf2NearestRangeMatcher` struct.

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
  def to_struct({:ok, %{class: Evision.Detail.BestOf2NearestRangeMatcher, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Detail.BestOf2NearestRangeMatcher, ref: ref}) do
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
  BestOf2NearestRangeMatcher
  ##### Keyword Arguments
  - **range_width**: `integer()`.
  - **try_use_gpu**: `bool`.
  - **match_conf**: `float`.
  - **num_matches_thresh1**: `integer()`.
  - **num_matches_thresh2**: `integer()`.

  ##### Return
  - **self**: `Evision.Detail.BestOf2NearestRangeMatcher.t()`

  Python prototype (for reference only):
  ```python3
  BestOf2NearestRangeMatcher([, range_width[, try_use_gpu[, match_conf[, num_matches_thresh1[, num_matches_thresh2]]]]]) -> <detail_BestOf2NearestRangeMatcher object>
  ```
  """
  @spec bestOf2NearestRangeMatcher([{:match_conf, term()} | {:num_matches_thresh1, term()} | {:num_matches_thresh2, term()} | {:range_width, term()} | {:try_use_gpu, term()}] | nil) :: Evision.Detail.BestOf2NearestRangeMatcher.t() | {:error, String.t()}
  def bestOf2NearestRangeMatcher(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:match_conf, :num_matches_thresh1, :num_matches_thresh2, :range_width, :try_use_gpu])
    positional = [
    ]
    :evision_nif.detail_detail_BestOf2NearestRangeMatcher_BestOf2NearestRangeMatcher(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  BestOf2NearestRangeMatcher
  ##### Keyword Arguments
  - **range_width**: `integer()`.
  - **try_use_gpu**: `bool`.
  - **match_conf**: `float`.
  - **num_matches_thresh1**: `integer()`.
  - **num_matches_thresh2**: `integer()`.

  ##### Return
  - **self**: `Evision.Detail.BestOf2NearestRangeMatcher.t()`

  Python prototype (for reference only):
  ```python3
  BestOf2NearestRangeMatcher([, range_width[, try_use_gpu[, match_conf[, num_matches_thresh1[, num_matches_thresh2]]]]]) -> <detail_BestOf2NearestRangeMatcher object>
  ```
  """
  @spec bestOf2NearestRangeMatcher() :: Evision.Detail.BestOf2NearestRangeMatcher.t() | {:error, String.t()}
  def bestOf2NearestRangeMatcher() do
    positional = [
    ]
    :evision_nif.detail_detail_BestOf2NearestRangeMatcher_BestOf2NearestRangeMatcher(positional)
    |> to_struct()
  end

  @doc """
  apply

  ##### Positional Arguments
  - **self**: `Evision.Detail.BestOf2NearestRangeMatcher.t()`
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
  @spec apply(Evision.Detail.BestOf2NearestRangeMatcher.t(), Evision.Detail.ImageFeatures.t(), Evision.Detail.ImageFeatures.t()) :: Evision.Detail.MatchesInfo.t() | {:error, String.t()}
  def apply(self, features1, features2) when is_struct(features1, Evision.Detail.ImageFeatures) and is_struct(features2, Evision.Detail.ImageFeatures)
  do
    positional = [
      features1: Evision.Internal.Structurise.from_struct(features1),
      features2: Evision.Internal.Structurise.from_struct(features2)
    ]
    :evision_nif.detail_detail_BestOf2NearestRangeMatcher_apply(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Performs images matching.

  ##### Positional Arguments
  - **self**: `Evision.Detail.BestOf2NearestRangeMatcher.t()`
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
  @spec apply2(Evision.Detail.BestOf2NearestRangeMatcher.t(), list(Evision.Detail.ImageFeatures.t()), [{:mask, term()}] | nil) :: list(Evision.Detail.MatchesInfo.t()) | {:error, String.t()}
  def apply2(self, features, opts) when is_list(features) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:mask])
    positional = [
      features: Evision.Internal.Structurise.from_struct(features)
    ]
    :evision_nif.detail_detail_BestOf2NearestRangeMatcher_apply2(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Performs images matching.

  ##### Positional Arguments
  - **self**: `Evision.Detail.BestOf2NearestRangeMatcher.t()`
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
  @spec apply2(Evision.Detail.BestOf2NearestRangeMatcher.t(), list(Evision.Detail.ImageFeatures.t())) :: list(Evision.Detail.MatchesInfo.t()) | {:error, String.t()}
  def apply2(self, features) when is_list(features)
  do
    positional = [
      features: Evision.Internal.Structurise.from_struct(features)
    ]
    :evision_nif.detail_detail_BestOf2NearestRangeMatcher_apply2(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  collectGarbage

  ##### Positional Arguments
  - **self**: `Evision.Detail.BestOf2NearestRangeMatcher.t()`

  Python prototype (for reference only):
  ```python3
  collectGarbage() -> None
  ```
  """
  @spec collectGarbage(Evision.Detail.BestOf2NearestRangeMatcher.t()) :: Evision.Detail.BestOf2NearestRangeMatcher.t() | {:error, String.t()}
  def collectGarbage(self) do
    positional = [
    ]
    :evision_nif.detail_detail_BestOf2NearestRangeMatcher_collectGarbage(Evision.Internal.Structurise.from_struct(self), positional)
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
    :evision_nif.detail_detail_BestOf2NearestRangeMatcher_create_static(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
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
    :evision_nif.detail_detail_BestOf2NearestRangeMatcher_create_static(positional)
    |> to_struct()
  end

  @doc """
  isThreadSafe

  ##### Positional Arguments
  - **self**: `Evision.Detail.BestOf2NearestRangeMatcher.t()`

  ##### Return
  - **retval**: `bool`

  @return True, if it's possible to use the same matcher instance in parallel, false otherwise

  Python prototype (for reference only):
  ```python3
  isThreadSafe() -> retval
  ```
  """
  @spec isThreadSafe(Evision.Detail.BestOf2NearestRangeMatcher.t()) :: boolean() | {:error, String.t()}
  def isThreadSafe(self) do
    positional = [
    ]
    :evision_nif.detail_detail_BestOf2NearestRangeMatcher_isThreadSafe(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
