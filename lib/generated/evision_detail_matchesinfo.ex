defmodule Evision.Detail.MatchesInfo do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Detail.MatchesInfo` struct.

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
  def to_struct({:ok, %{class: Evision.Detail.MatchesInfo, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Detail.MatchesInfo, ref: ref}) do
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
  getInliers

  ##### Positional Arguments
  - **self**: `Evision.Detail.MatchesInfo.t()`

  ##### Return
  - **retval**: `[uchar]`

  Python prototype (for reference only):
  ```python3
  getInliers() -> retval
  ```
  """
  @spec getInliers(Evision.Detail.MatchesInfo.t()) :: binary() | {:error, String.t()}
  def getInliers(self) do
    positional = [
    ]
    :evision_nif.detail_detail_MatchesInfo_getInliers(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMatches

  ##### Positional Arguments
  - **self**: `Evision.Detail.MatchesInfo.t()`

  ##### Return
  - **retval**: `[Evision.DMatch]`

  Python prototype (for reference only):
  ```python3
  getMatches() -> retval
  ```
  """
  @spec getMatches(Evision.Detail.MatchesInfo.t()) :: list(Evision.DMatch.t()) | {:error, String.t()}
  def getMatches(self) do
    positional = [
    ]
    :evision_nif.detail_detail_MatchesInfo_getMatches(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec get_H(Evision.Detail.MatchesInfo.t()) :: Evision.Mat.t()
  def get_H(self) do
    :evision_nif.detail_MatchesInfo_get_H(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_H(Evision.Detail.MatchesInfo.t(), Evision.Mat.maybe_mat_in()) :: Evision.Detail.MatchesInfo.t()
  def set_H(self, prop) do
    :evision_nif.detail_MatchesInfo_set_H(
        Evision.Internal.Structurise.from_struct(self),
        [H: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_confidence(Evision.Detail.MatchesInfo.t()) :: number()
  def get_confidence(self) do
    :evision_nif.detail_MatchesInfo_get_confidence(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_confidence(Evision.Detail.MatchesInfo.t(), number()) :: Evision.Detail.MatchesInfo.t()
  def set_confidence(self, prop) do
    :evision_nif.detail_MatchesInfo_set_confidence(
        Evision.Internal.Structurise.from_struct(self),
        [confidence: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_dst_img_idx(Evision.Detail.MatchesInfo.t()) :: integer()
  def get_dst_img_idx(self) do
    :evision_nif.detail_MatchesInfo_get_dst_img_idx(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_dst_img_idx(Evision.Detail.MatchesInfo.t(), integer()) :: Evision.Detail.MatchesInfo.t()
  def set_dst_img_idx(self, prop) do
    :evision_nif.detail_MatchesInfo_set_dst_img_idx(
        Evision.Internal.Structurise.from_struct(self),
        [dst_img_idx: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_inliers_mask(Evision.Detail.MatchesInfo.t()) :: binary()
  def get_inliers_mask(self) do
    :evision_nif.detail_MatchesInfo_get_inliers_mask(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_inliers_mask(Evision.Detail.MatchesInfo.t(), binary()) :: Evision.Detail.MatchesInfo.t()
  def set_inliers_mask(self, prop) do
    :evision_nif.detail_MatchesInfo_set_inliers_mask(
        Evision.Internal.Structurise.from_struct(self),
        [inliers_mask: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_matches(Evision.Detail.MatchesInfo.t()) :: list(Evision.DMatch.t())
  def get_matches(self) do
    :evision_nif.detail_MatchesInfo_get_matches(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_matches(Evision.Detail.MatchesInfo.t(), list(Evision.DMatch.t())) :: Evision.Detail.MatchesInfo.t()
  def set_matches(self, prop) do
    :evision_nif.detail_MatchesInfo_set_matches(
        Evision.Internal.Structurise.from_struct(self),
        [matches: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_num_inliers(Evision.Detail.MatchesInfo.t()) :: integer()
  def get_num_inliers(self) do
    :evision_nif.detail_MatchesInfo_get_num_inliers(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_num_inliers(Evision.Detail.MatchesInfo.t(), integer()) :: Evision.Detail.MatchesInfo.t()
  def set_num_inliers(self, prop) do
    :evision_nif.detail_MatchesInfo_set_num_inliers(
        Evision.Internal.Structurise.from_struct(self),
        [num_inliers: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_src_img_idx(Evision.Detail.MatchesInfo.t()) :: integer()
  def get_src_img_idx(self) do
    :evision_nif.detail_MatchesInfo_get_src_img_idx(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_src_img_idx(Evision.Detail.MatchesInfo.t(), integer()) :: Evision.Detail.MatchesInfo.t()
  def set_src_img_idx(self, prop) do
    :evision_nif.detail_MatchesInfo_set_src_img_idx(
        Evision.Internal.Structurise.from_struct(self),
        [src_img_idx: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
end
