defmodule Evision.XFeatures2D do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `XFeatures2D` struct.

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
  def to_struct({:ok, %{class: Evision.XFeatures2D, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.XFeatures2D, ref: ref}) do
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
  GMS (Grid-based Motion Statistics) feature matching strategy described in @cite Bian2017gms .

  ##### Positional Arguments
  - **size1**: `Size`.

    Input size of image1.

  - **size2**: `Size`.

    Input size of image2.

  - **keypoints1**: `[Evision.KeyPoint]`.

    Input keypoints of image1.

  - **keypoints2**: `[Evision.KeyPoint]`.

    Input keypoints of image2.

  - **matches1to2**: `[Evision.DMatch]`.

    Input 1-nearest neighbor matches.

  ##### Keyword Arguments
  - **withRotation**: `bool`.

    Take rotation transformation into account.

  - **withScale**: `bool`.

    Take scale transformation into account.

  - **thresholdFactor**: `double`.

    The higher, the less matches.

  ##### Return
  - **matchesGMS**: `[Evision.DMatch]`.

    Matches returned by the GMS matching strategy.

  **Note**: 
  Since GMS works well when the number of features is large, we recommend to use the ORB feature and set FastThreshold to 0 to get as many as possible features quickly.
  If matching results are not satisfying, please add more features. (We use 10000 for images with 640 X 480).
  If your images have big rotation and scale changes, please set withRotation or withScale to true.

  Python prototype (for reference only):
  ```python3
  matchGMS(size1, size2, keypoints1, keypoints2, matches1to2[, withRotation[, withScale[, thresholdFactor]]]) -> matchesGMS
  ```
  """
  @spec matchGMS({number(), number()}, {number(), number()}, list(Evision.KeyPoint.t()), list(Evision.KeyPoint.t()), list(Evision.DMatch.t()), [{:thresholdFactor, term()} | {:withRotation, term()} | {:withScale, term()}] | nil) :: list(Evision.DMatch.t()) | {:error, String.t()}
  def matchGMS(size1, size2, keypoints1, keypoints2, matches1to2, opts) when is_tuple(size1) and is_tuple(size2) and is_list(keypoints1) and is_list(keypoints2) and is_list(matches1to2) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:thresholdFactor, :withRotation, :withScale])
    positional = [
      size1: Evision.Internal.Structurise.from_struct(size1),
      size2: Evision.Internal.Structurise.from_struct(size2),
      keypoints1: Evision.Internal.Structurise.from_struct(keypoints1),
      keypoints2: Evision.Internal.Structurise.from_struct(keypoints2),
      matches1to2: Evision.Internal.Structurise.from_struct(matches1to2)
    ]
    :evision_nif.xfeatures2d_matchGMS(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  GMS (Grid-based Motion Statistics) feature matching strategy described in @cite Bian2017gms .

  ##### Positional Arguments
  - **size1**: `Size`.

    Input size of image1.

  - **size2**: `Size`.

    Input size of image2.

  - **keypoints1**: `[Evision.KeyPoint]`.

    Input keypoints of image1.

  - **keypoints2**: `[Evision.KeyPoint]`.

    Input keypoints of image2.

  - **matches1to2**: `[Evision.DMatch]`.

    Input 1-nearest neighbor matches.

  ##### Keyword Arguments
  - **withRotation**: `bool`.

    Take rotation transformation into account.

  - **withScale**: `bool`.

    Take scale transformation into account.

  - **thresholdFactor**: `double`.

    The higher, the less matches.

  ##### Return
  - **matchesGMS**: `[Evision.DMatch]`.

    Matches returned by the GMS matching strategy.

  **Note**: 
  Since GMS works well when the number of features is large, we recommend to use the ORB feature and set FastThreshold to 0 to get as many as possible features quickly.
  If matching results are not satisfying, please add more features. (We use 10000 for images with 640 X 480).
  If your images have big rotation and scale changes, please set withRotation or withScale to true.

  Python prototype (for reference only):
  ```python3
  matchGMS(size1, size2, keypoints1, keypoints2, matches1to2[, withRotation[, withScale[, thresholdFactor]]]) -> matchesGMS
  ```
  """
  @spec matchGMS({number(), number()}, {number(), number()}, list(Evision.KeyPoint.t()), list(Evision.KeyPoint.t()), list(Evision.DMatch.t())) :: list(Evision.DMatch.t()) | {:error, String.t()}
  def matchGMS(size1, size2, keypoints1, keypoints2, matches1to2) when is_tuple(size1) and is_tuple(size2) and is_list(keypoints1) and is_list(keypoints2) and is_list(matches1to2)
  do
    positional = [
      size1: Evision.Internal.Structurise.from_struct(size1),
      size2: Evision.Internal.Structurise.from_struct(size2),
      keypoints1: Evision.Internal.Structurise.from_struct(keypoints1),
      keypoints2: Evision.Internal.Structurise.from_struct(keypoints2),
      matches1to2: Evision.Internal.Structurise.from_struct(matches1to2)
    ]
    :evision_nif.xfeatures2d_matchGMS(positional)
    |> to_struct()
  end

  @doc """
  LOGOS (Local geometric support for high-outlier spatial verification) feature matching strategy described in @cite Lowry2018LOGOSLG .

  ##### Positional Arguments
  - **keypoints1**: `[Evision.KeyPoint]`.

    Input keypoints of image1.

  - **keypoints2**: `[Evision.KeyPoint]`.

    Input keypoints of image2.

  - **nn1**: `[integer()]`.

    Index to the closest BoW centroid for each descriptors of image1.

  - **nn2**: `[integer()]`.

    Index to the closest BoW centroid for each descriptors of image2.

  ##### Return
  - **matches1to2**: `[Evision.DMatch]`.

    Matches returned by the LOGOS matching strategy.

  **Note**: 
  This matching strategy is suitable for features matching against large scale database.
  First step consists in constructing the bag-of-words (BoW) from a representative image database.
  Image descriptors are then represented by their closest codevector (nearest BoW centroid).

  Python prototype (for reference only):
  ```python3
  matchLOGOS(keypoints1, keypoints2, nn1, nn2) -> matches1to2
  ```
  """
  @spec matchLOGOS(list(Evision.KeyPoint.t()), list(Evision.KeyPoint.t()), list(integer()), list(integer())) :: list(Evision.DMatch.t()) | {:error, String.t()}
  def matchLOGOS(keypoints1, keypoints2, nn1, nn2) when is_list(keypoints1) and is_list(keypoints2) and is_list(nn1) and is_list(nn2)
  do
    positional = [
      keypoints1: Evision.Internal.Structurise.from_struct(keypoints1),
      keypoints2: Evision.Internal.Structurise.from_struct(keypoints2),
      nn1: Evision.Internal.Structurise.from_struct(nn1),
      nn2: Evision.Internal.Structurise.from_struct(nn2)
    ]
    :evision_nif.xfeatures2d_matchLOGOS(positional)
    |> to_struct()
  end
end
