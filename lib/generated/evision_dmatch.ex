defmodule Evision.DMatch do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `DMatch` struct.

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
  def to_struct({:ok, %{class: Evision.DMatch, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.DMatch, ref: ref}) do
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
  DMatch

  ##### Positional Arguments
  - **queryIdx**: `integer()`
  - **trainIdx**: `integer()`
  - **imgIdx**: `integer()`
  - **distance**: `float`

  ##### Return
  - **self**: `Evision.DMatch.t()`

  Python prototype (for reference only):
  ```python3
  DMatch(_queryIdx, _trainIdx, _imgIdx, _distance) -> <DMatch object>
  ```
  """
  @spec dMatcher(integer(), integer(), integer(), number()) :: Evision.DMatch.t() | {:error, String.t()}
  def dMatcher(queryIdx, trainIdx, imgIdx, distance) when is_integer(queryIdx) and is_integer(trainIdx) and is_integer(imgIdx) and is_float(distance)
  do
    positional = [
      queryIdx: Evision.Internal.Structurise.from_struct(queryIdx),
      trainIdx: Evision.Internal.Structurise.from_struct(trainIdx),
      imgIdx: Evision.Internal.Structurise.from_struct(imgIdx),
      distance: Evision.Internal.Structurise.from_struct(distance)
    ]
    :evision_nif.dMatch_DMatch(positional)
    |> to_struct()
  end

  @doc """
  DMatch

  ##### Positional Arguments
  - **queryIdx**: `integer()`
  - **trainIdx**: `integer()`
  - **distance**: `float`

  ##### Return
  - **self**: `Evision.DMatch.t()`

  Python prototype (for reference only):
  ```python3
  DMatch(_queryIdx, _trainIdx, _distance) -> <DMatch object>
  ```
  """
  @spec dMatcher(integer(), integer(), number()) :: Evision.DMatch.t() | {:error, String.t()}
  def dMatcher(queryIdx, trainIdx, distance) when is_integer(queryIdx) and is_integer(trainIdx) and is_float(distance)
  do
    positional = [
      queryIdx: Evision.Internal.Structurise.from_struct(queryIdx),
      trainIdx: Evision.Internal.Structurise.from_struct(trainIdx),
      distance: Evision.Internal.Structurise.from_struct(distance)
    ]
    :evision_nif.dMatch_DMatch(positional)
    |> to_struct()
  end

  @doc """
  DMatch
  ##### Return
  - **self**: `Evision.DMatch.t()`

  Python prototype (for reference only):
  ```python3
  DMatch() -> <DMatch object>
  ```
  """
  @spec dMatcher() :: Evision.DMatch.t() | {:error, String.t()}
  def dMatcher() do
    positional = [
    ]
    :evision_nif.dMatch_DMatch(positional)
    |> to_struct()
  end
  @spec get_distance(Evision.DMatch.t()) :: number()
  def get_distance(self) do
    :evision_nif.dMatch_get_distance(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_distance(Evision.DMatch.t(), number()) :: Evision.DMatch.t()
  def set_distance(self, prop) do
    :evision_nif.dMatch_set_distance(
        Evision.Internal.Structurise.from_struct(self),
        [distance: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_imgIdx(Evision.DMatch.t()) :: integer()
  def get_imgIdx(self) do
    :evision_nif.dMatch_get_imgIdx(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_imgIdx(Evision.DMatch.t(), integer()) :: Evision.DMatch.t()
  def set_imgIdx(self, prop) do
    :evision_nif.dMatch_set_imgIdx(
        Evision.Internal.Structurise.from_struct(self),
        [imgIdx: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_queryIdx(Evision.DMatch.t()) :: integer()
  def get_queryIdx(self) do
    :evision_nif.dMatch_get_queryIdx(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_queryIdx(Evision.DMatch.t(), integer()) :: Evision.DMatch.t()
  def set_queryIdx(self, prop) do
    :evision_nif.dMatch_set_queryIdx(
        Evision.Internal.Structurise.from_struct(self),
        [queryIdx: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_trainIdx(Evision.DMatch.t()) :: integer()
  def get_trainIdx(self) do
    :evision_nif.dMatch_get_trainIdx(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_trainIdx(Evision.DMatch.t(), integer()) :: Evision.DMatch.t()
  def set_trainIdx(self, prop) do
    :evision_nif.dMatch_set_trainIdx(
        Evision.Internal.Structurise.from_struct(self),
        [trainIdx: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
end
