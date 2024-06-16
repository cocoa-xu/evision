defmodule Evision.KeyPoint do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `KeyPoint` struct.

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
  def to_struct({:ok, %{class: Evision.KeyPoint, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.KeyPoint, ref: ref}) do
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
  KeyPoint

  ##### Positional Arguments
  - **x**: `float`.

    x-coordinate of the keypoint

  - **y**: `float`.

    y-coordinate of the keypoint

  - **size**: `float`.

    keypoint diameter

  ##### Keyword Arguments
  - **angle**: `float`.

    keypoint orientation

  - **response**: `float`.

    keypoint detector response on the keypoint (that is, strength of the keypoint)

  - **octave**: `integer()`.

    pyramid octave in which the keypoint has been detected

  - **class_id**: `integer()`.

    object id

  ##### Return
  - **self**: `Evision.KeyPoint.t()`

  Python prototype (for reference only):
  ```python3
  KeyPoint(x, y, size[, angle[, response[, octave[, class_id]]]]) -> <KeyPoint object>
  ```
  """
  @spec keyPoint(number(), number(), number(), [{:angle, term()} | {:class_id, term()} | {:octave, term()} | {:response, term()}] | nil) :: Evision.KeyPoint.t() | {:error, String.t()}
  def keyPoint(x, y, size, opts) when is_float(x) and is_float(y) and is_float(size) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:angle, :class_id, :octave, :response])
    positional = [
      x: Evision.Internal.Structurise.from_struct(x),
      y: Evision.Internal.Structurise.from_struct(y),
      size: Evision.Internal.Structurise.from_struct(size)
    ]
    :evision_nif.keyPoint_KeyPoint(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  KeyPoint

  ##### Positional Arguments
  - **x**: `float`.

    x-coordinate of the keypoint

  - **y**: `float`.

    y-coordinate of the keypoint

  - **size**: `float`.

    keypoint diameter

  ##### Keyword Arguments
  - **angle**: `float`.

    keypoint orientation

  - **response**: `float`.

    keypoint detector response on the keypoint (that is, strength of the keypoint)

  - **octave**: `integer()`.

    pyramid octave in which the keypoint has been detected

  - **class_id**: `integer()`.

    object id

  ##### Return
  - **self**: `Evision.KeyPoint.t()`

  Python prototype (for reference only):
  ```python3
  KeyPoint(x, y, size[, angle[, response[, octave[, class_id]]]]) -> <KeyPoint object>
  ```
  """
  @spec keyPoint(number(), number(), number()) :: Evision.KeyPoint.t() | {:error, String.t()}
  def keyPoint(x, y, size) when is_float(x) and is_float(y) and is_float(size)
  do
    positional = [
      x: Evision.Internal.Structurise.from_struct(x),
      y: Evision.Internal.Structurise.from_struct(y),
      size: Evision.Internal.Structurise.from_struct(size)
    ]
    :evision_nif.keyPoint_KeyPoint(positional)
    |> to_struct()
  end

  @doc """
  KeyPoint
  ##### Return
  - **self**: `Evision.KeyPoint.t()`

  Python prototype (for reference only):
  ```python3
  KeyPoint() -> <KeyPoint object>
  ```
  """
  @spec keyPoint() :: Evision.KeyPoint.t() | {:error, String.t()}
  def keyPoint() do
    positional = [
    ]
    :evision_nif.keyPoint_KeyPoint(positional)
    |> to_struct()
  end

  @doc """
  convert

  ##### Positional Arguments
  - **points2f**: `[Point2f]`.

    Array of (x,y) coordinates of each keypoint

  ##### Keyword Arguments
  - **size**: `float`.

    keypoint diameter

  - **response**: `float`.

    keypoint detector response on the keypoint (that is, strength of the keypoint)

  - **octave**: `integer()`.

    pyramid octave in which the keypoint has been detected

  - **class_id**: `integer()`.

    object id

  ##### Return
  - **keypoints**: `[Evision.KeyPoint]`.

    Keypoints obtained from any feature detection algorithm like SIFT/SURF/ORB

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  convert(points2f[, size[, response[, octave[, class_id]]]]) -> keypoints
  ```
  """
  @spec convert(list({number(), number()}), [{:class_id, term()} | {:octave, term()} | {:response, term()} | {:size, term()}] | nil) :: list(Evision.KeyPoint.t()) | {:error, String.t()}
  def convert(points2f, opts) when is_list(points2f) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:class_id, :octave, :response, :size])
    positional = [
      points2f: Evision.Internal.Structurise.from_struct(points2f)
    ]
    :evision_nif.keyPoint_convert_static(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  convert

  ##### Positional Arguments
  - **points2f**: `[Point2f]`.

    Array of (x,y) coordinates of each keypoint

  ##### Keyword Arguments
  - **size**: `float`.

    keypoint diameter

  - **response**: `float`.

    keypoint detector response on the keypoint (that is, strength of the keypoint)

  - **octave**: `integer()`.

    pyramid octave in which the keypoint has been detected

  - **class_id**: `integer()`.

    object id

  ##### Return
  - **keypoints**: `[Evision.KeyPoint]`.

    Keypoints obtained from any feature detection algorithm like SIFT/SURF/ORB

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  convert(points2f[, size[, response[, octave[, class_id]]]]) -> keypoints
  ```
  """
  @spec convert(list({number(), number()})) :: list(Evision.KeyPoint.t()) | {:error, String.t()}
  def convert(points2f) when is_list(points2f)
  do
    positional = [
      points2f: Evision.Internal.Structurise.from_struct(points2f)
    ]
    :evision_nif.keyPoint_convert_static(positional)
    |> to_struct()
  end

  @doc """
  overlap

  ##### Positional Arguments
  - **kp1**: `Evision.KeyPoint`.

    First keypoint

  - **kp2**: `Evision.KeyPoint`.

    Second keypoint

  ##### Return
  - **retval**: `float`

  This method computes overlap for pair of keypoints. Overlap is the ratio between area of keypoint
  regions' intersection and area of keypoint regions' union (considering keypoint region as circle).
  If they don't overlap, we get zero. If they coincide at same location with same size, we get 1.

  Python prototype (for reference only):
  ```python3
  overlap(kp1, kp2) -> retval
  ```
  """
  @spec overlap(Evision.KeyPoint.t(), Evision.KeyPoint.t()) :: number() | {:error, String.t()}
  def overlap(kp1, kp2) when is_struct(kp1, Evision.KeyPoint) and is_struct(kp2, Evision.KeyPoint)
  do
    positional = [
      kp1: Evision.Internal.Structurise.from_struct(kp1),
      kp2: Evision.Internal.Structurise.from_struct(kp2)
    ]
    :evision_nif.keyPoint_overlap_static(positional)
    |> to_struct()
  end
  @spec get_angle(Evision.KeyPoint.t()) :: number()
  def get_angle(self) do
    :evision_nif.keyPoint_get_angle(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_angle(Evision.KeyPoint.t(), number()) :: Evision.KeyPoint.t()
  def set_angle(self, prop) do
    :evision_nif.keyPoint_set_angle(
        Evision.Internal.Structurise.from_struct(self),
        [angle: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_class_id(Evision.KeyPoint.t()) :: integer()
  def get_class_id(self) do
    :evision_nif.keyPoint_get_class_id(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_class_id(Evision.KeyPoint.t(), integer()) :: Evision.KeyPoint.t()
  def set_class_id(self, prop) do
    :evision_nif.keyPoint_set_class_id(
        Evision.Internal.Structurise.from_struct(self),
        [class_id: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_octave(Evision.KeyPoint.t()) :: integer()
  def get_octave(self) do
    :evision_nif.keyPoint_get_octave(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_octave(Evision.KeyPoint.t(), integer()) :: Evision.KeyPoint.t()
  def set_octave(self, prop) do
    :evision_nif.keyPoint_set_octave(
        Evision.Internal.Structurise.from_struct(self),
        [octave: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_pt(Evision.KeyPoint.t()) :: {number(), number()}
  def get_pt(self) do
    :evision_nif.keyPoint_get_pt(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_pt(Evision.KeyPoint.t(), {number(), number()}) :: Evision.KeyPoint.t()
  def set_pt(self, prop) do
    :evision_nif.keyPoint_set_pt(
        Evision.Internal.Structurise.from_struct(self),
        [pt: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_response(Evision.KeyPoint.t()) :: number()
  def get_response(self) do
    :evision_nif.keyPoint_get_response(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_response(Evision.KeyPoint.t(), number()) :: Evision.KeyPoint.t()
  def set_response(self, prop) do
    :evision_nif.keyPoint_set_response(
        Evision.Internal.Structurise.from_struct(self),
        [response: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_size(Evision.KeyPoint.t()) :: number()
  def get_size(self) do
    :evision_nif.keyPoint_get_size(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_size(Evision.KeyPoint.t(), number()) :: Evision.KeyPoint.t()
  def set_size(self, prop) do
    :evision_nif.keyPoint_set_size(
        Evision.Internal.Structurise.from_struct(self),
        [size: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
end
