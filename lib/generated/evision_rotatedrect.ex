defmodule Evision.RotatedRect do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `RotatedRect` struct.

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
  def to_struct({:ok, %{class: Evision.RotatedRect, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.RotatedRect, ref: ref}) do
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
  #### Variant 1:
  RotatedRect

  ##### Positional Arguments
  - **point1**: `Point2f`
  - **point2**: `Point2f`
  - **point3**: `Point2f`

  ##### Return
  - **self**: `{centre={x, y}, size={s1, s2}, angle}`

  Any 3 end points of the RotatedRect. They must be given in order (either clockwise or
  anticlockwise).

  Python prototype (for reference only):
  ```python3
  RotatedRect(point1, point2, point3) -> <RotatedRect object>
  ```
  #### Variant 2:
  RotatedRect

  ##### Positional Arguments
  - **center**: `Point2f`.

    The rectangle mass center.

  - **size**: `Size2f`.

    Width and height of the rectangle.

  - **angle**: `float`.

    The rotation angle in a clockwise direction. When the angle is 0, 90, 180, 270 etc.,
    the rectangle becomes an up-right rectangle.

  ##### Return
  - **self**: `{centre={x, y}, size={s1, s2}, angle}`

  full constructor

  Python prototype (for reference only):
  ```python3
  RotatedRect(center, size, angle) -> <RotatedRect object>
  ```

  """
  @spec rotatedRect({number(), number()}, {number(), number()}, {number(), number()}) :: {{number(), number()}, {number(), number()}, number()} | {:error, String.t()}
  def rotatedRect(point1, point2, point3) when is_tuple(point1) and is_tuple(point2) and is_tuple(point3)
  do
    positional = [
      point1: Evision.Internal.Structurise.from_struct(point1),
      point2: Evision.Internal.Structurise.from_struct(point2),
      point3: Evision.Internal.Structurise.from_struct(point3)
    ]
    :evision_nif.rotatedRect_RotatedRect(positional)
    |> to_struct()
  end
  @spec rotatedRect({number(), number()}, {number(), number()}, number()) :: {{number(), number()}, {number(), number()}, number()} | {:error, String.t()}
  def rotatedRect(center, size, angle) when is_tuple(center) and is_float(angle)
  do
    positional = [
      center: Evision.Internal.Structurise.from_struct(center),
      size: Evision.Internal.Structurise.from_struct(size),
      angle: Evision.Internal.Structurise.from_struct(angle)
    ]
    :evision_nif.rotatedRect_RotatedRect(positional)
    |> to_struct()
  end

  @doc """
  RotatedRect
  ##### Return
  - **self**: `{centre={x, y}, size={s1, s2}, angle}`

  Python prototype (for reference only):
  ```python3
  RotatedRect() -> <RotatedRect object>
  ```
  """
  @spec rotatedRect() :: {{number(), number()}, {number(), number()}, number()} | {:error, String.t()}
  def rotatedRect() do
    positional = [
    ]
    :evision_nif.rotatedRect_RotatedRect(positional)
    |> to_struct()
  end

  @doc """
  boundingRect

  ##### Positional Arguments
  - **self**: `Evision.RotatedRect.t()`

  ##### Return
  - **retval**: `Rect`

  Python prototype (for reference only):
  ```python3
  boundingRect() -> retval
  ```
  """
  @spec boundingRect(Evision.RotatedRect.t()) :: {number(), number(), number(), number()} | {:error, String.t()}
  def boundingRect(self) do
    positional = [
    ]
    :evision_nif.rotatedRect_boundingRect(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  boundingRect2f

  ##### Positional Arguments
  - **self**: `Evision.RotatedRect.t()`

  ##### Return
  - **retval**: `Rect2f`

  Python prototype (for reference only):
  ```python3
  boundingRect2f() -> retval
  ```
  """
  @spec boundingRect2f(Evision.RotatedRect.t()) :: {number(), number(), number(), number()} | {:error, String.t()}
  def boundingRect2f(self) do
    positional = [
    ]
    :evision_nif.rotatedRect_boundingRect2f(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  points

  ##### Positional Arguments
  - **self**: `Evision.RotatedRect.t()`

  ##### Return
  - **pts**: `[Point2f]`.

    The points array for storing rectangle vertices. The order is _bottomLeft_, _topLeft_, topRight, bottomRight.

  returns 4 vertices of the rotated rectangle
  **Note**: _Bottom_, _Top_, _Left_ and _Right_ sides refer to the original rectangle (angle is 0),
  so after 180 degree rotation _bottomLeft_ point will be located at the top right corner of the
  rectangle.

  Python prototype (for reference only):
  ```python3
  points() -> pts
  ```
  """
  @spec points(Evision.RotatedRect.t()) :: list({number(), number()}) | {:error, String.t()}
  def points(self) do
    positional = [
    ]
    :evision_nif.rotatedRect_points(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec get_angle({{number(), number()}, {number(), number()}, number()}) :: number()
  def get_angle(self) do
    :evision_nif.rotatedRect_get_angle(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_angle({{number(), number()}, {number(), number()}, number()}, number()) :: {{number(), number()}, {number(), number()}, number()}
  def set_angle(self, prop) do
    :evision_nif.rotatedRect_set_angle(
        Evision.Internal.Structurise.from_struct(self),
        [angle: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_center({{number(), number()}, {number(), number()}, number()}) :: {number(), number()}
  def get_center(self) do
    :evision_nif.rotatedRect_get_center(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_center({{number(), number()}, {number(), number()}, number()}, {number(), number()}) :: {{number(), number()}, {number(), number()}, number()}
  def set_center(self, prop) do
    :evision_nif.rotatedRect_set_center(
        Evision.Internal.Structurise.from_struct(self),
        [center: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_size({{number(), number()}, {number(), number()}, number()}) :: {number(), number()}
  def get_size(self) do
    :evision_nif.rotatedRect_get_size(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_size({{number(), number()}, {number(), number()}, number()}, {number(), number()}) :: {{number(), number()}, {number(), number()}, number()}
  def set_size(self, prop) do
    :evision_nif.rotatedRect_set_size(
        Evision.Internal.Structurise.from_struct(self),
        [size: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
end
