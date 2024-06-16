defmodule Evision.PyRotationWarper do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `PyRotationWarper` struct.

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
  def to_struct({:ok, %{class: Evision.PyRotationWarper, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.PyRotationWarper, ref: ref}) do
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
  PyRotationWarper

  ##### Positional Arguments
  - **type**: `String`
  - **scale**: `float`

  ##### Return
  - **self**: `Evision.PyRotationWarper.t()`

  Python prototype (for reference only):
  ```python3
  PyRotationWarper(type, scale) -> <PyRotationWarper object>
  ```
  """
  @spec pyRotationWarper(binary(), number()) :: Evision.PyRotationWarper.t() | {:error, String.t()}
  def pyRotationWarper(type, scale) when is_binary(type) and is_float(scale)
  do
    positional = [
      type: Evision.Internal.Structurise.from_struct(type),
      scale: Evision.Internal.Structurise.from_struct(scale)
    ]
    :evision_nif.pyRotationWarper_PyRotationWarper(positional)
    |> to_struct()
  end

  @doc """
  PyRotationWarper
  ##### Return
  - **self**: `Evision.PyRotationWarper.t()`

  Python prototype (for reference only):
  ```python3
  PyRotationWarper() -> <PyRotationWarper object>
  ```
  """
  @spec pyRotationWarper() :: Evision.PyRotationWarper.t() | {:error, String.t()}
  def pyRotationWarper() do
    positional = [
    ]
    :evision_nif.pyRotationWarper_PyRotationWarper(positional)
    |> to_struct()
  end

  @doc """
  Builds the projection maps according to the given camera data.

  ##### Positional Arguments
  - **self**: `Evision.PyRotationWarper.t()`
  - **src_size**: `Size`.

    Source image size

  - **k**: `Evision.Mat`.

    Camera intrinsic parameters

  - **r**: `Evision.Mat`.

    Camera rotation matrix

  ##### Return
  - **retval**: `Rect`
  - **xmap**: `Evision.Mat.t()`.

    Projection map for the x axis

  - **ymap**: `Evision.Mat.t()`.

    Projection map for the y axis

  @return Projected image minimum bounding box

  Python prototype (for reference only):
  ```python3
  buildMaps(src_size, K, R[, xmap[, ymap]]) -> retval, xmap, ymap
  ```
  """
  @spec buildMaps(Evision.PyRotationWarper.t(), {number(), number()}, Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: {{number(), number(), number(), number()}, Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def buildMaps(self, src_size, k, r, opts) when is_tuple(src_size) and (is_struct(k, Evision.Mat) or is_struct(k, Nx.Tensor) or is_number(k) or is_tuple(k)) and (is_struct(r, Evision.Mat) or is_struct(r, Nx.Tensor) or is_number(r) or is_tuple(r)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      src_size: Evision.Internal.Structurise.from_struct(src_size),
      k: Evision.Internal.Structurise.from_struct(k),
      r: Evision.Internal.Structurise.from_struct(r)
    ]
    :evision_nif.pyRotationWarper_buildMaps(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Builds the projection maps according to the given camera data.

  ##### Positional Arguments
  - **self**: `Evision.PyRotationWarper.t()`
  - **src_size**: `Size`.

    Source image size

  - **k**: `Evision.Mat`.

    Camera intrinsic parameters

  - **r**: `Evision.Mat`.

    Camera rotation matrix

  ##### Return
  - **retval**: `Rect`
  - **xmap**: `Evision.Mat.t()`.

    Projection map for the x axis

  - **ymap**: `Evision.Mat.t()`.

    Projection map for the y axis

  @return Projected image minimum bounding box

  Python prototype (for reference only):
  ```python3
  buildMaps(src_size, K, R[, xmap[, ymap]]) -> retval, xmap, ymap
  ```
  """
  @spec buildMaps(Evision.PyRotationWarper.t(), {number(), number()}, Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: {{number(), number(), number(), number()}, Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def buildMaps(self, src_size, k, r) when is_tuple(src_size) and (is_struct(k, Evision.Mat) or is_struct(k, Nx.Tensor) or is_number(k) or is_tuple(k)) and (is_struct(r, Evision.Mat) or is_struct(r, Nx.Tensor) or is_number(r) or is_tuple(r))
  do
    positional = [
      src_size: Evision.Internal.Structurise.from_struct(src_size),
      k: Evision.Internal.Structurise.from_struct(k),
      r: Evision.Internal.Structurise.from_struct(r)
    ]
    :evision_nif.pyRotationWarper_buildMaps(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getScale

  ##### Positional Arguments
  - **self**: `Evision.PyRotationWarper.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  getScale() -> retval
  ```
  """
  @spec getScale(Evision.PyRotationWarper.t()) :: number() | {:error, String.t()}
  def getScale(self) do
    positional = [
    ]
    :evision_nif.pyRotationWarper_getScale(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setScale

  ##### Positional Arguments
  - **self**: `Evision.PyRotationWarper.t()`
  - **arg1**: `float`

  Python prototype (for reference only):
  ```python3
  setScale(arg1) -> None
  ```
  """
  @spec setScale(Evision.PyRotationWarper.t(), number()) :: Evision.PyRotationWarper.t() | {:error, String.t()}
  def setScale(self, arg1) when is_float(arg1)
  do
    positional = [
      arg1: Evision.Internal.Structurise.from_struct(arg1)
    ]
    :evision_nif.pyRotationWarper_setScale(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Projects the image.

  ##### Positional Arguments
  - **self**: `Evision.PyRotationWarper.t()`
  - **src**: `Evision.Mat`.

    Source image

  - **k**: `Evision.Mat`.

    Camera intrinsic parameters

  - **r**: `Evision.Mat`.

    Camera rotation matrix

  - **interp_mode**: `integer()`.

    Interpolation mode

  - **border_mode**: `integer()`.

    Border extrapolation mode

  ##### Return
  - **retval**: `Point`
  - **dst**: `Evision.Mat.t()`.

    Projected image

  @return Project image top-left corner

  Python prototype (for reference only):
  ```python3
  warp(src, K, R, interp_mode, border_mode[, dst]) -> retval, dst
  ```
  """
  @spec warp(Evision.PyRotationWarper.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), integer(), integer(), [{atom(), term()},...] | nil) :: {{number(), number()}, Evision.Mat.t()} | {:error, String.t()}
  def warp(self, src, k, r, interp_mode, border_mode, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (is_struct(k, Evision.Mat) or is_struct(k, Nx.Tensor) or is_number(k) or is_tuple(k)) and (is_struct(r, Evision.Mat) or is_struct(r, Nx.Tensor) or is_number(r) or is_tuple(r)) and is_integer(interp_mode) and is_integer(border_mode) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      k: Evision.Internal.Structurise.from_struct(k),
      r: Evision.Internal.Structurise.from_struct(r),
      interp_mode: Evision.Internal.Structurise.from_struct(interp_mode),
      border_mode: Evision.Internal.Structurise.from_struct(border_mode)
    ]
    :evision_nif.pyRotationWarper_warp(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Projects the image.

  ##### Positional Arguments
  - **self**: `Evision.PyRotationWarper.t()`
  - **src**: `Evision.Mat`.

    Source image

  - **k**: `Evision.Mat`.

    Camera intrinsic parameters

  - **r**: `Evision.Mat`.

    Camera rotation matrix

  - **interp_mode**: `integer()`.

    Interpolation mode

  - **border_mode**: `integer()`.

    Border extrapolation mode

  ##### Return
  - **retval**: `Point`
  - **dst**: `Evision.Mat.t()`.

    Projected image

  @return Project image top-left corner

  Python prototype (for reference only):
  ```python3
  warp(src, K, R, interp_mode, border_mode[, dst]) -> retval, dst
  ```
  """
  @spec warp(Evision.PyRotationWarper.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), integer(), integer()) :: {{number(), number()}, Evision.Mat.t()} | {:error, String.t()}
  def warp(self, src, k, r, interp_mode, border_mode) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (is_struct(k, Evision.Mat) or is_struct(k, Nx.Tensor) or is_number(k) or is_tuple(k)) and (is_struct(r, Evision.Mat) or is_struct(r, Nx.Tensor) or is_number(r) or is_tuple(r)) and is_integer(interp_mode) and is_integer(border_mode)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      k: Evision.Internal.Structurise.from_struct(k),
      r: Evision.Internal.Structurise.from_struct(r),
      interp_mode: Evision.Internal.Structurise.from_struct(interp_mode),
      border_mode: Evision.Internal.Structurise.from_struct(border_mode)
    ]
    :evision_nif.pyRotationWarper_warp(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Projects the image backward.

  ##### Positional Arguments
  - **self**: `Evision.PyRotationWarper.t()`
  - **src**: `Evision.Mat`.

    Projected image

  - **k**: `Evision.Mat`.

    Camera intrinsic parameters

  - **r**: `Evision.Mat`.

    Camera rotation matrix

  - **interp_mode**: `integer()`.

    Interpolation mode

  - **border_mode**: `integer()`.

    Border extrapolation mode

  - **dst_size**: `Size`.

    Backward-projected image size

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Backward-projected image

  Python prototype (for reference only):
  ```python3
  warpBackward(src, K, R, interp_mode, border_mode, dst_size[, dst]) -> dst
  ```
  """
  @spec warpBackward(Evision.PyRotationWarper.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), integer(), integer(), {number(), number()}, [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def warpBackward(self, src, k, r, interp_mode, border_mode, dst_size, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (is_struct(k, Evision.Mat) or is_struct(k, Nx.Tensor) or is_number(k) or is_tuple(k)) and (is_struct(r, Evision.Mat) or is_struct(r, Nx.Tensor) or is_number(r) or is_tuple(r)) and is_integer(interp_mode) and is_integer(border_mode) and is_tuple(dst_size) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      k: Evision.Internal.Structurise.from_struct(k),
      r: Evision.Internal.Structurise.from_struct(r),
      interp_mode: Evision.Internal.Structurise.from_struct(interp_mode),
      border_mode: Evision.Internal.Structurise.from_struct(border_mode),
      dst_size: Evision.Internal.Structurise.from_struct(dst_size)
    ]
    :evision_nif.pyRotationWarper_warpBackward(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Projects the image backward.

  ##### Positional Arguments
  - **self**: `Evision.PyRotationWarper.t()`
  - **src**: `Evision.Mat`.

    Projected image

  - **k**: `Evision.Mat`.

    Camera intrinsic parameters

  - **r**: `Evision.Mat`.

    Camera rotation matrix

  - **interp_mode**: `integer()`.

    Interpolation mode

  - **border_mode**: `integer()`.

    Border extrapolation mode

  - **dst_size**: `Size`.

    Backward-projected image size

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Backward-projected image

  Python prototype (for reference only):
  ```python3
  warpBackward(src, K, R, interp_mode, border_mode, dst_size[, dst]) -> dst
  ```
  """
  @spec warpBackward(Evision.PyRotationWarper.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), integer(), integer(), {number(), number()}) :: Evision.Mat.t() | {:error, String.t()}
  def warpBackward(self, src, k, r, interp_mode, border_mode, dst_size) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (is_struct(k, Evision.Mat) or is_struct(k, Nx.Tensor) or is_number(k) or is_tuple(k)) and (is_struct(r, Evision.Mat) or is_struct(r, Nx.Tensor) or is_number(r) or is_tuple(r)) and is_integer(interp_mode) and is_integer(border_mode) and is_tuple(dst_size)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      k: Evision.Internal.Structurise.from_struct(k),
      r: Evision.Internal.Structurise.from_struct(r),
      interp_mode: Evision.Internal.Structurise.from_struct(interp_mode),
      border_mode: Evision.Internal.Structurise.from_struct(border_mode),
      dst_size: Evision.Internal.Structurise.from_struct(dst_size)
    ]
    :evision_nif.pyRotationWarper_warpBackward(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Projects the image point.

  ##### Positional Arguments
  - **self**: `Evision.PyRotationWarper.t()`
  - **pt**: `Point2f`.

    Source point

  - **k**: `Evision.Mat`.

    Camera intrinsic parameters

  - **r**: `Evision.Mat`.

    Camera rotation matrix

  ##### Return
  - **retval**: `Point2f`

  @return Projected point

  Python prototype (for reference only):
  ```python3
  warpPoint(pt, K, R) -> retval
  ```
  """
  @spec warpPoint(Evision.PyRotationWarper.t(), {number(), number()}, Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: {number(), number()} | {:error, String.t()}
  def warpPoint(self, pt, k, r) when is_tuple(pt) and (is_struct(k, Evision.Mat) or is_struct(k, Nx.Tensor) or is_number(k) or is_tuple(k)) and (is_struct(r, Evision.Mat) or is_struct(r, Nx.Tensor) or is_number(r) or is_tuple(r))
  do
    positional = [
      pt: Evision.Internal.Structurise.from_struct(pt),
      k: Evision.Internal.Structurise.from_struct(k),
      r: Evision.Internal.Structurise.from_struct(r)
    ]
    :evision_nif.pyRotationWarper_warpPoint(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  warpPointBackward

  ##### Positional Arguments
  - **self**: `Evision.PyRotationWarper.t()`
  - **pt**: `Point2f`
  - **k**: `Evision.Mat`
  - **r**: `Evision.Mat`

  ##### Return
  - **retval**: `Point2f`

  Python prototype (for reference only):
  ```python3
  warpPointBackward(pt, K, R) -> retval
  ```
  """
  @spec warpPointBackward(Evision.PyRotationWarper.t(), {number(), number()}, Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: {number(), number()} | {:error, String.t()}
  def warpPointBackward(self, pt, k, r) when is_tuple(pt) and (is_struct(k, Evision.Mat) or is_struct(k, Nx.Tensor) or is_number(k) or is_tuple(k)) and (is_struct(r, Evision.Mat) or is_struct(r, Nx.Tensor) or is_number(r) or is_tuple(r))
  do
    positional = [
      pt: Evision.Internal.Structurise.from_struct(pt),
      k: Evision.Internal.Structurise.from_struct(k),
      r: Evision.Internal.Structurise.from_struct(r)
    ]
    :evision_nif.pyRotationWarper_warpPointBackward(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  warpRoi

  ##### Positional Arguments
  - **self**: `Evision.PyRotationWarper.t()`
  - **src_size**: `Size`.

    Source image bounding box

  - **k**: `Evision.Mat`.

    Camera intrinsic parameters

  - **r**: `Evision.Mat`.

    Camera rotation matrix

  ##### Return
  - **retval**: `Rect`

  @return Projected image minimum bounding box

  Python prototype (for reference only):
  ```python3
  warpRoi(src_size, K, R) -> retval
  ```
  """
  @spec warpRoi(Evision.PyRotationWarper.t(), {number(), number()}, Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: {number(), number(), number(), number()} | {:error, String.t()}
  def warpRoi(self, src_size, k, r) when is_tuple(src_size) and (is_struct(k, Evision.Mat) or is_struct(k, Nx.Tensor) or is_number(k) or is_tuple(k)) and (is_struct(r, Evision.Mat) or is_struct(r, Nx.Tensor) or is_number(r) or is_tuple(r))
  do
    positional = [
      src_size: Evision.Internal.Structurise.from_struct(src_size),
      k: Evision.Internal.Structurise.from_struct(k),
      r: Evision.Internal.Structurise.from_struct(r)
    ]
    :evision_nif.pyRotationWarper_warpRoi(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
