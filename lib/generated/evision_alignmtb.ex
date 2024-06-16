defmodule Evision.AlignMTB do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `AlignMTB` struct.

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
  def to_struct({:ok, %{class: Evision.AlignMTB, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.AlignMTB, ref: ref}) do
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
  Calculates shift between two images, i. e. how to shift the second image to correspond it with the
  first.

  ##### Positional Arguments
  - **self**: `Evision.AlignMTB.t()`
  - **img0**: `Evision.Mat`.

    first image

  - **img1**: `Evision.Mat`.

    second image

  ##### Return
  - **retval**: `Point`

  Python prototype (for reference only):
  ```python3
  calculateShift(img0, img1) -> retval
  ```
  """
  @spec calculateShift(Evision.AlignMTB.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: {number(), number()} | {:error, String.t()}
  def calculateShift(self, img0, img1) when (is_struct(img0, Evision.Mat) or is_struct(img0, Nx.Tensor) or is_number(img0) or is_tuple(img0)) and (is_struct(img1, Evision.Mat) or is_struct(img1, Nx.Tensor) or is_number(img1) or is_tuple(img1))
  do
    positional = [
      img0: Evision.Internal.Structurise.from_struct(img0),
      img1: Evision.Internal.Structurise.from_struct(img1)
    ]
    :evision_nif.alignMTB_calculateShift(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Computes median threshold and exclude bitmaps of given image.

  ##### Positional Arguments
  - **self**: `Evision.AlignMTB.t()`
  - **img**: `Evision.Mat`.

    input image

  ##### Return
  - **tb**: `Evision.Mat.t()`.

    median threshold bitmap

  - **eb**: `Evision.Mat.t()`.

    exclude bitmap

  Python prototype (for reference only):
  ```python3
  computeBitmaps(img[, tb[, eb]]) -> tb, eb
  ```
  """
  @spec computeBitmaps(Evision.AlignMTB.t(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: {Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def computeBitmaps(self, img, opts) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img)
    ]
    :evision_nif.alignMTB_computeBitmaps(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Computes median threshold and exclude bitmaps of given image.

  ##### Positional Arguments
  - **self**: `Evision.AlignMTB.t()`
  - **img**: `Evision.Mat`.

    input image

  ##### Return
  - **tb**: `Evision.Mat.t()`.

    median threshold bitmap

  - **eb**: `Evision.Mat.t()`.

    exclude bitmap

  Python prototype (for reference only):
  ```python3
  computeBitmaps(img[, tb[, eb]]) -> tb, eb
  ```
  """
  @spec computeBitmaps(Evision.AlignMTB.t(), Evision.Mat.maybe_mat_in()) :: {Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def computeBitmaps(self, img) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img)
    ]
    :evision_nif.alignMTB_computeBitmaps(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getCut

  ##### Positional Arguments
  - **self**: `Evision.AlignMTB.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  getCut() -> retval
  ```
  """
  @spec getCut(Evision.AlignMTB.t()) :: boolean() | {:error, String.t()}
  def getCut(self) do
    positional = [
    ]
    :evision_nif.alignMTB_getCut(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getExcludeRange

  ##### Positional Arguments
  - **self**: `Evision.AlignMTB.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getExcludeRange() -> retval
  ```
  """
  @spec getExcludeRange(Evision.AlignMTB.t()) :: integer() | {:error, String.t()}
  def getExcludeRange(self) do
    positional = [
    ]
    :evision_nif.alignMTB_getExcludeRange(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMaxBits

  ##### Positional Arguments
  - **self**: `Evision.AlignMTB.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getMaxBits() -> retval
  ```
  """
  @spec getMaxBits(Evision.AlignMTB.t()) :: integer() | {:error, String.t()}
  def getMaxBits(self) do
    positional = [
    ]
    :evision_nif.alignMTB_getMaxBits(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  process

  ##### Positional Arguments
  - **self**: `Evision.AlignMTB.t()`
  - **src**: `[Evision.Mat]`
  - **dst**: `[Evision.Mat]`
  - **times**: `Evision.Mat`
  - **response**: `Evision.Mat`

  Python prototype (for reference only):
  ```python3
  process(src, dst, times, response) -> None
  ```
  """
  @spec process(Evision.AlignMTB.t(), list(Evision.Mat.maybe_mat_in()), list(Evision.Mat.maybe_mat_in()), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.AlignMTB.t() | {:error, String.t()}
  def process(self, src, dst, times, response) when is_list(src) and is_list(dst) and (is_struct(times, Evision.Mat) or is_struct(times, Nx.Tensor) or is_number(times) or is_tuple(times)) and (is_struct(response, Evision.Mat) or is_struct(response, Nx.Tensor) or is_number(response) or is_tuple(response))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      dst: Evision.Internal.Structurise.from_struct(dst),
      times: Evision.Internal.Structurise.from_struct(times),
      response: Evision.Internal.Structurise.from_struct(response)
    ]
    :evision_nif.alignMTB_process(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Short version of process, that doesn't take extra arguments.

  ##### Positional Arguments
  - **self**: `Evision.AlignMTB.t()`
  - **src**: `[Evision.Mat]`.

    vector of input images

  - **dst**: `[Evision.Mat]`.

    vector of aligned images

  Python prototype (for reference only):
  ```python3
  process(src, dst) -> None
  ```
  """
  @spec process(Evision.AlignMTB.t(), list(Evision.Mat.maybe_mat_in()), list(Evision.Mat.maybe_mat_in())) :: Evision.AlignMTB.t() | {:error, String.t()}
  def process(self, src, dst) when is_list(src) and is_list(dst)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      dst: Evision.Internal.Structurise.from_struct(dst)
    ]
    :evision_nif.alignMTB_process(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setCut

  ##### Positional Arguments
  - **self**: `Evision.AlignMTB.t()`
  - **value**: `bool`

  Python prototype (for reference only):
  ```python3
  setCut(value) -> None
  ```
  """
  @spec setCut(Evision.AlignMTB.t(), boolean()) :: Evision.AlignMTB.t() | {:error, String.t()}
  def setCut(self, value) when is_boolean(value)
  do
    positional = [
      value: Evision.Internal.Structurise.from_struct(value)
    ]
    :evision_nif.alignMTB_setCut(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setExcludeRange

  ##### Positional Arguments
  - **self**: `Evision.AlignMTB.t()`
  - **exclude_range**: `integer()`

  Python prototype (for reference only):
  ```python3
  setExcludeRange(exclude_range) -> None
  ```
  """
  @spec setExcludeRange(Evision.AlignMTB.t(), integer()) :: Evision.AlignMTB.t() | {:error, String.t()}
  def setExcludeRange(self, exclude_range) when is_integer(exclude_range)
  do
    positional = [
      exclude_range: Evision.Internal.Structurise.from_struct(exclude_range)
    ]
    :evision_nif.alignMTB_setExcludeRange(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMaxBits

  ##### Positional Arguments
  - **self**: `Evision.AlignMTB.t()`
  - **max_bits**: `integer()`

  Python prototype (for reference only):
  ```python3
  setMaxBits(max_bits) -> None
  ```
  """
  @spec setMaxBits(Evision.AlignMTB.t(), integer()) :: Evision.AlignMTB.t() | {:error, String.t()}
  def setMaxBits(self, max_bits) when is_integer(max_bits)
  do
    positional = [
      max_bits: Evision.Internal.Structurise.from_struct(max_bits)
    ]
    :evision_nif.alignMTB_setMaxBits(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Helper function, that shift Mat filling new regions with zeros.

  ##### Positional Arguments
  - **self**: `Evision.AlignMTB.t()`
  - **src**: `Evision.Mat`.

    input image

  - **shift**: `Point`.

    shift value

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    result image

  Python prototype (for reference only):
  ```python3
  shiftMat(src, shift[, dst]) -> dst
  ```
  """
  @spec shiftMat(Evision.AlignMTB.t(), Evision.Mat.maybe_mat_in(), {number(), number()}, [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def shiftMat(self, src, shift, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_tuple(shift) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      shift: Evision.Internal.Structurise.from_struct(shift)
    ]
    :evision_nif.alignMTB_shiftMat(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Helper function, that shift Mat filling new regions with zeros.

  ##### Positional Arguments
  - **self**: `Evision.AlignMTB.t()`
  - **src**: `Evision.Mat`.

    input image

  - **shift**: `Point`.

    shift value

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    result image

  Python prototype (for reference only):
  ```python3
  shiftMat(src, shift[, dst]) -> dst
  ```
  """
  @spec shiftMat(Evision.AlignMTB.t(), Evision.Mat.maybe_mat_in(), {number(), number()}) :: Evision.Mat.t() | {:error, String.t()}
  def shiftMat(self, src, shift) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_tuple(shift)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      shift: Evision.Internal.Structurise.from_struct(shift)
    ]
    :evision_nif.alignMTB_shiftMat(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
