defmodule Evision.XImgProc.SparseMatchInterpolator do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `XImgProc.SparseMatchInterpolator` struct.

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
  def to_struct({:ok, %{class: Evision.XImgProc.SparseMatchInterpolator, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.XImgProc.SparseMatchInterpolator, ref: ref}) do
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
  Clears the algorithm state

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.SparseMatchInterpolator.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.XImgProc.SparseMatchInterpolator.t()) :: Evision.XImgProc.SparseMatchInterpolator.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.ximgproc_SparseMatchInterpolator_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.SparseMatchInterpolator.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.XImgProc.SparseMatchInterpolator.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.ximgproc_SparseMatchInterpolator_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.SparseMatchInterpolator.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.XImgProc.SparseMatchInterpolator.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.ximgproc_SparseMatchInterpolator_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Interpolate input sparse matches.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.SparseMatchInterpolator.t()`
  - **from_image**: `Evision.Mat`.

    first of the two matched images, 8-bit single-channel or three-channel.

  - **from_points**: `Evision.Mat`.

    points of the from_image for which there are correspondences in the
    to_image (Point2f vector or Mat of depth CV_32F)

  - **to_image**: `Evision.Mat`.

    second of the two matched images, 8-bit single-channel or three-channel.

  - **to_points**: `Evision.Mat`.

    points in the to_image corresponding to from_points
    (Point2f vector or Mat of depth CV_32F)

  ##### Return
  - **dense_flow**: `Evision.Mat.t()`.

    output dense matching (two-channel CV_32F image)

  Python prototype (for reference only):
  ```python3
  interpolate(from_image, from_points, to_image, to_points[, dense_flow]) -> dense_flow
  ```
  """
  @spec interpolate(Evision.XImgProc.SparseMatchInterpolator.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def interpolate(self, from_image, from_points, to_image, to_points, opts) when (is_struct(from_image, Evision.Mat) or is_struct(from_image, Nx.Tensor) or is_number(from_image) or is_tuple(from_image)) and (is_struct(from_points, Evision.Mat) or is_struct(from_points, Nx.Tensor) or is_number(from_points) or is_tuple(from_points)) and (is_struct(to_image, Evision.Mat) or is_struct(to_image, Nx.Tensor) or is_number(to_image) or is_tuple(to_image)) and (is_struct(to_points, Evision.Mat) or is_struct(to_points, Nx.Tensor) or is_number(to_points) or is_tuple(to_points)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      from_image: Evision.Internal.Structurise.from_struct(from_image),
      from_points: Evision.Internal.Structurise.from_struct(from_points),
      to_image: Evision.Internal.Structurise.from_struct(to_image),
      to_points: Evision.Internal.Structurise.from_struct(to_points)
    ]
    :evision_nif.ximgproc_ximgproc_SparseMatchInterpolator_interpolate(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Interpolate input sparse matches.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.SparseMatchInterpolator.t()`
  - **from_image**: `Evision.Mat`.

    first of the two matched images, 8-bit single-channel or three-channel.

  - **from_points**: `Evision.Mat`.

    points of the from_image for which there are correspondences in the
    to_image (Point2f vector or Mat of depth CV_32F)

  - **to_image**: `Evision.Mat`.

    second of the two matched images, 8-bit single-channel or three-channel.

  - **to_points**: `Evision.Mat`.

    points in the to_image corresponding to from_points
    (Point2f vector or Mat of depth CV_32F)

  ##### Return
  - **dense_flow**: `Evision.Mat.t()`.

    output dense matching (two-channel CV_32F image)

  Python prototype (for reference only):
  ```python3
  interpolate(from_image, from_points, to_image, to_points[, dense_flow]) -> dense_flow
  ```
  """
  @spec interpolate(Evision.XImgProc.SparseMatchInterpolator.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def interpolate(self, from_image, from_points, to_image, to_points) when (is_struct(from_image, Evision.Mat) or is_struct(from_image, Nx.Tensor) or is_number(from_image) or is_tuple(from_image)) and (is_struct(from_points, Evision.Mat) or is_struct(from_points, Nx.Tensor) or is_number(from_points) or is_tuple(from_points)) and (is_struct(to_image, Evision.Mat) or is_struct(to_image, Nx.Tensor) or is_number(to_image) or is_tuple(to_image)) and (is_struct(to_points, Evision.Mat) or is_struct(to_points, Nx.Tensor) or is_number(to_points) or is_tuple(to_points))
  do
    positional = [
      from_image: Evision.Internal.Structurise.from_struct(from_image),
      from_points: Evision.Internal.Structurise.from_struct(from_points),
      to_image: Evision.Internal.Structurise.from_struct(to_image),
      to_points: Evision.Internal.Structurise.from_struct(to_points)
    ]
    :evision_nif.ximgproc_ximgproc_SparseMatchInterpolator_interpolate(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.SparseMatchInterpolator.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.XImgProc.SparseMatchInterpolator.t(), Evision.FileNode.t()) :: Evision.XImgProc.SparseMatchInterpolator.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.ximgproc_SparseMatchInterpolator_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.SparseMatchInterpolator.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.XImgProc.SparseMatchInterpolator.t(), binary()) :: Evision.XImgProc.SparseMatchInterpolator.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.ximgproc_SparseMatchInterpolator_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.SparseMatchInterpolator.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.XImgProc.SparseMatchInterpolator.t(), Evision.FileStorage.t(), binary()) :: Evision.XImgProc.SparseMatchInterpolator.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.ximgproc_SparseMatchInterpolator_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.SparseMatchInterpolator.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.XImgProc.SparseMatchInterpolator.t(), Evision.FileStorage.t()) :: Evision.XImgProc.SparseMatchInterpolator.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.ximgproc_SparseMatchInterpolator_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
