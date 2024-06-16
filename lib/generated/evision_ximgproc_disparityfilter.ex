defmodule Evision.XImgProc.DisparityFilter do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `XImgProc.DisparityFilter` struct.

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
  def to_struct({:ok, %{class: Evision.XImgProc.DisparityFilter, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.XImgProc.DisparityFilter, ref: ref}) do
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
  - **self**: `Evision.XImgProc.DisparityFilter.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.XImgProc.DisparityFilter.t()) :: Evision.XImgProc.DisparityFilter.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.ximgproc_DisparityFilter_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.DisparityFilter.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.XImgProc.DisparityFilter.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.ximgproc_DisparityFilter_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Apply filtering to the disparity map.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.DisparityFilter.t()`
  - **disparity_map_left**: `Evision.Mat`.

    disparity map of the left view, 1 channel, CV_16S type. Implicitly assumes that disparity
    values are scaled by 16 (one-pixel disparity corresponds to the value of 16 in the disparity map). Disparity map
    can have any resolution, it will be automatically resized to fit left_view resolution.

  - **left_view**: `Evision.Mat`.

    left view of the original stereo-pair to guide the filtering process, 8-bit single-channel
    or three-channel image.

  ##### Keyword Arguments
  - **disparity_map_right**: `Evision.Mat`.

    optional argument, some implementations might also use the disparity map
    of the right view to compute confidence maps, for instance.

  - **rOI**: `Rect`.

    region of the disparity map to filter. Optional, usually it should be set automatically.

  - **right_view**: `Evision.Mat`.

    optional argument, some implementations might also use the right view of the original
    stereo-pair.

  ##### Return
  - **filtered_disparity_map**: `Evision.Mat.t()`.

    output disparity map.

  Python prototype (for reference only):
  ```python3
  filter(disparity_map_left, left_view[, filtered_disparity_map[, disparity_map_right[, ROI[, right_view]]]]) -> filtered_disparity_map
  ```
  """
  @spec filter(Evision.XImgProc.DisparityFilter.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:disparity_map_right, term()} | {:rOI, term()} | {:right_view, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def filter(self, disparity_map_left, left_view, opts) when (is_struct(disparity_map_left, Evision.Mat) or is_struct(disparity_map_left, Nx.Tensor) or is_number(disparity_map_left) or is_tuple(disparity_map_left)) and (is_struct(left_view, Evision.Mat) or is_struct(left_view, Nx.Tensor) or is_number(left_view) or is_tuple(left_view)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:disparity_map_right, :rOI, :right_view])
    positional = [
      disparity_map_left: Evision.Internal.Structurise.from_struct(disparity_map_left),
      left_view: Evision.Internal.Structurise.from_struct(left_view)
    ]
    :evision_nif.ximgproc_ximgproc_DisparityFilter_filter(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Apply filtering to the disparity map.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.DisparityFilter.t()`
  - **disparity_map_left**: `Evision.Mat`.

    disparity map of the left view, 1 channel, CV_16S type. Implicitly assumes that disparity
    values are scaled by 16 (one-pixel disparity corresponds to the value of 16 in the disparity map). Disparity map
    can have any resolution, it will be automatically resized to fit left_view resolution.

  - **left_view**: `Evision.Mat`.

    left view of the original stereo-pair to guide the filtering process, 8-bit single-channel
    or three-channel image.

  ##### Keyword Arguments
  - **disparity_map_right**: `Evision.Mat`.

    optional argument, some implementations might also use the disparity map
    of the right view to compute confidence maps, for instance.

  - **rOI**: `Rect`.

    region of the disparity map to filter. Optional, usually it should be set automatically.

  - **right_view**: `Evision.Mat`.

    optional argument, some implementations might also use the right view of the original
    stereo-pair.

  ##### Return
  - **filtered_disparity_map**: `Evision.Mat.t()`.

    output disparity map.

  Python prototype (for reference only):
  ```python3
  filter(disparity_map_left, left_view[, filtered_disparity_map[, disparity_map_right[, ROI[, right_view]]]]) -> filtered_disparity_map
  ```
  """
  @spec filter(Evision.XImgProc.DisparityFilter.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def filter(self, disparity_map_left, left_view) when (is_struct(disparity_map_left, Evision.Mat) or is_struct(disparity_map_left, Nx.Tensor) or is_number(disparity_map_left) or is_tuple(disparity_map_left)) and (is_struct(left_view, Evision.Mat) or is_struct(left_view, Nx.Tensor) or is_number(left_view) or is_tuple(left_view))
  do
    positional = [
      disparity_map_left: Evision.Internal.Structurise.from_struct(disparity_map_left),
      left_view: Evision.Internal.Structurise.from_struct(left_view)
    ]
    :evision_nif.ximgproc_ximgproc_DisparityFilter_filter(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.DisparityFilter.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.XImgProc.DisparityFilter.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.ximgproc_DisparityFilter_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.DisparityFilter.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.XImgProc.DisparityFilter.t(), Evision.FileNode.t()) :: Evision.XImgProc.DisparityFilter.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.ximgproc_DisparityFilter_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.DisparityFilter.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.XImgProc.DisparityFilter.t(), binary()) :: Evision.XImgProc.DisparityFilter.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.ximgproc_DisparityFilter_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.DisparityFilter.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.XImgProc.DisparityFilter.t(), Evision.FileStorage.t(), binary()) :: Evision.XImgProc.DisparityFilter.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.ximgproc_DisparityFilter_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.DisparityFilter.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.XImgProc.DisparityFilter.t(), Evision.FileStorage.t()) :: Evision.XImgProc.DisparityFilter.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.ximgproc_DisparityFilter_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
