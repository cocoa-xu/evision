defmodule Evision.XImgProc.StructuredEdgeDetection do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `XImgProc.StructuredEdgeDetection` struct.

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
  def to_struct({:ok, %{class: Evision.XImgProc.StructuredEdgeDetection, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.XImgProc.StructuredEdgeDetection, ref: ref}) do
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
  - **self**: `Evision.XImgProc.StructuredEdgeDetection.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.XImgProc.StructuredEdgeDetection.t()) :: Evision.XImgProc.StructuredEdgeDetection.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.ximgproc_StructuredEdgeDetection_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  The function computes orientation from edge image.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.StructuredEdgeDetection.t()`
  - **src**: `Evision.Mat`.

    edge image.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    orientation image.

  Python prototype (for reference only):
  ```python3
  computeOrientation(src[, dst]) -> dst
  ```
  """
  @spec computeOrientation(Evision.XImgProc.StructuredEdgeDetection.t(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def computeOrientation(self, src, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.ximgproc_ximgproc_StructuredEdgeDetection_computeOrientation(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  The function computes orientation from edge image.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.StructuredEdgeDetection.t()`
  - **src**: `Evision.Mat`.

    edge image.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    orientation image.

  Python prototype (for reference only):
  ```python3
  computeOrientation(src[, dst]) -> dst
  ```
  """
  @spec computeOrientation(Evision.XImgProc.StructuredEdgeDetection.t(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def computeOrientation(self, src) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.ximgproc_ximgproc_StructuredEdgeDetection_computeOrientation(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  The function detects edges in src and draw them to dst.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.StructuredEdgeDetection.t()`
  - **src**: `Evision.Mat`.

    source image (RGB, float, in [0;1]) to detect edges

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    destination image (grayscale, float, in [0;1]) where edges are drawn

  The algorithm underlies this function is much more robust to texture presence, than common
  approaches, e.g. Sobel
  @sa Sobel, Canny

  Python prototype (for reference only):
  ```python3
  detectEdges(src[, dst]) -> dst
  ```
  """
  @spec detectEdges(Evision.XImgProc.StructuredEdgeDetection.t(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def detectEdges(self, src, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.ximgproc_ximgproc_StructuredEdgeDetection_detectEdges(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  The function detects edges in src and draw them to dst.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.StructuredEdgeDetection.t()`
  - **src**: `Evision.Mat`.

    source image (RGB, float, in [0;1]) to detect edges

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    destination image (grayscale, float, in [0;1]) where edges are drawn

  The algorithm underlies this function is much more robust to texture presence, than common
  approaches, e.g. Sobel
  @sa Sobel, Canny

  Python prototype (for reference only):
  ```python3
  detectEdges(src[, dst]) -> dst
  ```
  """
  @spec detectEdges(Evision.XImgProc.StructuredEdgeDetection.t(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def detectEdges(self, src) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.ximgproc_ximgproc_StructuredEdgeDetection_detectEdges(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  The function edgenms in edge image and suppress edges where edge is stronger in orthogonal direction.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.StructuredEdgeDetection.t()`
  - **edge_image**: `Evision.Mat`.

    edge image from detectEdges function.

  - **orientation_image**: `Evision.Mat`.

    orientation image from computeOrientation function.

  ##### Keyword Arguments
  - **r**: `integer()`.

    radius for NMS suppression.

  - **s**: `integer()`.

    radius for boundary suppression.

  - **m**: `float`.

    multiplier for conservative suppression.

  - **isParallel**: `bool`.

    enables/disables parallel computing.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    suppressed image (grayscale, float, in [0;1])

  Python prototype (for reference only):
  ```python3
  edgesNms(edge_image, orientation_image[, dst[, r[, s[, m[, isParallel]]]]]) -> dst
  ```
  """
  @spec edgesNms(Evision.XImgProc.StructuredEdgeDetection.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:isParallel, term()} | {:m, term()} | {:r, term()} | {:s, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def edgesNms(self, edge_image, orientation_image, opts) when (is_struct(edge_image, Evision.Mat) or is_struct(edge_image, Nx.Tensor) or is_number(edge_image) or is_tuple(edge_image)) and (is_struct(orientation_image, Evision.Mat) or is_struct(orientation_image, Nx.Tensor) or is_number(orientation_image) or is_tuple(orientation_image)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:isParallel, :m, :r, :s])
    positional = [
      edge_image: Evision.Internal.Structurise.from_struct(edge_image),
      orientation_image: Evision.Internal.Structurise.from_struct(orientation_image)
    ]
    :evision_nif.ximgproc_ximgproc_StructuredEdgeDetection_edgesNms(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  The function edgenms in edge image and suppress edges where edge is stronger in orthogonal direction.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.StructuredEdgeDetection.t()`
  - **edge_image**: `Evision.Mat`.

    edge image from detectEdges function.

  - **orientation_image**: `Evision.Mat`.

    orientation image from computeOrientation function.

  ##### Keyword Arguments
  - **r**: `integer()`.

    radius for NMS suppression.

  - **s**: `integer()`.

    radius for boundary suppression.

  - **m**: `float`.

    multiplier for conservative suppression.

  - **isParallel**: `bool`.

    enables/disables parallel computing.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    suppressed image (grayscale, float, in [0;1])

  Python prototype (for reference only):
  ```python3
  edgesNms(edge_image, orientation_image[, dst[, r[, s[, m[, isParallel]]]]]) -> dst
  ```
  """
  @spec edgesNms(Evision.XImgProc.StructuredEdgeDetection.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def edgesNms(self, edge_image, orientation_image) when (is_struct(edge_image, Evision.Mat) or is_struct(edge_image, Nx.Tensor) or is_number(edge_image) or is_tuple(edge_image)) and (is_struct(orientation_image, Evision.Mat) or is_struct(orientation_image, Nx.Tensor) or is_number(orientation_image) or is_tuple(orientation_image))
  do
    positional = [
      edge_image: Evision.Internal.Structurise.from_struct(edge_image),
      orientation_image: Evision.Internal.Structurise.from_struct(orientation_image)
    ]
    :evision_nif.ximgproc_ximgproc_StructuredEdgeDetection_edgesNms(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.StructuredEdgeDetection.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.XImgProc.StructuredEdgeDetection.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.ximgproc_StructuredEdgeDetection_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.StructuredEdgeDetection.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.XImgProc.StructuredEdgeDetection.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.ximgproc_StructuredEdgeDetection_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.StructuredEdgeDetection.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.XImgProc.StructuredEdgeDetection.t(), Evision.FileNode.t()) :: Evision.XImgProc.StructuredEdgeDetection.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.ximgproc_StructuredEdgeDetection_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.StructuredEdgeDetection.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.XImgProc.StructuredEdgeDetection.t(), binary()) :: Evision.XImgProc.StructuredEdgeDetection.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.ximgproc_StructuredEdgeDetection_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.StructuredEdgeDetection.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.XImgProc.StructuredEdgeDetection.t(), Evision.FileStorage.t(), binary()) :: Evision.XImgProc.StructuredEdgeDetection.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.ximgproc_StructuredEdgeDetection_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.StructuredEdgeDetection.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.XImgProc.StructuredEdgeDetection.t(), Evision.FileStorage.t()) :: Evision.XImgProc.StructuredEdgeDetection.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.ximgproc_StructuredEdgeDetection_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
