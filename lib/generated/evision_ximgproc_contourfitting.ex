defmodule Evision.XImgProc.ContourFitting do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `XImgProc.ContourFitting` struct.

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
  def to_struct({:ok, %{class: Evision.XImgProc.ContourFitting, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.XImgProc.ContourFitting, ref: ref}) do
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
  - **self**: `Evision.XImgProc.ContourFitting.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.XImgProc.ContourFitting.t()) :: Evision.XImgProc.ContourFitting.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.ximgproc_ContourFitting_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.ContourFitting.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.XImgProc.ContourFitting.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.ximgproc_ContourFitting_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Fit two closed curves using fourier descriptors. More details in @cite PersoonFu1977 and @cite BergerRaghunathan1998

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.ContourFitting.t()`
  - **src**: `Evision.Mat`.

    Contour defining first shape.

  - **dst**: `Evision.Mat`.

    Contour defining second shape (Target).

  ##### Keyword Arguments
  - **fdContour**: `bool`.

    false then src and dst are contours and true src and dst are fourier descriptors.

  ##### Return
  - **alphaPhiST**: `Evision.Mat.t()`.

    : \\f$ \\alpha \\f$=alphaPhiST(0,0), \\f$ \\phi \\f$=alphaPhiST(0,1) (in radian), s=alphaPhiST(0,2), Tx=alphaPhiST(0,3), Ty=alphaPhiST(0,4) rotation center

  - **dist**: `double`.

    distance between src and dst after matching.

  Python prototype (for reference only):
  ```python3
  estimateTransformation(src, dst[, alphaPhiST[, fdContour]]) -> alphaPhiST, dist
  ```
  """
  @spec estimateTransformation(Evision.XImgProc.ContourFitting.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:fdContour, term()}] | nil) :: {Evision.Mat.t(), number()} | {:error, String.t()}
  def estimateTransformation(self, src, dst, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (is_struct(dst, Evision.Mat) or is_struct(dst, Nx.Tensor) or is_number(dst) or is_tuple(dst)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:fdContour])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      dst: Evision.Internal.Structurise.from_struct(dst)
    ]
    :evision_nif.ximgproc_ximgproc_ContourFitting_estimateTransformation(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Fit two closed curves using fourier descriptors. More details in @cite PersoonFu1977 and @cite BergerRaghunathan1998

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.ContourFitting.t()`
  - **src**: `Evision.Mat`.

    Contour defining first shape.

  - **dst**: `Evision.Mat`.

    Contour defining second shape (Target).

  ##### Keyword Arguments
  - **fdContour**: `bool`.

    false then src and dst are contours and true src and dst are fourier descriptors.

  ##### Return
  - **alphaPhiST**: `Evision.Mat.t()`.

    : \\f$ \\alpha \\f$=alphaPhiST(0,0), \\f$ \\phi \\f$=alphaPhiST(0,1) (in radian), s=alphaPhiST(0,2), Tx=alphaPhiST(0,3), Ty=alphaPhiST(0,4) rotation center

  - **dist**: `double`.

    distance between src and dst after matching.

  Python prototype (for reference only):
  ```python3
  estimateTransformation(src, dst[, alphaPhiST[, fdContour]]) -> alphaPhiST, dist
  ```
  """
  @spec estimateTransformation(Evision.XImgProc.ContourFitting.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: {Evision.Mat.t(), number()} | {:error, String.t()}
  def estimateTransformation(self, src, dst) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (is_struct(dst, Evision.Mat) or is_struct(dst, Nx.Tensor) or is_number(dst) or is_tuple(dst))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      dst: Evision.Internal.Structurise.from_struct(dst)
    ]
    :evision_nif.ximgproc_ximgproc_ContourFitting_estimateTransformation(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getCtrSize

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.ContourFitting.t()`

  ##### Return
  - **retval**: `integer()`

  @returns number of fourier descriptors

  Python prototype (for reference only):
  ```python3
  getCtrSize() -> retval
  ```
  """
  @spec getCtrSize(Evision.XImgProc.ContourFitting.t()) :: integer() | {:error, String.t()}
  def getCtrSize(self) do
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_ContourFitting_getCtrSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.ContourFitting.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.XImgProc.ContourFitting.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.ximgproc_ContourFitting_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getFDSize

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.ContourFitting.t()`

  ##### Return
  - **retval**: `integer()`

  @returns number of fourier descriptors used for optimal curve matching

  Python prototype (for reference only):
  ```python3
  getFDSize() -> retval
  ```
  """
  @spec getFDSize(Evision.XImgProc.ContourFitting.t()) :: integer() | {:error, String.t()}
  def getFDSize(self) do
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_ContourFitting_getFDSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.ContourFitting.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.XImgProc.ContourFitting.t(), Evision.FileNode.t()) :: Evision.XImgProc.ContourFitting.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.ximgproc_ContourFitting_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.ContourFitting.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.XImgProc.ContourFitting.t(), binary()) :: Evision.XImgProc.ContourFitting.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.ximgproc_ContourFitting_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  set number of Fourier descriptors used in estimateTransformation

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.ContourFitting.t()`
  - **n**: `integer()`.

    number of Fourier descriptors equal to number of contour points after resampling.

  Python prototype (for reference only):
  ```python3
  setCtrSize(n) -> None
  ```
  """
  @spec setCtrSize(Evision.XImgProc.ContourFitting.t(), integer()) :: Evision.XImgProc.ContourFitting.t() | {:error, String.t()}
  def setCtrSize(self, n) when is_integer(n)
  do
    positional = [
      n: Evision.Internal.Structurise.from_struct(n)
    ]
    :evision_nif.ximgproc_ximgproc_ContourFitting_setCtrSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  set number of Fourier descriptors when estimateTransformation used vector<Point>

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.ContourFitting.t()`
  - **n**: `integer()`.

    number of fourier descriptors used for optimal curve matching.

  Python prototype (for reference only):
  ```python3
  setFDSize(n) -> None
  ```
  """
  @spec setFDSize(Evision.XImgProc.ContourFitting.t(), integer()) :: Evision.XImgProc.ContourFitting.t() | {:error, String.t()}
  def setFDSize(self, n) when is_integer(n)
  do
    positional = [
      n: Evision.Internal.Structurise.from_struct(n)
    ]
    :evision_nif.ximgproc_ximgproc_ContourFitting_setFDSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.ContourFitting.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.XImgProc.ContourFitting.t(), Evision.FileStorage.t(), binary()) :: Evision.XImgProc.ContourFitting.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.ximgproc_ContourFitting_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.ContourFitting.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.XImgProc.ContourFitting.t(), Evision.FileStorage.t()) :: Evision.XImgProc.ContourFitting.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.ximgproc_ContourFitting_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
