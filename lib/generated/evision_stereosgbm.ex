defmodule Evision.StereoSGBM do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `StereoSGBM` struct.

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
  def to_struct({:ok, %{class: Evision.StereoSGBM, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.StereoSGBM, ref: ref}) do
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
  @type enum :: integer()
  @doc enum: true
  def cv_MODE_SGBM, do: 0
  @doc enum: true
  def cv_MODE_HH, do: 1
  @doc enum: true
  def cv_MODE_SGBM_3WAY, do: 2
  @doc enum: true
  def cv_MODE_HH4, do: 3


  @doc """
  Clears the algorithm state

  ##### Positional Arguments
  - **self**: `Evision.StereoSGBM.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.StereoSGBM.t()) :: Evision.StereoSGBM.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.stereoSGBM_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Computes disparity map for the specified stereo pair

  ##### Positional Arguments
  - **self**: `Evision.StereoSGBM.t()`
  - **left**: `Evision.Mat`.

    Left 8-bit single-channel image.

  - **right**: `Evision.Mat`.

    Right image of the same size and the same type as the left one.

  ##### Return
  - **disparity**: `Evision.Mat.t()`.

    Output disparity map. It has the same size as the input images. Some algorithms,
    like StereoBM or StereoSGBM compute 16-bit fixed-point disparity map (where each disparity value
    has 4 fractional bits), whereas other algorithms output 32-bit floating-point disparity map.

  Python prototype (for reference only):
  ```python3
  compute(left, right[, disparity]) -> disparity
  ```
  """
  @spec compute(Evision.StereoSGBM.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def compute(self, left, right, opts) when (is_struct(left, Evision.Mat) or is_struct(left, Nx.Tensor) or is_number(left) or is_tuple(left)) and (is_struct(right, Evision.Mat) or is_struct(right, Nx.Tensor) or is_number(right) or is_tuple(right)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      left: Evision.Internal.Structurise.from_struct(left),
      right: Evision.Internal.Structurise.from_struct(right)
    ]
    :evision_nif.stereoSGBM_compute(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Computes disparity map for the specified stereo pair

  ##### Positional Arguments
  - **self**: `Evision.StereoSGBM.t()`
  - **left**: `Evision.Mat`.

    Left 8-bit single-channel image.

  - **right**: `Evision.Mat`.

    Right image of the same size and the same type as the left one.

  ##### Return
  - **disparity**: `Evision.Mat.t()`.

    Output disparity map. It has the same size as the input images. Some algorithms,
    like StereoBM or StereoSGBM compute 16-bit fixed-point disparity map (where each disparity value
    has 4 fractional bits), whereas other algorithms output 32-bit floating-point disparity map.

  Python prototype (for reference only):
  ```python3
  compute(left, right[, disparity]) -> disparity
  ```
  """
  @spec compute(Evision.StereoSGBM.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def compute(self, left, right) when (is_struct(left, Evision.Mat) or is_struct(left, Nx.Tensor) or is_number(left) or is_tuple(left)) and (is_struct(right, Evision.Mat) or is_struct(right, Nx.Tensor) or is_number(right) or is_tuple(right))
  do
    positional = [
      left: Evision.Internal.Structurise.from_struct(left),
      right: Evision.Internal.Structurise.from_struct(right)
    ]
    :evision_nif.stereoSGBM_compute(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Creates StereoSGBM object
  ##### Keyword Arguments
  - **minDisparity**: `integer()`.

    Minimum possible disparity value. Normally, it is zero but sometimes
    rectification algorithms can shift images, so this parameter needs to be adjusted accordingly.

  - **numDisparities**: `integer()`.

    Maximum disparity minus minimum disparity. The value is always greater than
    zero. In the current implementation, this parameter must be divisible by 16.

  - **blockSize**: `integer()`.

    Matched block size. It must be an odd number \\>=1 . Normally, it should be
    somewhere in the 3..11 range.

  - **p1**: `integer()`.

    The first parameter controlling the disparity smoothness. See below.

  - **p2**: `integer()`.

    The second parameter controlling the disparity smoothness. The larger the values are,
    the smoother the disparity is. P1 is the penalty on the disparity change by plus or minus 1
    between neighbor pixels. P2 is the penalty on the disparity change by more than 1 between neighbor
    pixels. The algorithm requires P2 \\> P1 . See stereo_match.cpp sample where some reasonably good
    P1 and P2 values are shown (like 8\\*number_of_image_channels\\*blockSize\\*blockSize and
    32\\*number_of_image_channels\\*blockSize\\*blockSize , respectively).

  - **disp12MaxDiff**: `integer()`.

    Maximum allowed difference (in integer pixel units) in the left-right
    disparity check. Set it to a non-positive value to disable the check.

  - **preFilterCap**: `integer()`.

    Truncation value for the prefiltered image pixels. The algorithm first
    computes x-derivative at each pixel and clips its value by [-preFilterCap, preFilterCap] interval.
    The result values are passed to the Birchfield-Tomasi pixel cost function.

  - **uniquenessRatio**: `integer()`.

    Margin in percentage by which the best (minimum) computed cost function
    value should "win" the second best value to consider the found match correct. Normally, a value
    within the 5-15 range is good enough.

  - **speckleWindowSize**: `integer()`.

    Maximum size of smooth disparity regions to consider their noise speckles
    and invalidate. Set it to 0 to disable speckle filtering. Otherwise, set it somewhere in the
    50-200 range.

  - **speckleRange**: `integer()`.

    Maximum disparity variation within each connected component. If you do speckle
    filtering, set the parameter to a positive value, it will be implicitly multiplied by 16.
    Normally, 1 or 2 is good enough.

  - **mode**: `integer()`.

    Set it to StereoSGBM::MODE_HH to run the full-scale two-pass dynamic programming
    algorithm. It will consume O(W\\*H\\*numDisparities) bytes, which is large for 640x480 stereo and
    huge for HD-size pictures. By default, it is set to false .

  ##### Return
  - **retval**: `Evision.StereoSGBM.t()`

  The first constructor initializes StereoSGBM with all the default parameters. So, you only have to
  set StereoSGBM::numDisparities at minimum. The second constructor enables you to set each parameter
  to a custom value.

  Python prototype (for reference only):
  ```python3
  create([, minDisparity[, numDisparities[, blockSize[, P1[, P2[, disp12MaxDiff[, preFilterCap[, uniquenessRatio[, speckleWindowSize[, speckleRange[, mode]]]]]]]]]]]) -> retval
  ```
  """
  @spec create([{:blockSize, term()} | {:disp12MaxDiff, term()} | {:minDisparity, term()} | {:mode, term()} | {:numDisparities, term()} | {:p1, term()} | {:p2, term()} | {:preFilterCap, term()} | {:speckleRange, term()} | {:speckleWindowSize, term()} | {:uniquenessRatio, term()}] | nil) :: Evision.StereoSGBM.t() | {:error, String.t()}
  def create(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:blockSize, :disp12MaxDiff, :minDisparity, :mode, :numDisparities, :p1, :p2, :preFilterCap, :speckleRange, :speckleWindowSize, :uniquenessRatio])
    positional = [
    ]
    :evision_nif.stereoSGBM_create_static(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Creates StereoSGBM object
  ##### Keyword Arguments
  - **minDisparity**: `integer()`.

    Minimum possible disparity value. Normally, it is zero but sometimes
    rectification algorithms can shift images, so this parameter needs to be adjusted accordingly.

  - **numDisparities**: `integer()`.

    Maximum disparity minus minimum disparity. The value is always greater than
    zero. In the current implementation, this parameter must be divisible by 16.

  - **blockSize**: `integer()`.

    Matched block size. It must be an odd number \\>=1 . Normally, it should be
    somewhere in the 3..11 range.

  - **p1**: `integer()`.

    The first parameter controlling the disparity smoothness. See below.

  - **p2**: `integer()`.

    The second parameter controlling the disparity smoothness. The larger the values are,
    the smoother the disparity is. P1 is the penalty on the disparity change by plus or minus 1
    between neighbor pixels. P2 is the penalty on the disparity change by more than 1 between neighbor
    pixels. The algorithm requires P2 \\> P1 . See stereo_match.cpp sample where some reasonably good
    P1 and P2 values are shown (like 8\\*number_of_image_channels\\*blockSize\\*blockSize and
    32\\*number_of_image_channels\\*blockSize\\*blockSize , respectively).

  - **disp12MaxDiff**: `integer()`.

    Maximum allowed difference (in integer pixel units) in the left-right
    disparity check. Set it to a non-positive value to disable the check.

  - **preFilterCap**: `integer()`.

    Truncation value for the prefiltered image pixels. The algorithm first
    computes x-derivative at each pixel and clips its value by [-preFilterCap, preFilterCap] interval.
    The result values are passed to the Birchfield-Tomasi pixel cost function.

  - **uniquenessRatio**: `integer()`.

    Margin in percentage by which the best (minimum) computed cost function
    value should "win" the second best value to consider the found match correct. Normally, a value
    within the 5-15 range is good enough.

  - **speckleWindowSize**: `integer()`.

    Maximum size of smooth disparity regions to consider their noise speckles
    and invalidate. Set it to 0 to disable speckle filtering. Otherwise, set it somewhere in the
    50-200 range.

  - **speckleRange**: `integer()`.

    Maximum disparity variation within each connected component. If you do speckle
    filtering, set the parameter to a positive value, it will be implicitly multiplied by 16.
    Normally, 1 or 2 is good enough.

  - **mode**: `integer()`.

    Set it to StereoSGBM::MODE_HH to run the full-scale two-pass dynamic programming
    algorithm. It will consume O(W\\*H\\*numDisparities) bytes, which is large for 640x480 stereo and
    huge for HD-size pictures. By default, it is set to false .

  ##### Return
  - **retval**: `Evision.StereoSGBM.t()`

  The first constructor initializes StereoSGBM with all the default parameters. So, you only have to
  set StereoSGBM::numDisparities at minimum. The second constructor enables you to set each parameter
  to a custom value.

  Python prototype (for reference only):
  ```python3
  create([, minDisparity[, numDisparities[, blockSize[, P1[, P2[, disp12MaxDiff[, preFilterCap[, uniquenessRatio[, speckleWindowSize[, speckleRange[, mode]]]]]]]]]]]) -> retval
  ```
  """
  @spec create() :: Evision.StereoSGBM.t() | {:error, String.t()}
  def create() do
    positional = [
    ]
    :evision_nif.stereoSGBM_create_static(positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.StereoSGBM.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.StereoSGBM.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.stereoSGBM_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getBlockSize

  ##### Positional Arguments
  - **self**: `Evision.StereoSGBM.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getBlockSize() -> retval
  ```
  """
  @spec getBlockSize(Evision.StereoSGBM.t()) :: integer() | {:error, String.t()}
  def getBlockSize(self) do
    positional = [
    ]
    :evision_nif.stereoSGBM_getBlockSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.StereoSGBM.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.StereoSGBM.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.stereoSGBM_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDisp12MaxDiff

  ##### Positional Arguments
  - **self**: `Evision.StereoSGBM.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getDisp12MaxDiff() -> retval
  ```
  """
  @spec getDisp12MaxDiff(Evision.StereoSGBM.t()) :: integer() | {:error, String.t()}
  def getDisp12MaxDiff(self) do
    positional = [
    ]
    :evision_nif.stereoSGBM_getDisp12MaxDiff(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMinDisparity

  ##### Positional Arguments
  - **self**: `Evision.StereoSGBM.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getMinDisparity() -> retval
  ```
  """
  @spec getMinDisparity(Evision.StereoSGBM.t()) :: integer() | {:error, String.t()}
  def getMinDisparity(self) do
    positional = [
    ]
    :evision_nif.stereoSGBM_getMinDisparity(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMode

  ##### Positional Arguments
  - **self**: `Evision.StereoSGBM.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getMode() -> retval
  ```
  """
  @spec getMode(Evision.StereoSGBM.t()) :: integer() | {:error, String.t()}
  def getMode(self) do
    positional = [
    ]
    :evision_nif.stereoSGBM_getMode(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getNumDisparities

  ##### Positional Arguments
  - **self**: `Evision.StereoSGBM.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getNumDisparities() -> retval
  ```
  """
  @spec getNumDisparities(Evision.StereoSGBM.t()) :: integer() | {:error, String.t()}
  def getNumDisparities(self) do
    positional = [
    ]
    :evision_nif.stereoSGBM_getNumDisparities(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getP1

  ##### Positional Arguments
  - **self**: `Evision.StereoSGBM.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getP1() -> retval
  ```
  """
  @spec getP1(Evision.StereoSGBM.t()) :: integer() | {:error, String.t()}
  def getP1(self) do
    positional = [
    ]
    :evision_nif.stereoSGBM_getP1(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getP2

  ##### Positional Arguments
  - **self**: `Evision.StereoSGBM.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getP2() -> retval
  ```
  """
  @spec getP2(Evision.StereoSGBM.t()) :: integer() | {:error, String.t()}
  def getP2(self) do
    positional = [
    ]
    :evision_nif.stereoSGBM_getP2(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getPreFilterCap

  ##### Positional Arguments
  - **self**: `Evision.StereoSGBM.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getPreFilterCap() -> retval
  ```
  """
  @spec getPreFilterCap(Evision.StereoSGBM.t()) :: integer() | {:error, String.t()}
  def getPreFilterCap(self) do
    positional = [
    ]
    :evision_nif.stereoSGBM_getPreFilterCap(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getSpeckleRange

  ##### Positional Arguments
  - **self**: `Evision.StereoSGBM.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getSpeckleRange() -> retval
  ```
  """
  @spec getSpeckleRange(Evision.StereoSGBM.t()) :: integer() | {:error, String.t()}
  def getSpeckleRange(self) do
    positional = [
    ]
    :evision_nif.stereoSGBM_getSpeckleRange(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getSpeckleWindowSize

  ##### Positional Arguments
  - **self**: `Evision.StereoSGBM.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getSpeckleWindowSize() -> retval
  ```
  """
  @spec getSpeckleWindowSize(Evision.StereoSGBM.t()) :: integer() | {:error, String.t()}
  def getSpeckleWindowSize(self) do
    positional = [
    ]
    :evision_nif.stereoSGBM_getSpeckleWindowSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getUniquenessRatio

  ##### Positional Arguments
  - **self**: `Evision.StereoSGBM.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getUniquenessRatio() -> retval
  ```
  """
  @spec getUniquenessRatio(Evision.StereoSGBM.t()) :: integer() | {:error, String.t()}
  def getUniquenessRatio(self) do
    positional = [
    ]
    :evision_nif.stereoSGBM_getUniquenessRatio(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.StereoSGBM.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.StereoSGBM.t(), Evision.FileNode.t()) :: Evision.StereoSGBM.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.stereoSGBM_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.StereoSGBM.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.StereoSGBM.t(), binary()) :: Evision.StereoSGBM.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.stereoSGBM_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setBlockSize

  ##### Positional Arguments
  - **self**: `Evision.StereoSGBM.t()`
  - **blockSize**: `integer()`

  Python prototype (for reference only):
  ```python3
  setBlockSize(blockSize) -> None
  ```
  """
  @spec setBlockSize(Evision.StereoSGBM.t(), integer()) :: Evision.StereoSGBM.t() | {:error, String.t()}
  def setBlockSize(self, blockSize) when is_integer(blockSize)
  do
    positional = [
      blockSize: Evision.Internal.Structurise.from_struct(blockSize)
    ]
    :evision_nif.stereoSGBM_setBlockSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setDisp12MaxDiff

  ##### Positional Arguments
  - **self**: `Evision.StereoSGBM.t()`
  - **disp12MaxDiff**: `integer()`

  Python prototype (for reference only):
  ```python3
  setDisp12MaxDiff(disp12MaxDiff) -> None
  ```
  """
  @spec setDisp12MaxDiff(Evision.StereoSGBM.t(), integer()) :: Evision.StereoSGBM.t() | {:error, String.t()}
  def setDisp12MaxDiff(self, disp12MaxDiff) when is_integer(disp12MaxDiff)
  do
    positional = [
      disp12MaxDiff: Evision.Internal.Structurise.from_struct(disp12MaxDiff)
    ]
    :evision_nif.stereoSGBM_setDisp12MaxDiff(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMinDisparity

  ##### Positional Arguments
  - **self**: `Evision.StereoSGBM.t()`
  - **minDisparity**: `integer()`

  Python prototype (for reference only):
  ```python3
  setMinDisparity(minDisparity) -> None
  ```
  """
  @spec setMinDisparity(Evision.StereoSGBM.t(), integer()) :: Evision.StereoSGBM.t() | {:error, String.t()}
  def setMinDisparity(self, minDisparity) when is_integer(minDisparity)
  do
    positional = [
      minDisparity: Evision.Internal.Structurise.from_struct(minDisparity)
    ]
    :evision_nif.stereoSGBM_setMinDisparity(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMode

  ##### Positional Arguments
  - **self**: `Evision.StereoSGBM.t()`
  - **mode**: `integer()`

  Python prototype (for reference only):
  ```python3
  setMode(mode) -> None
  ```
  """
  @spec setMode(Evision.StereoSGBM.t(), integer()) :: Evision.StereoSGBM.t() | {:error, String.t()}
  def setMode(self, mode) when is_integer(mode)
  do
    positional = [
      mode: Evision.Internal.Structurise.from_struct(mode)
    ]
    :evision_nif.stereoSGBM_setMode(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setNumDisparities

  ##### Positional Arguments
  - **self**: `Evision.StereoSGBM.t()`
  - **numDisparities**: `integer()`

  Python prototype (for reference only):
  ```python3
  setNumDisparities(numDisparities) -> None
  ```
  """
  @spec setNumDisparities(Evision.StereoSGBM.t(), integer()) :: Evision.StereoSGBM.t() | {:error, String.t()}
  def setNumDisparities(self, numDisparities) when is_integer(numDisparities)
  do
    positional = [
      numDisparities: Evision.Internal.Structurise.from_struct(numDisparities)
    ]
    :evision_nif.stereoSGBM_setNumDisparities(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setP1

  ##### Positional Arguments
  - **self**: `Evision.StereoSGBM.t()`
  - **p1**: `integer()`

  Python prototype (for reference only):
  ```python3
  setP1(P1) -> None
  ```
  """
  @spec setP1(Evision.StereoSGBM.t(), integer()) :: Evision.StereoSGBM.t() | {:error, String.t()}
  def setP1(self, p1) when is_integer(p1)
  do
    positional = [
      p1: Evision.Internal.Structurise.from_struct(p1)
    ]
    :evision_nif.stereoSGBM_setP1(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setP2

  ##### Positional Arguments
  - **self**: `Evision.StereoSGBM.t()`
  - **p2**: `integer()`

  Python prototype (for reference only):
  ```python3
  setP2(P2) -> None
  ```
  """
  @spec setP2(Evision.StereoSGBM.t(), integer()) :: Evision.StereoSGBM.t() | {:error, String.t()}
  def setP2(self, p2) when is_integer(p2)
  do
    positional = [
      p2: Evision.Internal.Structurise.from_struct(p2)
    ]
    :evision_nif.stereoSGBM_setP2(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setPreFilterCap

  ##### Positional Arguments
  - **self**: `Evision.StereoSGBM.t()`
  - **preFilterCap**: `integer()`

  Python prototype (for reference only):
  ```python3
  setPreFilterCap(preFilterCap) -> None
  ```
  """
  @spec setPreFilterCap(Evision.StereoSGBM.t(), integer()) :: Evision.StereoSGBM.t() | {:error, String.t()}
  def setPreFilterCap(self, preFilterCap) when is_integer(preFilterCap)
  do
    positional = [
      preFilterCap: Evision.Internal.Structurise.from_struct(preFilterCap)
    ]
    :evision_nif.stereoSGBM_setPreFilterCap(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setSpeckleRange

  ##### Positional Arguments
  - **self**: `Evision.StereoSGBM.t()`
  - **speckleRange**: `integer()`

  Python prototype (for reference only):
  ```python3
  setSpeckleRange(speckleRange) -> None
  ```
  """
  @spec setSpeckleRange(Evision.StereoSGBM.t(), integer()) :: Evision.StereoSGBM.t() | {:error, String.t()}
  def setSpeckleRange(self, speckleRange) when is_integer(speckleRange)
  do
    positional = [
      speckleRange: Evision.Internal.Structurise.from_struct(speckleRange)
    ]
    :evision_nif.stereoSGBM_setSpeckleRange(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setSpeckleWindowSize

  ##### Positional Arguments
  - **self**: `Evision.StereoSGBM.t()`
  - **speckleWindowSize**: `integer()`

  Python prototype (for reference only):
  ```python3
  setSpeckleWindowSize(speckleWindowSize) -> None
  ```
  """
  @spec setSpeckleWindowSize(Evision.StereoSGBM.t(), integer()) :: Evision.StereoSGBM.t() | {:error, String.t()}
  def setSpeckleWindowSize(self, speckleWindowSize) when is_integer(speckleWindowSize)
  do
    positional = [
      speckleWindowSize: Evision.Internal.Structurise.from_struct(speckleWindowSize)
    ]
    :evision_nif.stereoSGBM_setSpeckleWindowSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setUniquenessRatio

  ##### Positional Arguments
  - **self**: `Evision.StereoSGBM.t()`
  - **uniquenessRatio**: `integer()`

  Python prototype (for reference only):
  ```python3
  setUniquenessRatio(uniquenessRatio) -> None
  ```
  """
  @spec setUniquenessRatio(Evision.StereoSGBM.t(), integer()) :: Evision.StereoSGBM.t() | {:error, String.t()}
  def setUniquenessRatio(self, uniquenessRatio) when is_integer(uniquenessRatio)
  do
    positional = [
      uniquenessRatio: Evision.Internal.Structurise.from_struct(uniquenessRatio)
    ]
    :evision_nif.stereoSGBM_setUniquenessRatio(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.StereoSGBM.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.StereoSGBM.t(), Evision.FileStorage.t(), binary()) :: Evision.StereoSGBM.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.stereoSGBM_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.StereoSGBM.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.StereoSGBM.t(), Evision.FileStorage.t()) :: Evision.StereoSGBM.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.stereoSGBM_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
