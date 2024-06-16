defmodule Evision.ArUco.CharucoBoard do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `ArUco.CharucoBoard` struct.

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
  def to_struct({:ok, %{class: Evision.ArUco.CharucoBoard, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.ArUco.CharucoBoard, ref: ref}) do
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
  CharucoBoard constructor

  ##### Positional Arguments
  - **size**: `Size`.

    number of chessboard squares in x and y directions

  - **squareLength**: `float`.

    squareLength chessboard square side length (normally in meters)

  - **markerLength**: `float`.

    marker side length (same unit than squareLength)

  - **dictionary**: `Dictionary`.

    dictionary of markers indicating the type of markers

  ##### Keyword Arguments
  - **ids**: `Evision.Mat`.

    array of id used markers
    The first markers in the dictionary are used to fill the white chessboard squares.

  ##### Return
  - **self**: `Evision.ArUco.CharucoBoard.t()`

  Python prototype (for reference only):
  ```python3
  CharucoBoard(size, squareLength, markerLength, dictionary[, ids]) -> <aruco_CharucoBoard object>
  ```
  """
  @spec charucoBoard({number(), number()}, number(), number(), Evision.ArUco.Dictionary.t(), [{:ids, term()}] | nil) :: Evision.ArUco.CharucoBoard.t() | {:error, String.t()}
  def charucoBoard(size, squareLength, markerLength, dictionary, opts) when is_tuple(size) and is_float(squareLength) and is_float(markerLength) and is_struct(dictionary, Evision.ArUco.Dictionary) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:ids])
    positional = [
      size: Evision.Internal.Structurise.from_struct(size),
      squareLength: Evision.Internal.Structurise.from_struct(squareLength),
      markerLength: Evision.Internal.Structurise.from_struct(markerLength),
      dictionary: Evision.Internal.Structurise.from_struct(dictionary)
    ]
    :evision_nif.aruco_aruco_CharucoBoard_CharucoBoard(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  CharucoBoard constructor

  ##### Positional Arguments
  - **size**: `Size`.

    number of chessboard squares in x and y directions

  - **squareLength**: `float`.

    squareLength chessboard square side length (normally in meters)

  - **markerLength**: `float`.

    marker side length (same unit than squareLength)

  - **dictionary**: `Dictionary`.

    dictionary of markers indicating the type of markers

  ##### Keyword Arguments
  - **ids**: `Evision.Mat`.

    array of id used markers
    The first markers in the dictionary are used to fill the white chessboard squares.

  ##### Return
  - **self**: `Evision.ArUco.CharucoBoard.t()`

  Python prototype (for reference only):
  ```python3
  CharucoBoard(size, squareLength, markerLength, dictionary[, ids]) -> <aruco_CharucoBoard object>
  ```
  """
  @spec charucoBoard({number(), number()}, number(), number(), Evision.ArUco.Dictionary.t()) :: Evision.ArUco.CharucoBoard.t() | {:error, String.t()}
  def charucoBoard(size, squareLength, markerLength, dictionary) when is_tuple(size) and is_float(squareLength) and is_float(markerLength) and is_struct(dictionary, Evision.ArUco.Dictionary)
  do
    positional = [
      size: Evision.Internal.Structurise.from_struct(size),
      squareLength: Evision.Internal.Structurise.from_struct(squareLength),
      markerLength: Evision.Internal.Structurise.from_struct(markerLength),
      dictionary: Evision.Internal.Structurise.from_struct(dictionary)
    ]
    :evision_nif.aruco_aruco_CharucoBoard_CharucoBoard(positional)
    |> to_struct()
  end

  @doc """
  check whether the ChArUco markers are collinear

  ##### Positional Arguments
  - **self**: `Evision.ArUco.CharucoBoard.t()`
  - **charucoIds**: `Evision.Mat`.

    list of identifiers for each corner in charucoCorners per frame.

  ##### Return
  - **retval**: `bool`

  @return bool value, 1 (true) if detected corners form a line, 0 (false) if they do not.
   solvePnP, calibration functions will fail if the corners are collinear (true).
   The number of ids in charucoIDs should be <= the number of chessboard corners in the board.
   This functions checks whether the charuco corners are on a straight line (returns true, if so), or not (false).
   Axis parallel, as well as diagonal and other straight lines detected.  Degenerate cases:
   for number of charucoIDs <= 2,the function returns true.

  Python prototype (for reference only):
  ```python3
  checkCharucoCornersCollinear(charucoIds) -> retval
  ```
  """
  @spec checkCharucoCornersCollinear(Evision.ArUco.CharucoBoard.t(), Evision.Mat.maybe_mat_in()) :: boolean() | {:error, String.t()}
  def checkCharucoCornersCollinear(self, charucoIds) when (is_struct(charucoIds, Evision.Mat) or is_struct(charucoIds, Nx.Tensor) or is_number(charucoIds) or is_tuple(charucoIds))
  do
    positional = [
      charucoIds: Evision.Internal.Structurise.from_struct(charucoIds)
    ]
    :evision_nif.aruco_aruco_CharucoBoard_checkCharucoCornersCollinear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  get CharucoBoard::chessboardCorners

  ##### Positional Arguments
  - **self**: `Evision.ArUco.CharucoBoard.t()`

  ##### Return
  - **retval**: `[Point3f]`

  Python prototype (for reference only):
  ```python3
  getChessboardCorners() -> retval
  ```
  """
  @spec getChessboardCorners(Evision.ArUco.CharucoBoard.t()) :: list({number(), number(), number()}) | {:error, String.t()}
  def getChessboardCorners(self) do
    positional = [
    ]
    :evision_nif.aruco_aruco_CharucoBoard_getChessboardCorners(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getChessboardSize

  ##### Positional Arguments
  - **self**: `Evision.ArUco.CharucoBoard.t()`

  ##### Return
  - **retval**: `Size`

  Python prototype (for reference only):
  ```python3
  getChessboardSize() -> retval
  ```
  """
  @spec getChessboardSize(Evision.ArUco.CharucoBoard.t()) :: {number(), number()} | {:error, String.t()}
  def getChessboardSize(self) do
    positional = [
    ]
    :evision_nif.aruco_aruco_CharucoBoard_getChessboardSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getLegacyPattern

  ##### Positional Arguments
  - **self**: `Evision.ArUco.CharucoBoard.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  getLegacyPattern() -> retval
  ```
  """
  @spec getLegacyPattern(Evision.ArUco.CharucoBoard.t()) :: boolean() | {:error, String.t()}
  def getLegacyPattern(self) do
    positional = [
    ]
    :evision_nif.aruco_aruco_CharucoBoard_getLegacyPattern(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMarkerLength

  ##### Positional Arguments
  - **self**: `Evision.ArUco.CharucoBoard.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  getMarkerLength() -> retval
  ```
  """
  @spec getMarkerLength(Evision.ArUco.CharucoBoard.t()) :: number() | {:error, String.t()}
  def getMarkerLength(self) do
    positional = [
    ]
    :evision_nif.aruco_aruco_CharucoBoard_getMarkerLength(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getSquareLength

  ##### Positional Arguments
  - **self**: `Evision.ArUco.CharucoBoard.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  getSquareLength() -> retval
  ```
  """
  @spec getSquareLength(Evision.ArUco.CharucoBoard.t()) :: number() | {:error, String.t()}
  def getSquareLength(self) do
    positional = [
    ]
    :evision_nif.aruco_aruco_CharucoBoard_getSquareLength(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  set legacy chessboard pattern.

  ##### Positional Arguments
  - **self**: `Evision.ArUco.CharucoBoard.t()`
  - **legacyPattern**: `bool`

   Legacy setting creates chessboard patterns starting with a white box in the upper left corner
   if there is an even row count of chessboard boxes, otherwise it starts with a black box.
   This setting ensures compatibility to patterns created with OpenCV versions prior OpenCV 4.6.0.
   See https://github.com/opencv/opencv/issues/23152.
   Default value: false.

  Python prototype (for reference only):
  ```python3
  setLegacyPattern(legacyPattern) -> None
  ```
  """
  @spec setLegacyPattern(Evision.ArUco.CharucoBoard.t(), boolean()) :: Evision.ArUco.CharucoBoard.t() | {:error, String.t()}
  def setLegacyPattern(self, legacyPattern) when is_boolean(legacyPattern)
  do
    positional = [
      legacyPattern: Evision.Internal.Structurise.from_struct(legacyPattern)
    ]
    :evision_nif.aruco_aruco_CharucoBoard_setLegacyPattern(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
