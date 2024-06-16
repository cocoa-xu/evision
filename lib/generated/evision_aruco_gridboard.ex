defmodule Evision.ArUco.GridBoard do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `ArUco.GridBoard` struct.

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
  def to_struct({:ok, %{class: Evision.ArUco.GridBoard, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.ArUco.GridBoard, ref: ref}) do
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
  GridBoard constructor

  ##### Positional Arguments
  - **size**: `Size`.

    number of markers in x and y directions

  - **markerLength**: `float`.

    marker side length (normally in meters)

  - **markerSeparation**: `float`.

    separation between two markers (same unit as markerLength)

  - **dictionary**: `Dictionary`.

    dictionary of markers indicating the type of markers

  ##### Keyword Arguments
  - **ids**: `Evision.Mat`.

    set of marker ids in dictionary to use on board.

  ##### Return
  - **self**: `GridBoard`

  Python prototype (for reference only):
  ```python3
  GridBoard(size, markerLength, markerSeparation, dictionary[, ids]) -> <aruco_GridBoard object>
  ```
  """
  @spec gridBoard({number(), number()}, number(), number(), Evision.ArUco.Dictionary.t(), [{:ids, term()}] | nil) :: Evision.ArUco.GridBoard.t() | {:error, String.t()}
  def gridBoard(size, markerLength, markerSeparation, dictionary, opts) when is_tuple(size) and is_float(markerLength) and is_float(markerSeparation) and is_struct(dictionary, Evision.ArUco.Dictionary) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:ids])
    positional = [
      size: Evision.Internal.Structurise.from_struct(size),
      markerLength: Evision.Internal.Structurise.from_struct(markerLength),
      markerSeparation: Evision.Internal.Structurise.from_struct(markerSeparation),
      dictionary: Evision.Internal.Structurise.from_struct(dictionary)
    ]
    :evision_nif.aruco_aruco_GridBoard_GridBoard(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  GridBoard constructor

  ##### Positional Arguments
  - **size**: `Size`.

    number of markers in x and y directions

  - **markerLength**: `float`.

    marker side length (normally in meters)

  - **markerSeparation**: `float`.

    separation between two markers (same unit as markerLength)

  - **dictionary**: `Dictionary`.

    dictionary of markers indicating the type of markers

  ##### Keyword Arguments
  - **ids**: `Evision.Mat`.

    set of marker ids in dictionary to use on board.

  ##### Return
  - **self**: `GridBoard`

  Python prototype (for reference only):
  ```python3
  GridBoard(size, markerLength, markerSeparation, dictionary[, ids]) -> <aruco_GridBoard object>
  ```
  """
  @spec gridBoard({number(), number()}, number(), number(), Evision.ArUco.Dictionary.t()) :: Evision.ArUco.GridBoard.t() | {:error, String.t()}
  def gridBoard(size, markerLength, markerSeparation, dictionary) when is_tuple(size) and is_float(markerLength) and is_float(markerSeparation) and is_struct(dictionary, Evision.ArUco.Dictionary)
  do
    positional = [
      size: Evision.Internal.Structurise.from_struct(size),
      markerLength: Evision.Internal.Structurise.from_struct(markerLength),
      markerSeparation: Evision.Internal.Structurise.from_struct(markerSeparation),
      dictionary: Evision.Internal.Structurise.from_struct(dictionary)
    ]
    :evision_nif.aruco_aruco_GridBoard_GridBoard(positional)
    |> to_struct()
  end

  @doc """
  getGridSize

  ##### Positional Arguments
  - **self**: `Evision.ArUco.GridBoard.t()`

  ##### Return
  - **retval**: `Size`

  Python prototype (for reference only):
  ```python3
  getGridSize() -> retval
  ```
  """
  @spec getGridSize(Evision.ArUco.GridBoard.t()) :: {number(), number()} | {:error, String.t()}
  def getGridSize(self) do
    positional = [
    ]
    :evision_nif.aruco_aruco_GridBoard_getGridSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMarkerLength

  ##### Positional Arguments
  - **self**: `Evision.ArUco.GridBoard.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  getMarkerLength() -> retval
  ```
  """
  @spec getMarkerLength(Evision.ArUco.GridBoard.t()) :: number() | {:error, String.t()}
  def getMarkerLength(self) do
    positional = [
    ]
    :evision_nif.aruco_aruco_GridBoard_getMarkerLength(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMarkerSeparation

  ##### Positional Arguments
  - **self**: `Evision.ArUco.GridBoard.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  getMarkerSeparation() -> retval
  ```
  """
  @spec getMarkerSeparation(Evision.ArUco.GridBoard.t()) :: number() | {:error, String.t()}
  def getMarkerSeparation(self) do
    positional = [
    ]
    :evision_nif.aruco_aruco_GridBoard_getMarkerSeparation(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
