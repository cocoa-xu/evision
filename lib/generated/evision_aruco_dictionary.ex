defmodule Evision.ArUco.Dictionary do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `ArUco.Dictionary` struct.

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
  def to_struct({:ok, %{class: Evision.ArUco.Dictionary, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.ArUco.Dictionary, ref: ref}) do
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
  Basic ArUco dictionary constructor

  ##### Positional Arguments
  - **bytesList**: `Evision.Mat`.

    bits for all ArUco markers in dictionary see memory layout in the class description

  - **markerSize**: `integer()`.

    ArUco marker size in units

  ##### Keyword Arguments
  - **maxcorr**: `integer()`.

    maximum number of bits that can be corrected

  ##### Return
  - **self**: `Dictionary`

  Python prototype (for reference only):
  ```python3
  Dictionary(bytesList, _markerSize[, maxcorr]) -> <aruco_Dictionary object>
  ```
  """
  @spec dictionary(Evision.Mat.maybe_mat_in(), integer(), [{:maxcorr, term()}] | nil) :: Evision.ArUco.Dictionary.t() | {:error, String.t()}
  def dictionary(bytesList, markerSize, opts) when (is_struct(bytesList, Evision.Mat) or is_struct(bytesList, Nx.Tensor) or is_number(bytesList) or is_tuple(bytesList)) and is_integer(markerSize) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:maxcorr])
    positional = [
      bytesList: Evision.Internal.Structurise.from_struct(bytesList),
      markerSize: Evision.Internal.Structurise.from_struct(markerSize)
    ]
    :evision_nif.aruco_aruco_Dictionary_Dictionary(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Basic ArUco dictionary constructor

  ##### Positional Arguments
  - **bytesList**: `Evision.Mat`.

    bits for all ArUco markers in dictionary see memory layout in the class description

  - **markerSize**: `integer()`.

    ArUco marker size in units

  ##### Keyword Arguments
  - **maxcorr**: `integer()`.

    maximum number of bits that can be corrected

  ##### Return
  - **self**: `Dictionary`

  Python prototype (for reference only):
  ```python3
  Dictionary(bytesList, _markerSize[, maxcorr]) -> <aruco_Dictionary object>
  ```
  """
  @spec dictionary(Evision.Mat.maybe_mat_in(), integer()) :: Evision.ArUco.Dictionary.t() | {:error, String.t()}
  def dictionary(bytesList, markerSize) when (is_struct(bytesList, Evision.Mat) or is_struct(bytesList, Nx.Tensor) or is_number(bytesList) or is_tuple(bytesList)) and is_integer(markerSize)
  do
    positional = [
      bytesList: Evision.Internal.Structurise.from_struct(bytesList),
      markerSize: Evision.Internal.Structurise.from_struct(markerSize)
    ]
    :evision_nif.aruco_aruco_Dictionary_Dictionary(positional)
    |> to_struct()
  end

  @doc """
  Dictionary
  ##### Return
  - **self**: `Dictionary`

  Python prototype (for reference only):
  ```python3
  Dictionary() -> <aruco_Dictionary object>
  ```
  """
  @spec dictionary() :: Evision.ArUco.Dictionary.t() | {:error, String.t()}
  def dictionary() do
    positional = [
    ]
    :evision_nif.aruco_aruco_Dictionary_Dictionary(positional)
    |> to_struct()
  end

  @doc """
  Generate a canonical marker image

  ##### Positional Arguments
  - **self**: `Evision.ArUco.Dictionary.t()`
  - **id**: `integer()`
  - **sidePixels**: `integer()`

  ##### Keyword Arguments
  - **borderBits**: `integer()`.

  ##### Return
  - **img**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  generateImageMarker(id, sidePixels[, _img[, borderBits]]) -> _img
  ```
  """
  @spec generateImageMarker(Evision.ArUco.Dictionary.t(), integer(), integer(), [{:borderBits, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def generateImageMarker(self, id, sidePixels, opts) when is_integer(id) and is_integer(sidePixels) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:borderBits])
    positional = [
      id: Evision.Internal.Structurise.from_struct(id),
      sidePixels: Evision.Internal.Structurise.from_struct(sidePixels)
    ]
    :evision_nif.aruco_aruco_Dictionary_generateImageMarker(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Generate a canonical marker image

  ##### Positional Arguments
  - **self**: `Evision.ArUco.Dictionary.t()`
  - **id**: `integer()`
  - **sidePixels**: `integer()`

  ##### Keyword Arguments
  - **borderBits**: `integer()`.

  ##### Return
  - **img**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  generateImageMarker(id, sidePixels[, _img[, borderBits]]) -> _img
  ```
  """
  @spec generateImageMarker(Evision.ArUco.Dictionary.t(), integer(), integer()) :: Evision.Mat.t() | {:error, String.t()}
  def generateImageMarker(self, id, sidePixels) when is_integer(id) and is_integer(sidePixels)
  do
    positional = [
      id: Evision.Internal.Structurise.from_struct(id),
      sidePixels: Evision.Internal.Structurise.from_struct(sidePixels)
    ]
    :evision_nif.aruco_aruco_Dictionary_generateImageMarker(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Transform list of bytes to matrix of bits

  ##### Positional Arguments
  - **byteList**: `Evision.Mat`
  - **markerSize**: `integer()`

  ##### Return
  - **retval**: `Evision.Mat.t()`

  Python prototype (for reference only):
  ```python3
  getBitsFromByteList(byteList, markerSize) -> retval
  ```
  """
  @spec getBitsFromByteList(Evision.Mat.maybe_mat_in(), integer()) :: Evision.Mat.t() | {:error, String.t()}
  def getBitsFromByteList(byteList, markerSize) when (is_struct(byteList, Evision.Mat) or is_struct(byteList, Nx.Tensor) or is_number(byteList) or is_tuple(byteList)) and is_integer(markerSize)
  do
    positional = [
      byteList: Evision.Internal.Structurise.from_struct(byteList),
      markerSize: Evision.Internal.Structurise.from_struct(markerSize)
    ]
    :evision_nif.aruco_aruco_Dictionary_getBitsFromByteList_static(positional)
    |> to_struct()
  end

  @doc """
  Transform matrix of bits to list of bytes with 4 marker rotations

  ##### Positional Arguments
  - **bits**: `Evision.Mat`

  ##### Return
  - **retval**: `Evision.Mat.t()`

  Python prototype (for reference only):
  ```python3
  getByteListFromBits(bits) -> retval
  ```
  """
  @spec getByteListFromBits(Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def getByteListFromBits(bits) when (is_struct(bits, Evision.Mat) or is_struct(bits, Nx.Tensor) or is_number(bits) or is_tuple(bits))
  do
    positional = [
      bits: Evision.Internal.Structurise.from_struct(bits)
    ]
    :evision_nif.aruco_aruco_Dictionary_getByteListFromBits_static(positional)
    |> to_struct()
  end

  @doc """
  Returns Hamming distance of the input bits to the specific id.

  ##### Positional Arguments
  - **self**: `Evision.ArUco.Dictionary.t()`
  - **bits**: `Evision.Mat`
  - **id**: `integer()`

  ##### Keyword Arguments
  - **allRotations**: `bool`.

  ##### Return
  - **retval**: `integer()`

   If `allRotations` flag is set, the four posible marker rotations are considered

  Python prototype (for reference only):
  ```python3
  getDistanceToId(bits, id[, allRotations]) -> retval
  ```
  """
  @spec getDistanceToId(Evision.ArUco.Dictionary.t(), Evision.Mat.maybe_mat_in(), integer(), [{:allRotations, term()}] | nil) :: integer() | {:error, String.t()}
  def getDistanceToId(self, bits, id, opts) when (is_struct(bits, Evision.Mat) or is_struct(bits, Nx.Tensor) or is_number(bits) or is_tuple(bits)) and is_integer(id) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:allRotations])
    positional = [
      bits: Evision.Internal.Structurise.from_struct(bits),
      id: Evision.Internal.Structurise.from_struct(id)
    ]
    :evision_nif.aruco_aruco_Dictionary_getDistanceToId(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Returns Hamming distance of the input bits to the specific id.

  ##### Positional Arguments
  - **self**: `Evision.ArUco.Dictionary.t()`
  - **bits**: `Evision.Mat`
  - **id**: `integer()`

  ##### Keyword Arguments
  - **allRotations**: `bool`.

  ##### Return
  - **retval**: `integer()`

   If `allRotations` flag is set, the four posible marker rotations are considered

  Python prototype (for reference only):
  ```python3
  getDistanceToId(bits, id[, allRotations]) -> retval
  ```
  """
  @spec getDistanceToId(Evision.ArUco.Dictionary.t(), Evision.Mat.maybe_mat_in(), integer()) :: integer() | {:error, String.t()}
  def getDistanceToId(self, bits, id) when (is_struct(bits, Evision.Mat) or is_struct(bits, Nx.Tensor) or is_number(bits) or is_tuple(bits)) and is_integer(id)
  do
    positional = [
      bits: Evision.Internal.Structurise.from_struct(bits),
      id: Evision.Internal.Structurise.from_struct(id)
    ]
    :evision_nif.aruco_aruco_Dictionary_getDistanceToId(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Given a matrix of bits. Returns whether if marker is identified or not.

  ##### Positional Arguments
  - **self**: `Evision.ArUco.Dictionary.t()`
  - **onlyBits**: `Evision.Mat`
  - **maxCorrectionRate**: `double`

  ##### Return
  - **retval**: `bool`
  - **idx**: `integer()`
  - **rotation**: `integer()`

   Returns reference to the marker id in the dictionary (if any) and its rotation.

  Python prototype (for reference only):
  ```python3
  identify(onlyBits, maxCorrectionRate) -> retval, idx, rotation
  ```
  """
  @spec identify(Evision.ArUco.Dictionary.t(), Evision.Mat.maybe_mat_in(), number()) :: {integer(), integer()} | false | {:error, String.t()}
  def identify(self, onlyBits, maxCorrectionRate) when (is_struct(onlyBits, Evision.Mat) or is_struct(onlyBits, Nx.Tensor) or is_number(onlyBits) or is_tuple(onlyBits)) and is_number(maxCorrectionRate)
  do
    positional = [
      onlyBits: Evision.Internal.Structurise.from_struct(onlyBits),
      maxCorrectionRate: Evision.Internal.Structurise.from_struct(maxCorrectionRate)
    ]
    :evision_nif.aruco_aruco_Dictionary_identify(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Read a new dictionary from FileNode.

  ##### Positional Arguments
  - **self**: `Evision.ArUco.Dictionary.t()`
  - **func**: `Evision.FileNode`

  ##### Return
  - **retval**: `bool`

   Dictionary example in YAML format:\\n
   nmarkers: 35\\n
   markersize: 6\\n
   maxCorrectionBits: 5\\n
   marker_0: "101011111011111001001001101100000000"\\n
   ...\\n
   marker_34: "011111010000111011111110110101100101"

  Python prototype (for reference only):
  ```python3
  readDictionary(fn) -> retval
  ```
  """
  @spec readDictionary(Evision.ArUco.Dictionary.t(), Evision.FileNode.t()) :: boolean() | {:error, String.t()}
  def readDictionary(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.aruco_aruco_Dictionary_readDictionary(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Write a dictionary to FileStorage, format is the same as in readDictionary().

  ##### Positional Arguments
  - **self**: `Evision.ArUco.Dictionary.t()`
  - **fs**: `Evision.FileStorage`

  ##### Keyword Arguments
  - **name**: `String`.

  Python prototype (for reference only):
  ```python3
  writeDictionary(fs[, name]) -> None
  ```
  """
  @spec writeDictionary(Evision.ArUco.Dictionary.t(), Evision.FileStorage.t(), [{:name, term()}] | nil) :: Evision.ArUco.Dictionary.t() | {:error, String.t()}
  def writeDictionary(self, fs, opts) when is_struct(fs, Evision.FileStorage) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:name])
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.aruco_aruco_Dictionary_writeDictionary(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Write a dictionary to FileStorage, format is the same as in readDictionary().

  ##### Positional Arguments
  - **self**: `Evision.ArUco.Dictionary.t()`
  - **fs**: `Evision.FileStorage`

  ##### Keyword Arguments
  - **name**: `String`.

  Python prototype (for reference only):
  ```python3
  writeDictionary(fs[, name]) -> None
  ```
  """
  @spec writeDictionary(Evision.ArUco.Dictionary.t(), Evision.FileStorage.t()) :: Evision.ArUco.Dictionary.t() | {:error, String.t()}
  def writeDictionary(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.aruco_aruco_Dictionary_writeDictionary(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec get_bytesList(Evision.ArUco.Dictionary.t()) :: Evision.Mat.t()
  def get_bytesList(self) do
    :evision_nif.aruco_Dictionary_get_bytesList(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_bytesList(Evision.ArUco.Dictionary.t(), Evision.Mat.maybe_mat_in()) :: Evision.ArUco.Dictionary.t()
  def set_bytesList(self, prop) do
    :evision_nif.aruco_Dictionary_set_bytesList(
        Evision.Internal.Structurise.from_struct(self),
        [bytesList: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_markerSize(Evision.ArUco.Dictionary.t()) :: integer()
  def get_markerSize(self) do
    :evision_nif.aruco_Dictionary_get_markerSize(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_markerSize(Evision.ArUco.Dictionary.t(), integer()) :: Evision.ArUco.Dictionary.t()
  def set_markerSize(self, prop) do
    :evision_nif.aruco_Dictionary_set_markerSize(
        Evision.Internal.Structurise.from_struct(self),
        [markerSize: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_maxCorrectionBits(Evision.ArUco.Dictionary.t()) :: integer()
  def get_maxCorrectionBits(self) do
    :evision_nif.aruco_Dictionary_get_maxCorrectionBits(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_maxCorrectionBits(Evision.ArUco.Dictionary.t(), integer()) :: Evision.ArUco.Dictionary.t()
  def set_maxCorrectionBits(self, prop) do
    :evision_nif.aruco_Dictionary_set_maxCorrectionBits(
        Evision.Internal.Structurise.from_struct(self),
        [maxCorrectionBits: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
end
