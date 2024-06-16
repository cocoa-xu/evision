defmodule Evision.MCC.CCheckerDetector do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `MCC.CCheckerDetector` struct.

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
  def to_struct({:ok, %{class: Evision.MCC.CCheckerDetector, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.MCC.CCheckerDetector, ref: ref}) do
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
  - **self**: `Evision.MCC.CCheckerDetector.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.MCC.CCheckerDetector.t()) :: Evision.MCC.CCheckerDetector.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.mcc_CCheckerDetector_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  create
  ##### Return
  - **retval**: `CCheckerDetector`

  \\brief Returns the implementation of the CCheckerDetector.

  Python prototype (for reference only):
  ```python3
  create() -> retval
  ```
  """
  @spec create() :: Evision.MCC.CCheckerDetector.t() | {:error, String.t()}
  def create() do
    positional = [
    ]
    :evision_nif.mcc_mcc_CCheckerDetector_create_static(positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.MCC.CCheckerDetector.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.MCC.CCheckerDetector.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.mcc_CCheckerDetector_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getBestColorChecker

  ##### Positional Arguments
  - **self**: `Evision.MCC.CCheckerDetector.t()`

  ##### Return
  - **retval**: `Evision.MCC.CCheckerDetector.t()`

  \\brief Get the best color checker. By the best it means the one
           detected with the highest confidence.
   \\return checker A single colorchecker, if atleast one colorchecker
                   was detected, 'nullptr' otherwise.

  Python prototype (for reference only):
  ```python3
  getBestColorChecker() -> retval
  ```
  """
  @spec getBestColorChecker(Evision.MCC.CCheckerDetector.t()) :: Evision.MCC.CCheckerDetector.t() | {:error, String.t()}
  def getBestColorChecker(self) do
    positional = [
    ]
    :evision_nif.mcc_mcc_CCheckerDetector_getBestColorChecker(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.MCC.CCheckerDetector.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.MCC.CCheckerDetector.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.mcc_CCheckerDetector_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getListColorChecker

  ##### Positional Arguments
  - **self**: `Evision.MCC.CCheckerDetector.t()`

  ##### Return
  - **retval**: `[CChecker]`

  \\brief Get the list of all detected colorcheckers
   \\return checkers vector of colorcheckers

  Python prototype (for reference only):
  ```python3
  getListColorChecker() -> retval
  ```
  """
  @spec getListColorChecker(Evision.MCC.CCheckerDetector.t()) :: list(Evision.MCC.CChecker.t()) | {:error, String.t()}
  def getListColorChecker(self) do
    positional = [
    ]
    :evision_nif.mcc_mcc_CCheckerDetector_getListColorChecker(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  process

  ##### Positional Arguments
  - **self**: `Evision.MCC.CCheckerDetector.t()`
  - **image**: `Evision.Mat`
  - **chartType**: `TYPECHART`

  ##### Keyword Arguments
  - **nc**: `integer()`.
  - **useNet**: `bool`.
  - **params**: `DetectorParameters`.

  ##### Return
  - **retval**: `bool`

  \\brief Find the ColorCharts in the given image.
   Differs from the above one only in the arguments.
   This version searches for the chart in the full image.
   The found charts are not returned but instead stored in the
   detector, these can be accessed later on using getBestColorChecker()
   and getListColorChecker()
   \\param image image in color space BGR
   \\param chartType type of the chart to detect
   \\param nc number of charts in the image, if you don't know the exact
             then keeping this number high helps.
   \\param useNet if it is true the network provided using the setNet()
                 is used for preliminary search for regions where chart
                 could be present, inside the regionsOfInterest provied.
   \\param params parameters of the detection system. More information
                 about them can be found in the struct DetectorParameters.
   \\return true if atleast one chart is detected otherwise false

  Python prototype (for reference only):
  ```python3
  process(image, chartType[, nc[, useNet[, params]]]) -> retval
  ```
  """
  @spec process(Evision.MCC.CCheckerDetector.t(), Evision.Mat.maybe_mat_in(), Evision.MCC.TYPECHART.enum(), [{:nc, term()} | {:params, term()} | {:useNet, term()}] | nil) :: boolean() | {:error, String.t()}
  def process(self, image, chartType, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and is_integer(chartType) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:nc, :params, :useNet])
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      chartType: Evision.Internal.Structurise.from_struct(chartType)
    ]
    :evision_nif.mcc_mcc_CCheckerDetector_process(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  process

  ##### Positional Arguments
  - **self**: `Evision.MCC.CCheckerDetector.t()`
  - **image**: `Evision.Mat`
  - **chartType**: `TYPECHART`

  ##### Keyword Arguments
  - **nc**: `integer()`.
  - **useNet**: `bool`.
  - **params**: `DetectorParameters`.

  ##### Return
  - **retval**: `bool`

  \\brief Find the ColorCharts in the given image.
   Differs from the above one only in the arguments.
   This version searches for the chart in the full image.
   The found charts are not returned but instead stored in the
   detector, these can be accessed later on using getBestColorChecker()
   and getListColorChecker()
   \\param image image in color space BGR
   \\param chartType type of the chart to detect
   \\param nc number of charts in the image, if you don't know the exact
             then keeping this number high helps.
   \\param useNet if it is true the network provided using the setNet()
                 is used for preliminary search for regions where chart
                 could be present, inside the regionsOfInterest provied.
   \\param params parameters of the detection system. More information
                 about them can be found in the struct DetectorParameters.
   \\return true if atleast one chart is detected otherwise false

  Python prototype (for reference only):
  ```python3
  process(image, chartType[, nc[, useNet[, params]]]) -> retval
  ```
  """
  @spec process(Evision.MCC.CCheckerDetector.t(), Evision.Mat.maybe_mat_in(), Evision.MCC.TYPECHART.enum()) :: boolean() | {:error, String.t()}
  def process(self, image, chartType) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and is_integer(chartType)
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      chartType: Evision.Internal.Structurise.from_struct(chartType)
    ]
    :evision_nif.mcc_mcc_CCheckerDetector_process(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  processWithROI

  ##### Positional Arguments
  - **self**: `Evision.MCC.CCheckerDetector.t()`
  - **image**: `Evision.Mat`
  - **chartType**: `TYPECHART`
  - **regionsOfInterest**: `[Rect]`

  ##### Keyword Arguments
  - **nc**: `integer()`.
  - **useNet**: `bool`.
  - **params**: `DetectorParameters`.

  ##### Return
  - **retval**: `bool`

  \\brief Find the ColorCharts in the given image.
   The found charts are not returned but instead stored in the
   detector, these can be accessed later on using getBestColorChecker()
   and getListColorChecker()
   \\param image image in color space BGR
   \\param chartType type of the chart to detect
   \\param regionsOfInterest regions of image to look for the chart, if
                            it is empty, charts are looked for in the
                            entire image
   \\param nc number of charts in the image, if you don't know the exact
             then keeping this number high helps.
   \\param useNet if it is true the network provided using the setNet()
                 is used for preliminary search for regions where chart
                 could be present, inside the regionsOfInterest provied.
   \\param params parameters of the detection system. More information
                 about them can be found in the struct DetectorParameters.
   \\return true if atleast one chart is detected otherwise false

  Python prototype (for reference only):
  ```python3
  processWithROI(image, chartType, regionsOfInterest[, nc[, useNet[, params]]]) -> retval
  ```
  """
  @spec processWithROI(Evision.MCC.CCheckerDetector.t(), Evision.Mat.maybe_mat_in(), Evision.MCC.TYPECHART.enum(), list({number(), number(), number(), number()}), [{:nc, term()} | {:params, term()} | {:useNet, term()}] | nil) :: boolean() | {:error, String.t()}
  def processWithROI(self, image, chartType, regionsOfInterest, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and is_integer(chartType) and is_list(regionsOfInterest) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:nc, :params, :useNet])
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      chartType: Evision.Internal.Structurise.from_struct(chartType),
      regionsOfInterest: Evision.Internal.Structurise.from_struct(regionsOfInterest)
    ]
    :evision_nif.mcc_mcc_CCheckerDetector_processWithROI(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  processWithROI

  ##### Positional Arguments
  - **self**: `Evision.MCC.CCheckerDetector.t()`
  - **image**: `Evision.Mat`
  - **chartType**: `TYPECHART`
  - **regionsOfInterest**: `[Rect]`

  ##### Keyword Arguments
  - **nc**: `integer()`.
  - **useNet**: `bool`.
  - **params**: `DetectorParameters`.

  ##### Return
  - **retval**: `bool`

  \\brief Find the ColorCharts in the given image.
   The found charts are not returned but instead stored in the
   detector, these can be accessed later on using getBestColorChecker()
   and getListColorChecker()
   \\param image image in color space BGR
   \\param chartType type of the chart to detect
   \\param regionsOfInterest regions of image to look for the chart, if
                            it is empty, charts are looked for in the
                            entire image
   \\param nc number of charts in the image, if you don't know the exact
             then keeping this number high helps.
   \\param useNet if it is true the network provided using the setNet()
                 is used for preliminary search for regions where chart
                 could be present, inside the regionsOfInterest provied.
   \\param params parameters of the detection system. More information
                 about them can be found in the struct DetectorParameters.
   \\return true if atleast one chart is detected otherwise false

  Python prototype (for reference only):
  ```python3
  processWithROI(image, chartType, regionsOfInterest[, nc[, useNet[, params]]]) -> retval
  ```
  """
  @spec processWithROI(Evision.MCC.CCheckerDetector.t(), Evision.Mat.maybe_mat_in(), Evision.MCC.TYPECHART.enum(), list({number(), number(), number(), number()})) :: boolean() | {:error, String.t()}
  def processWithROI(self, image, chartType, regionsOfInterest) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and is_integer(chartType) and is_list(regionsOfInterest)
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      chartType: Evision.Internal.Structurise.from_struct(chartType),
      regionsOfInterest: Evision.Internal.Structurise.from_struct(regionsOfInterest)
    ]
    :evision_nif.mcc_mcc_CCheckerDetector_processWithROI(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.MCC.CCheckerDetector.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.MCC.CCheckerDetector.t(), Evision.FileNode.t()) :: Evision.MCC.CCheckerDetector.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.mcc_CCheckerDetector_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.MCC.CCheckerDetector.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.MCC.CCheckerDetector.t(), binary()) :: Evision.MCC.CCheckerDetector.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.mcc_CCheckerDetector_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setNet

  ##### Positional Arguments
  - **self**: `Evision.MCC.CCheckerDetector.t()`
  - **net**: `Evision.DNN.Net.t()`

  ##### Return
  - **retval**: `bool`

  \\brief Set the net which will be used to find the approximate
           bounding boxes for the color charts.
   It is not necessary to use this, but this usually results in
   better detection rate.
   \\param net the neural network, if the network in empty, then
              the function will return false.
   \\return true if it was able to set the detector's network,
           false otherwise.

  Python prototype (for reference only):
  ```python3
  setNet(net) -> retval
  ```
  """
  @spec setNet(Evision.MCC.CCheckerDetector.t(), Evision.DNN.Net.t()) :: boolean() | {:error, String.t()}
  def setNet(self, net) when is_struct(net, Evision.DNN.Net)
  do
    positional = [
      net: Evision.Internal.Structurise.from_struct(net)
    ]
    :evision_nif.mcc_mcc_CCheckerDetector_setNet(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.MCC.CCheckerDetector.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.MCC.CCheckerDetector.t(), Evision.FileStorage.t(), binary()) :: Evision.MCC.CCheckerDetector.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.mcc_CCheckerDetector_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.MCC.CCheckerDetector.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.MCC.CCheckerDetector.t(), Evision.FileStorage.t()) :: Evision.MCC.CCheckerDetector.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.mcc_CCheckerDetector_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
