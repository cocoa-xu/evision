defmodule Evision.XImgProc.RICInterpolator do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `XImgProc.RICInterpolator` struct.

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
  def to_struct({:ok, %{class: Evision.XImgProc.RICInterpolator, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.XImgProc.RICInterpolator, ref: ref}) do
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
  getAlpha

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.RICInterpolator.t()`

  ##### Return
  - **retval**: `float`

  @copybrief setAlpha
  @see `setAlpha/2`

  Python prototype (for reference only):
  ```python3
  getAlpha() -> retval
  ```
  """
  @spec getAlpha(Evision.XImgProc.RICInterpolator.t()) :: number() | {:error, String.t()}
  def getAlpha(self) do
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_RICInterpolator_getAlpha(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getFGSLambda

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.RICInterpolator.t()`

  ##### Return
  - **retval**: `float`

  @copybrief setFGSLambda
  @see `setFGSLambda/2`

  Python prototype (for reference only):
  ```python3
  getFGSLambda() -> retval
  ```
  """
  @spec getFGSLambda(Evision.XImgProc.RICInterpolator.t()) :: number() | {:error, String.t()}
  def getFGSLambda(self) do
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_RICInterpolator_getFGSLambda(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getFGSSigma

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.RICInterpolator.t()`

  ##### Return
  - **retval**: `float`

  @copybrief setFGSSigma
  @see `setFGSSigma/2`

  Python prototype (for reference only):
  ```python3
  getFGSSigma() -> retval
  ```
  """
  @spec getFGSSigma(Evision.XImgProc.RICInterpolator.t()) :: number() | {:error, String.t()}
  def getFGSSigma(self) do
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_RICInterpolator_getFGSSigma(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getK

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.RICInterpolator.t()`

  ##### Return
  - **retval**: `integer()`

  @copybrief setK
  @see `setK/2`

  Python prototype (for reference only):
  ```python3
  getK() -> retval
  ```
  """
  @spec getK(Evision.XImgProc.RICInterpolator.t()) :: integer() | {:error, String.t()}
  def getK(self) do
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_RICInterpolator_getK(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMaxFlow

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.RICInterpolator.t()`

  ##### Return
  - **retval**: `float`

  @copybrief setMaxFlow
  @see `setMaxFlow/2`

  Python prototype (for reference only):
  ```python3
  getMaxFlow() -> retval
  ```
  """
  @spec getMaxFlow(Evision.XImgProc.RICInterpolator.t()) :: number() | {:error, String.t()}
  def getMaxFlow(self) do
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_RICInterpolator_getMaxFlow(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getModelIter

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.RICInterpolator.t()`

  ##### Return
  - **retval**: `integer()`

  @copybrief setModelIter
  @see `setModelIter/2`

  Python prototype (for reference only):
  ```python3
  getModelIter() -> retval
  ```
  """
  @spec getModelIter(Evision.XImgProc.RICInterpolator.t()) :: integer() | {:error, String.t()}
  def getModelIter(self) do
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_RICInterpolator_getModelIter(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getRefineModels

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.RICInterpolator.t()`

  ##### Return
  - **retval**: `bool`

  @copybrief setRefineModels
  @see `setRefineModels/2`

  Python prototype (for reference only):
  ```python3
  getRefineModels() -> retval
  ```
  """
  @spec getRefineModels(Evision.XImgProc.RICInterpolator.t()) :: boolean() | {:error, String.t()}
  def getRefineModels(self) do
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_RICInterpolator_getRefineModels(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getSuperpixelMode

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.RICInterpolator.t()`

  ##### Return
  - **retval**: `integer()`

  @copybrief setSuperpixelMode
  @see `setSuperpixelMode/2`

  Python prototype (for reference only):
  ```python3
  getSuperpixelMode() -> retval
  ```
  """
  @spec getSuperpixelMode(Evision.XImgProc.RICInterpolator.t()) :: integer() | {:error, String.t()}
  def getSuperpixelMode(self) do
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_RICInterpolator_getSuperpixelMode(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getSuperpixelNNCnt

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.RICInterpolator.t()`

  ##### Return
  - **retval**: `integer()`

  @copybrief setSuperpixelNNCnt
  @see `setSuperpixelNNCnt/2`

  Python prototype (for reference only):
  ```python3
  getSuperpixelNNCnt() -> retval
  ```
  """
  @spec getSuperpixelNNCnt(Evision.XImgProc.RICInterpolator.t()) :: integer() | {:error, String.t()}
  def getSuperpixelNNCnt(self) do
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_RICInterpolator_getSuperpixelNNCnt(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getSuperpixelRuler

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.RICInterpolator.t()`

  ##### Return
  - **retval**: `float`

  @copybrief setSuperpixelRuler
  @see `setSuperpixelRuler/2`

  Python prototype (for reference only):
  ```python3
  getSuperpixelRuler() -> retval
  ```
  """
  @spec getSuperpixelRuler(Evision.XImgProc.RICInterpolator.t()) :: number() | {:error, String.t()}
  def getSuperpixelRuler(self) do
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_RICInterpolator_getSuperpixelRuler(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getSuperpixelSize

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.RICInterpolator.t()`

  ##### Return
  - **retval**: `integer()`

  @copybrief setSuperpixelSize
  @see `setSuperpixelSize/2`

  Python prototype (for reference only):
  ```python3
  getSuperpixelSize() -> retval
  ```
  """
  @spec getSuperpixelSize(Evision.XImgProc.RICInterpolator.t()) :: integer() | {:error, String.t()}
  def getSuperpixelSize(self) do
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_RICInterpolator_getSuperpixelSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getUseGlobalSmootherFilter

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.RICInterpolator.t()`

  ##### Return
  - **retval**: `bool`

  @copybrief setUseGlobalSmootherFilter
  @see `setUseGlobalSmootherFilter/2`

  Python prototype (for reference only):
  ```python3
  getUseGlobalSmootherFilter() -> retval
  ```
  """
  @spec getUseGlobalSmootherFilter(Evision.XImgProc.RICInterpolator.t()) :: boolean() | {:error, String.t()}
  def getUseGlobalSmootherFilter(self) do
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_RICInterpolator_getUseGlobalSmootherFilter(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getUseVariationalRefinement

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.RICInterpolator.t()`

  ##### Return
  - **retval**: `bool`

  @copybrief setUseVariationalRefinement
  @see `setUseVariationalRefinement/2`

  Python prototype (for reference only):
  ```python3
  getUseVariationalRefinement() -> retval
  ```
  """
  @spec getUseVariationalRefinement(Evision.XImgProc.RICInterpolator.t()) :: boolean() | {:error, String.t()}
  def getUseVariationalRefinement(self) do
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_RICInterpolator_getUseVariationalRefinement(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Alpha is a parameter defining a global weight for transforming geodesic distance into weight.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.RICInterpolator.t()`

  ##### Keyword Arguments
  - **alpha**: `float`.

  Python prototype (for reference only):
  ```python3
  setAlpha([, alpha]) -> None
  ```
  """
  @spec setAlpha(Evision.XImgProc.RICInterpolator.t(), [{:alpha, term()}] | nil) :: Evision.XImgProc.RICInterpolator.t() | {:error, String.t()}
  def setAlpha(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:alpha])
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_RICInterpolator_setAlpha(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Alpha is a parameter defining a global weight for transforming geodesic distance into weight.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.RICInterpolator.t()`

  ##### Keyword Arguments
  - **alpha**: `float`.

  Python prototype (for reference only):
  ```python3
  setAlpha([, alpha]) -> None
  ```
  """
  @spec setAlpha(Evision.XImgProc.RICInterpolator.t()) :: Evision.XImgProc.RICInterpolator.t() | {:error, String.t()}
  def setAlpha(self) do
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_RICInterpolator_setAlpha(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Interface to provide a more elaborated cost map, i.e. edge map, for the edge-aware term.
  This implementation is based on a rather simple gradient-based edge map estimation.
  To used more complex edge map estimator (e.g. StructuredEdgeDetection that has been
  used in the original publication) that may lead to improved accuracies, the internal
  edge map estimation can be bypassed here.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.RICInterpolator.t()`
  - **costMap**: `Evision.Mat`.

    a type CV_32FC1 Mat is required.

  @see cv::ximgproc::createSuperpixelSLIC

  Python prototype (for reference only):
  ```python3
  setCostMap(costMap) -> None
  ```
  """
  @spec setCostMap(Evision.XImgProc.RICInterpolator.t(), Evision.Mat.maybe_mat_in()) :: Evision.XImgProc.RICInterpolator.t() | {:error, String.t()}
  def setCostMap(self, costMap) when (is_struct(costMap, Evision.Mat) or is_struct(costMap, Nx.Tensor) or is_number(costMap) or is_tuple(costMap))
  do
    positional = [
      costMap: Evision.Internal.Structurise.from_struct(costMap)
    ]
    :evision_nif.ximgproc_ximgproc_RICInterpolator_setCostMap(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Sets the respective fastGlobalSmootherFilter() parameter.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.RICInterpolator.t()`

  ##### Keyword Arguments
  - **lambda**: `float`.

  Python prototype (for reference only):
  ```python3
  setFGSLambda([, lambda]) -> None
  ```
  """
  @spec setFGSLambda(Evision.XImgProc.RICInterpolator.t(), [{:lambda, term()}] | nil) :: Evision.XImgProc.RICInterpolator.t() | {:error, String.t()}
  def setFGSLambda(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:lambda])
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_RICInterpolator_setFGSLambda(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Sets the respective fastGlobalSmootherFilter() parameter.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.RICInterpolator.t()`

  ##### Keyword Arguments
  - **lambda**: `float`.

  Python prototype (for reference only):
  ```python3
  setFGSLambda([, lambda]) -> None
  ```
  """
  @spec setFGSLambda(Evision.XImgProc.RICInterpolator.t()) :: Evision.XImgProc.RICInterpolator.t() | {:error, String.t()}
  def setFGSLambda(self) do
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_RICInterpolator_setFGSLambda(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Sets the respective fastGlobalSmootherFilter() parameter.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.RICInterpolator.t()`

  ##### Keyword Arguments
  - **sigma**: `float`.

  Python prototype (for reference only):
  ```python3
  setFGSSigma([, sigma]) -> None
  ```
  """
  @spec setFGSSigma(Evision.XImgProc.RICInterpolator.t(), [{:sigma, term()}] | nil) :: Evision.XImgProc.RICInterpolator.t() | {:error, String.t()}
  def setFGSSigma(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:sigma])
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_RICInterpolator_setFGSSigma(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Sets the respective fastGlobalSmootherFilter() parameter.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.RICInterpolator.t()`

  ##### Keyword Arguments
  - **sigma**: `float`.

  Python prototype (for reference only):
  ```python3
  setFGSSigma([, sigma]) -> None
  ```
  """
  @spec setFGSSigma(Evision.XImgProc.RICInterpolator.t()) :: Evision.XImgProc.RICInterpolator.t() | {:error, String.t()}
  def setFGSSigma(self) do
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_RICInterpolator_setFGSSigma(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  K is a number of nearest-neighbor matches considered, when fitting a locally affine
  model for a superpixel segment. However, lower values would make the interpolation
  noticeably faster. The original implementation of @cite Hu2017 uses 32.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.RICInterpolator.t()`

  ##### Keyword Arguments
  - **k**: `integer()`.

  Python prototype (for reference only):
  ```python3
  setK([, k]) -> None
  ```
  """
  @spec setK(Evision.XImgProc.RICInterpolator.t(), [{:k, term()}] | nil) :: Evision.XImgProc.RICInterpolator.t() | {:error, String.t()}
  def setK(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:k])
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_RICInterpolator_setK(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  K is a number of nearest-neighbor matches considered, when fitting a locally affine
  model for a superpixel segment. However, lower values would make the interpolation
  noticeably faster. The original implementation of @cite Hu2017 uses 32.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.RICInterpolator.t()`

  ##### Keyword Arguments
  - **k**: `integer()`.

  Python prototype (for reference only):
  ```python3
  setK([, k]) -> None
  ```
  """
  @spec setK(Evision.XImgProc.RICInterpolator.t()) :: Evision.XImgProc.RICInterpolator.t() | {:error, String.t()}
  def setK(self) do
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_RICInterpolator_setK(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  MaxFlow is a threshold to validate the predictions using a certain piece-wise affine model.
  If the prediction exceeds the treshold the translational model will be applied instead.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.RICInterpolator.t()`

  ##### Keyword Arguments
  - **maxFlow**: `float`.

  Python prototype (for reference only):
  ```python3
  setMaxFlow([, maxFlow]) -> None
  ```
  """
  @spec setMaxFlow(Evision.XImgProc.RICInterpolator.t(), [{:maxFlow, term()}] | nil) :: Evision.XImgProc.RICInterpolator.t() | {:error, String.t()}
  def setMaxFlow(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:maxFlow])
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_RICInterpolator_setMaxFlow(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  MaxFlow is a threshold to validate the predictions using a certain piece-wise affine model.
  If the prediction exceeds the treshold the translational model will be applied instead.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.RICInterpolator.t()`

  ##### Keyword Arguments
  - **maxFlow**: `float`.

  Python prototype (for reference only):
  ```python3
  setMaxFlow([, maxFlow]) -> None
  ```
  """
  @spec setMaxFlow(Evision.XImgProc.RICInterpolator.t()) :: Evision.XImgProc.RICInterpolator.t() | {:error, String.t()}
  def setMaxFlow(self) do
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_RICInterpolator_setMaxFlow(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Parameter defining the number of iterations for piece-wise affine model estimation.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.RICInterpolator.t()`

  ##### Keyword Arguments
  - **modelIter**: `integer()`.

  Python prototype (for reference only):
  ```python3
  setModelIter([, modelIter]) -> None
  ```
  """
  @spec setModelIter(Evision.XImgProc.RICInterpolator.t(), [{:modelIter, term()}] | nil) :: Evision.XImgProc.RICInterpolator.t() | {:error, String.t()}
  def setModelIter(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:modelIter])
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_RICInterpolator_setModelIter(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Parameter defining the number of iterations for piece-wise affine model estimation.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.RICInterpolator.t()`

  ##### Keyword Arguments
  - **modelIter**: `integer()`.

  Python prototype (for reference only):
  ```python3
  setModelIter([, modelIter]) -> None
  ```
  """
  @spec setModelIter(Evision.XImgProc.RICInterpolator.t()) :: Evision.XImgProc.RICInterpolator.t() | {:error, String.t()}
  def setModelIter(self) do
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_RICInterpolator_setModelIter(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Parameter to choose wether additional refinement of the piece-wise affine models is employed.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.RICInterpolator.t()`

  ##### Keyword Arguments
  - **refineModles**: `bool`.

  Python prototype (for reference only):
  ```python3
  setRefineModels([, refineModles]) -> None
  ```
  """
  @spec setRefineModels(Evision.XImgProc.RICInterpolator.t(), [{:refineModles, term()}] | nil) :: Evision.XImgProc.RICInterpolator.t() | {:error, String.t()}
  def setRefineModels(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:refineModles])
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_RICInterpolator_setRefineModels(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Parameter to choose wether additional refinement of the piece-wise affine models is employed.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.RICInterpolator.t()`

  ##### Keyword Arguments
  - **refineModles**: `bool`.

  Python prototype (for reference only):
  ```python3
  setRefineModels([, refineModles]) -> None
  ```
  """
  @spec setRefineModels(Evision.XImgProc.RICInterpolator.t()) :: Evision.XImgProc.RICInterpolator.t() | {:error, String.t()}
  def setRefineModels(self) do
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_RICInterpolator_setRefineModels(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Parameter to choose superpixel algorithm variant to use:
  - cv::ximgproc::SLICType SLIC segments image using a desired region_size (value: 100)
  - cv::ximgproc::SLICType SLICO will optimize using adaptive compactness factor (value: 101)
  - cv::ximgproc::SLICType MSLIC will optimize using manifold methods resulting in more content-sensitive superpixels (value: 102).

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.RICInterpolator.t()`

  ##### Keyword Arguments
  - **mode**: `integer()`.

  @see cv::ximgproc::createSuperpixelSLIC

  Python prototype (for reference only):
  ```python3
  setSuperpixelMode([, mode]) -> None
  ```
  """
  @spec setSuperpixelMode(Evision.XImgProc.RICInterpolator.t(), [{:mode, term()}] | nil) :: Evision.XImgProc.RICInterpolator.t() | {:error, String.t()}
  def setSuperpixelMode(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:mode])
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_RICInterpolator_setSuperpixelMode(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Parameter to choose superpixel algorithm variant to use:
  - cv::ximgproc::SLICType SLIC segments image using a desired region_size (value: 100)
  - cv::ximgproc::SLICType SLICO will optimize using adaptive compactness factor (value: 101)
  - cv::ximgproc::SLICType MSLIC will optimize using manifold methods resulting in more content-sensitive superpixels (value: 102).

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.RICInterpolator.t()`

  ##### Keyword Arguments
  - **mode**: `integer()`.

  @see cv::ximgproc::createSuperpixelSLIC

  Python prototype (for reference only):
  ```python3
  setSuperpixelMode([, mode]) -> None
  ```
  """
  @spec setSuperpixelMode(Evision.XImgProc.RICInterpolator.t()) :: Evision.XImgProc.RICInterpolator.t() | {:error, String.t()}
  def setSuperpixelMode(self) do
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_RICInterpolator_setSuperpixelMode(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Parameter defines the number of nearest-neighbor matches for each superpixel considered, when fitting a locally affine
  model.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.RICInterpolator.t()`

  ##### Keyword Arguments
  - **spNN**: `integer()`.

  Python prototype (for reference only):
  ```python3
  setSuperpixelNNCnt([, spNN]) -> None
  ```
  """
  @spec setSuperpixelNNCnt(Evision.XImgProc.RICInterpolator.t(), [{:spNN, term()}] | nil) :: Evision.XImgProc.RICInterpolator.t() | {:error, String.t()}
  def setSuperpixelNNCnt(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:spNN])
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_RICInterpolator_setSuperpixelNNCnt(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Parameter defines the number of nearest-neighbor matches for each superpixel considered, when fitting a locally affine
  model.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.RICInterpolator.t()`

  ##### Keyword Arguments
  - **spNN**: `integer()`.

  Python prototype (for reference only):
  ```python3
  setSuperpixelNNCnt([, spNN]) -> None
  ```
  """
  @spec setSuperpixelNNCnt(Evision.XImgProc.RICInterpolator.t()) :: Evision.XImgProc.RICInterpolator.t() | {:error, String.t()}
  def setSuperpixelNNCnt(self) do
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_RICInterpolator_setSuperpixelNNCnt(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Parameter to tune enforcement of superpixel smoothness factor used for oversegmentation.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.RICInterpolator.t()`

  ##### Keyword Arguments
  - **ruler**: `float`.

  @see cv::ximgproc::createSuperpixelSLIC

  Python prototype (for reference only):
  ```python3
  setSuperpixelRuler([, ruler]) -> None
  ```
  """
  @spec setSuperpixelRuler(Evision.XImgProc.RICInterpolator.t(), [{:ruler, term()}] | nil) :: Evision.XImgProc.RICInterpolator.t() | {:error, String.t()}
  def setSuperpixelRuler(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:ruler])
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_RICInterpolator_setSuperpixelRuler(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Parameter to tune enforcement of superpixel smoothness factor used for oversegmentation.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.RICInterpolator.t()`

  ##### Keyword Arguments
  - **ruler**: `float`.

  @see cv::ximgproc::createSuperpixelSLIC

  Python prototype (for reference only):
  ```python3
  setSuperpixelRuler([, ruler]) -> None
  ```
  """
  @spec setSuperpixelRuler(Evision.XImgProc.RICInterpolator.t()) :: Evision.XImgProc.RICInterpolator.t() | {:error, String.t()}
  def setSuperpixelRuler(self) do
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_RICInterpolator_setSuperpixelRuler(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Get the internal cost, i.e. edge map, used for estimating the edge-aware term.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.RICInterpolator.t()`

  ##### Keyword Arguments
  - **spSize**: `integer()`.

  @see `setCostMap/2`

  Python prototype (for reference only):
  ```python3
  setSuperpixelSize([, spSize]) -> None
  ```
  """
  @spec setSuperpixelSize(Evision.XImgProc.RICInterpolator.t(), [{:spSize, term()}] | nil) :: Evision.XImgProc.RICInterpolator.t() | {:error, String.t()}
  def setSuperpixelSize(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:spSize])
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_RICInterpolator_setSuperpixelSize(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Get the internal cost, i.e. edge map, used for estimating the edge-aware term.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.RICInterpolator.t()`

  ##### Keyword Arguments
  - **spSize**: `integer()`.

  @see `setCostMap/2`

  Python prototype (for reference only):
  ```python3
  setSuperpixelSize([, spSize]) -> None
  ```
  """
  @spec setSuperpixelSize(Evision.XImgProc.RICInterpolator.t()) :: Evision.XImgProc.RICInterpolator.t() | {:error, String.t()}
  def setSuperpixelSize(self) do
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_RICInterpolator_setSuperpixelSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Sets whether the fastGlobalSmootherFilter() post-processing is employed.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.RICInterpolator.t()`

  ##### Keyword Arguments
  - **use_FGS**: `bool`.

  Python prototype (for reference only):
  ```python3
  setUseGlobalSmootherFilter([, use_FGS]) -> None
  ```
  """
  @spec setUseGlobalSmootherFilter(Evision.XImgProc.RICInterpolator.t(), [{:use_FGS, term()}] | nil) :: Evision.XImgProc.RICInterpolator.t() | {:error, String.t()}
  def setUseGlobalSmootherFilter(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:use_FGS])
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_RICInterpolator_setUseGlobalSmootherFilter(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Sets whether the fastGlobalSmootherFilter() post-processing is employed.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.RICInterpolator.t()`

  ##### Keyword Arguments
  - **use_FGS**: `bool`.

  Python prototype (for reference only):
  ```python3
  setUseGlobalSmootherFilter([, use_FGS]) -> None
  ```
  """
  @spec setUseGlobalSmootherFilter(Evision.XImgProc.RICInterpolator.t()) :: Evision.XImgProc.RICInterpolator.t() | {:error, String.t()}
  def setUseGlobalSmootherFilter(self) do
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_RICInterpolator_setUseGlobalSmootherFilter(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Parameter to choose wether the VariationalRefinement post-processing  is employed.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.RICInterpolator.t()`

  ##### Keyword Arguments
  - **use_variational_refinement**: `bool`.

  Python prototype (for reference only):
  ```python3
  setUseVariationalRefinement([, use_variational_refinement]) -> None
  ```
  """
  @spec setUseVariationalRefinement(Evision.XImgProc.RICInterpolator.t(), [{:use_variational_refinement, term()}] | nil) :: Evision.XImgProc.RICInterpolator.t() | {:error, String.t()}
  def setUseVariationalRefinement(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:use_variational_refinement])
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_RICInterpolator_setUseVariationalRefinement(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Parameter to choose wether the VariationalRefinement post-processing  is employed.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.RICInterpolator.t()`

  ##### Keyword Arguments
  - **use_variational_refinement**: `bool`.

  Python prototype (for reference only):
  ```python3
  setUseVariationalRefinement([, use_variational_refinement]) -> None
  ```
  """
  @spec setUseVariationalRefinement(Evision.XImgProc.RICInterpolator.t()) :: Evision.XImgProc.RICInterpolator.t() | {:error, String.t()}
  def setUseVariationalRefinement(self) do
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_RICInterpolator_setUseVariationalRefinement(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
