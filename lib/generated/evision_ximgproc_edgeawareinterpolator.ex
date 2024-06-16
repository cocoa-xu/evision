defmodule Evision.XImgProc.EdgeAwareInterpolator do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `XImgProc.EdgeAwareInterpolator` struct.

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
  def to_struct({:ok, %{class: Evision.XImgProc.EdgeAwareInterpolator, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.XImgProc.EdgeAwareInterpolator, ref: ref}) do
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
  getFGSLambda

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.EdgeAwareInterpolator.t()`

  ##### Return
  - **retval**: `float`

  @see `setFGSLambda/2`

  Python prototype (for reference only):
  ```python3
  getFGSLambda() -> retval
  ```
  """
  @spec getFGSLambda(Evision.XImgProc.EdgeAwareInterpolator.t()) :: number() | {:error, String.t()}
  def getFGSLambda(self) do
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_EdgeAwareInterpolator_getFGSLambda(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getFGSSigma

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.EdgeAwareInterpolator.t()`

  ##### Return
  - **retval**: `float`

  @see `setFGSLambda/2`

  Python prototype (for reference only):
  ```python3
  getFGSSigma() -> retval
  ```
  """
  @spec getFGSSigma(Evision.XImgProc.EdgeAwareInterpolator.t()) :: number() | {:error, String.t()}
  def getFGSSigma(self) do
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_EdgeAwareInterpolator_getFGSSigma(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getK

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.EdgeAwareInterpolator.t()`

  ##### Return
  - **retval**: `integer()`

  @see `setK/2`

  Python prototype (for reference only):
  ```python3
  getK() -> retval
  ```
  """
  @spec getK(Evision.XImgProc.EdgeAwareInterpolator.t()) :: integer() | {:error, String.t()}
  def getK(self) do
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_EdgeAwareInterpolator_getK(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getLambda

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.EdgeAwareInterpolator.t()`

  ##### Return
  - **retval**: `float`

  @see `setLambda/2`

  Python prototype (for reference only):
  ```python3
  getLambda() -> retval
  ```
  """
  @spec getLambda(Evision.XImgProc.EdgeAwareInterpolator.t()) :: number() | {:error, String.t()}
  def getLambda(self) do
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_EdgeAwareInterpolator_getLambda(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getSigma

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.EdgeAwareInterpolator.t()`

  ##### Return
  - **retval**: `float`

  @see `setSigma/2`

  Python prototype (for reference only):
  ```python3
  getSigma() -> retval
  ```
  """
  @spec getSigma(Evision.XImgProc.EdgeAwareInterpolator.t()) :: number() | {:error, String.t()}
  def getSigma(self) do
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_EdgeAwareInterpolator_getSigma(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getUsePostProcessing

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.EdgeAwareInterpolator.t()`

  ##### Return
  - **retval**: `bool`

  @see `setUsePostProcessing/2`

  Python prototype (for reference only):
  ```python3
  getUsePostProcessing() -> retval
  ```
  """
  @spec getUsePostProcessing(Evision.XImgProc.EdgeAwareInterpolator.t()) :: boolean() | {:error, String.t()}
  def getUsePostProcessing(self) do
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_EdgeAwareInterpolator_getUsePostProcessing(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Interface to provide a more elaborated cost map, i.e. edge map, for the edge-aware term.
  This implementation is based on a rather simple gradient-based edge map estimation.
  To used more complex edge map estimator (e.g. StructuredEdgeDetection that has been
  used in the original publication) that may lead to improved accuracies, the internal
  edge map estimation can be bypassed here.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.EdgeAwareInterpolator.t()`
  - **costMap**: `Evision.Mat`.

    a type CV_32FC1 Mat is required.

  @see cv::ximgproc::createSuperpixelSLIC

  Python prototype (for reference only):
  ```python3
  setCostMap(_costMap) -> None
  ```
  """
  @spec setCostMap(Evision.XImgProc.EdgeAwareInterpolator.t(), Evision.Mat.maybe_mat_in()) :: Evision.XImgProc.EdgeAwareInterpolator.t() | {:error, String.t()}
  def setCostMap(self, costMap) when (is_struct(costMap, Evision.Mat) or is_struct(costMap, Nx.Tensor) or is_number(costMap) or is_tuple(costMap))
  do
    positional = [
      costMap: Evision.Internal.Structurise.from_struct(costMap)
    ]
    :evision_nif.ximgproc_ximgproc_EdgeAwareInterpolator_setCostMap(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Sets the respective fastGlobalSmootherFilter() parameter.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.EdgeAwareInterpolator.t()`
  - **lambda**: `float`

  Python prototype (for reference only):
  ```python3
  setFGSLambda(_lambda) -> None
  ```
  """
  @spec setFGSLambda(Evision.XImgProc.EdgeAwareInterpolator.t(), number()) :: Evision.XImgProc.EdgeAwareInterpolator.t() | {:error, String.t()}
  def setFGSLambda(self, lambda) when is_float(lambda)
  do
    positional = [
      lambda: Evision.Internal.Structurise.from_struct(lambda)
    ]
    :evision_nif.ximgproc_ximgproc_EdgeAwareInterpolator_setFGSLambda(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setFGSSigma

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.EdgeAwareInterpolator.t()`
  - **sigma**: `float`

  @see `setFGSLambda/2`

  Python prototype (for reference only):
  ```python3
  setFGSSigma(_sigma) -> None
  ```
  """
  @spec setFGSSigma(Evision.XImgProc.EdgeAwareInterpolator.t(), number()) :: Evision.XImgProc.EdgeAwareInterpolator.t() | {:error, String.t()}
  def setFGSSigma(self, sigma) when is_float(sigma)
  do
    positional = [
      sigma: Evision.Internal.Structurise.from_struct(sigma)
    ]
    :evision_nif.ximgproc_ximgproc_EdgeAwareInterpolator_setFGSSigma(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  K is a number of nearest-neighbor matches considered, when fitting a locally affine
  model. Usually it should be around 128. However, lower values would make the interpolation
  noticeably faster.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.EdgeAwareInterpolator.t()`
  - **k**: `integer()`

  Python prototype (for reference only):
  ```python3
  setK(_k) -> None
  ```
  """
  @spec setK(Evision.XImgProc.EdgeAwareInterpolator.t(), integer()) :: Evision.XImgProc.EdgeAwareInterpolator.t() | {:error, String.t()}
  def setK(self, k) when is_integer(k)
  do
    positional = [
      k: Evision.Internal.Structurise.from_struct(k)
    ]
    :evision_nif.ximgproc_ximgproc_EdgeAwareInterpolator_setK(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Lambda is a parameter defining the weight of the edge-aware term in geodesic distance,
  should be in the range of 0 to 1000.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.EdgeAwareInterpolator.t()`
  - **lambda**: `float`

  Python prototype (for reference only):
  ```python3
  setLambda(_lambda) -> None
  ```
  """
  @spec setLambda(Evision.XImgProc.EdgeAwareInterpolator.t(), number()) :: Evision.XImgProc.EdgeAwareInterpolator.t() | {:error, String.t()}
  def setLambda(self, lambda) when is_float(lambda)
  do
    positional = [
      lambda: Evision.Internal.Structurise.from_struct(lambda)
    ]
    :evision_nif.ximgproc_ximgproc_EdgeAwareInterpolator_setLambda(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Sigma is a parameter defining how fast the weights decrease in the locally-weighted affine
  fitting. Higher values can help preserve fine details, lower values can help to get rid of noise in the
  output flow.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.EdgeAwareInterpolator.t()`
  - **sigma**: `float`

  Python prototype (for reference only):
  ```python3
  setSigma(_sigma) -> None
  ```
  """
  @spec setSigma(Evision.XImgProc.EdgeAwareInterpolator.t(), number()) :: Evision.XImgProc.EdgeAwareInterpolator.t() | {:error, String.t()}
  def setSigma(self, sigma) when is_float(sigma)
  do
    positional = [
      sigma: Evision.Internal.Structurise.from_struct(sigma)
    ]
    :evision_nif.ximgproc_ximgproc_EdgeAwareInterpolator_setSigma(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Sets whether the fastGlobalSmootherFilter() post-processing is employed. It is turned on by
  default.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.EdgeAwareInterpolator.t()`
  - **use_post_proc**: `bool`

  Python prototype (for reference only):
  ```python3
  setUsePostProcessing(_use_post_proc) -> None
  ```
  """
  @spec setUsePostProcessing(Evision.XImgProc.EdgeAwareInterpolator.t(), boolean()) :: Evision.XImgProc.EdgeAwareInterpolator.t() | {:error, String.t()}
  def setUsePostProcessing(self, use_post_proc) when is_boolean(use_post_proc)
  do
    positional = [
      use_post_proc: Evision.Internal.Structurise.from_struct(use_post_proc)
    ]
    :evision_nif.ximgproc_ximgproc_EdgeAwareInterpolator_setUsePostProcessing(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
