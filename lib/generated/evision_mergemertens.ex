defmodule Evision.MergeMertens do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `MergeMertens` struct.

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
  def to_struct({:ok, %{class: Evision.MergeMertens, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.MergeMertens, ref: ref}) do
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
  getContrastWeight

  ##### Positional Arguments
  - **self**: `Evision.MergeMertens.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  getContrastWeight() -> retval
  ```
  """
  @spec getContrastWeight(Evision.MergeMertens.t()) :: number() | {:error, String.t()}
  def getContrastWeight(self) do
    positional = [
    ]
    :evision_nif.mergeMertens_getContrastWeight(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getExposureWeight

  ##### Positional Arguments
  - **self**: `Evision.MergeMertens.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  getExposureWeight() -> retval
  ```
  """
  @spec getExposureWeight(Evision.MergeMertens.t()) :: number() | {:error, String.t()}
  def getExposureWeight(self) do
    positional = [
    ]
    :evision_nif.mergeMertens_getExposureWeight(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getSaturationWeight

  ##### Positional Arguments
  - **self**: `Evision.MergeMertens.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  getSaturationWeight() -> retval
  ```
  """
  @spec getSaturationWeight(Evision.MergeMertens.t()) :: number() | {:error, String.t()}
  def getSaturationWeight(self) do
    positional = [
    ]
    :evision_nif.mergeMertens_getSaturationWeight(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  process

  ##### Positional Arguments
  - **self**: `Evision.MergeMertens.t()`
  - **src**: `[Evision.Mat]`
  - **times**: `Evision.Mat`
  - **response**: `Evision.Mat`

  ##### Return
  - **dst**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  process(src, times, response[, dst]) -> dst
  ```
  """
  @spec process(Evision.MergeMertens.t(), list(Evision.Mat.maybe_mat_in()), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def process(self, src, times, response, opts) when is_list(src) and (is_struct(times, Evision.Mat) or is_struct(times, Nx.Tensor) or is_number(times) or is_tuple(times)) and (is_struct(response, Evision.Mat) or is_struct(response, Nx.Tensor) or is_number(response) or is_tuple(response)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      times: Evision.Internal.Structurise.from_struct(times),
      response: Evision.Internal.Structurise.from_struct(response)
    ]
    :evision_nif.mergeMertens_process(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  process

  ##### Positional Arguments
  - **self**: `Evision.MergeMertens.t()`
  - **src**: `[Evision.Mat]`
  - **times**: `Evision.Mat`
  - **response**: `Evision.Mat`

  ##### Return
  - **dst**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  process(src, times, response[, dst]) -> dst
  ```
  """
  @spec process(Evision.MergeMertens.t(), list(Evision.Mat.maybe_mat_in()), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def process(self, src, times, response) when is_list(src) and (is_struct(times, Evision.Mat) or is_struct(times, Nx.Tensor) or is_number(times) or is_tuple(times)) and (is_struct(response, Evision.Mat) or is_struct(response, Nx.Tensor) or is_number(response) or is_tuple(response))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      times: Evision.Internal.Structurise.from_struct(times),
      response: Evision.Internal.Structurise.from_struct(response)
    ]
    :evision_nif.mergeMertens_process(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Short version of process, that doesn't take extra arguments.

  ##### Positional Arguments
  - **self**: `Evision.MergeMertens.t()`
  - **src**: `[Evision.Mat]`.

    vector of input images

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    result image

  Python prototype (for reference only):
  ```python3
  process(src[, dst]) -> dst
  ```
  """
  @spec process(Evision.MergeMertens.t(), list(Evision.Mat.maybe_mat_in()), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def process(self, src, opts) when is_list(src) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.mergeMertens_process(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Short version of process, that doesn't take extra arguments.

  ##### Positional Arguments
  - **self**: `Evision.MergeMertens.t()`
  - **src**: `[Evision.Mat]`.

    vector of input images

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    result image

  Python prototype (for reference only):
  ```python3
  process(src[, dst]) -> dst
  ```
  """
  @spec process(Evision.MergeMertens.t(), list(Evision.Mat.maybe_mat_in())) :: Evision.Mat.t() | {:error, String.t()}
  def process(self, src) when is_list(src)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.mergeMertens_process(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setContrastWeight

  ##### Positional Arguments
  - **self**: `Evision.MergeMertens.t()`
  - **contrast_weiht**: `float`

  Python prototype (for reference only):
  ```python3
  setContrastWeight(contrast_weiht) -> None
  ```
  """
  @spec setContrastWeight(Evision.MergeMertens.t(), number()) :: Evision.MergeMertens.t() | {:error, String.t()}
  def setContrastWeight(self, contrast_weiht) when is_float(contrast_weiht)
  do
    positional = [
      contrast_weiht: Evision.Internal.Structurise.from_struct(contrast_weiht)
    ]
    :evision_nif.mergeMertens_setContrastWeight(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setExposureWeight

  ##### Positional Arguments
  - **self**: `Evision.MergeMertens.t()`
  - **exposure_weight**: `float`

  Python prototype (for reference only):
  ```python3
  setExposureWeight(exposure_weight) -> None
  ```
  """
  @spec setExposureWeight(Evision.MergeMertens.t(), number()) :: Evision.MergeMertens.t() | {:error, String.t()}
  def setExposureWeight(self, exposure_weight) when is_float(exposure_weight)
  do
    positional = [
      exposure_weight: Evision.Internal.Structurise.from_struct(exposure_weight)
    ]
    :evision_nif.mergeMertens_setExposureWeight(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setSaturationWeight

  ##### Positional Arguments
  - **self**: `Evision.MergeMertens.t()`
  - **saturation_weight**: `float`

  Python prototype (for reference only):
  ```python3
  setSaturationWeight(saturation_weight) -> None
  ```
  """
  @spec setSaturationWeight(Evision.MergeMertens.t(), number()) :: Evision.MergeMertens.t() | {:error, String.t()}
  def setSaturationWeight(self, saturation_weight) when is_float(saturation_weight)
  do
    positional = [
      saturation_weight: Evision.Internal.Structurise.from_struct(saturation_weight)
    ]
    :evision_nif.mergeMertens_setSaturationWeight(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
