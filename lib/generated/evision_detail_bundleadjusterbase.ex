defmodule Evision.Detail.BundleAdjusterBase do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Detail.BundleAdjusterBase` struct.

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
  def to_struct({:ok, %{class: Evision.Detail.BundleAdjusterBase, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Detail.BundleAdjusterBase, ref: ref}) do
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
  confThresh

  ##### Positional Arguments
  - **self**: `Evision.Detail.BundleAdjusterBase.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  confThresh() -> retval
  ```
  """
  @spec confThresh(Evision.Detail.BundleAdjusterBase.t()) :: number() | {:error, String.t()}
  def confThresh(self) do
    positional = [
    ]
    :evision_nif.detail_detail_BundleAdjusterBase_confThresh(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  refinementMask

  ##### Positional Arguments
  - **self**: `Evision.Detail.BundleAdjusterBase.t()`

  ##### Return
  - **retval**: `Evision.Mat.t()`

  Python prototype (for reference only):
  ```python3
  refinementMask() -> retval
  ```
  """
  @spec refinementMask(Evision.Detail.BundleAdjusterBase.t()) :: Evision.Mat.t() | {:error, String.t()}
  def refinementMask(self) do
    positional = [
    ]
    :evision_nif.detail_detail_BundleAdjusterBase_refinementMask(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setConfThresh

  ##### Positional Arguments
  - **self**: `Evision.Detail.BundleAdjusterBase.t()`
  - **conf_thresh**: `double`

  Python prototype (for reference only):
  ```python3
  setConfThresh(conf_thresh) -> None
  ```
  """
  @spec setConfThresh(Evision.Detail.BundleAdjusterBase.t(), number()) :: Evision.Detail.BundleAdjusterBase.t() | {:error, String.t()}
  def setConfThresh(self, conf_thresh) when is_number(conf_thresh)
  do
    positional = [
      conf_thresh: Evision.Internal.Structurise.from_struct(conf_thresh)
    ]
    :evision_nif.detail_detail_BundleAdjusterBase_setConfThresh(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setRefinementMask

  ##### Positional Arguments
  - **self**: `Evision.Detail.BundleAdjusterBase.t()`
  - **mask**: `Evision.Mat`

  Python prototype (for reference only):
  ```python3
  setRefinementMask(mask) -> None
  ```
  """
  @spec setRefinementMask(Evision.Detail.BundleAdjusterBase.t(), Evision.Mat.maybe_mat_in()) :: Evision.Detail.BundleAdjusterBase.t() | {:error, String.t()}
  def setRefinementMask(self, mask) when (is_struct(mask, Evision.Mat) or is_struct(mask, Nx.Tensor) or is_number(mask) or is_tuple(mask))
  do
    positional = [
      mask: Evision.Internal.Structurise.from_struct(mask)
    ]
    :evision_nif.detail_detail_BundleAdjusterBase_setRefinementMask(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setTermCriteria

  ##### Positional Arguments
  - **self**: `Evision.Detail.BundleAdjusterBase.t()`
  - **term_criteria**: `TermCriteria`

  Python prototype (for reference only):
  ```python3
  setTermCriteria(term_criteria) -> None
  ```
  """
  @spec setTermCriteria(Evision.Detail.BundleAdjusterBase.t(), {integer(), integer(), number()}) :: Evision.Detail.BundleAdjusterBase.t() | {:error, String.t()}
  def setTermCriteria(self, term_criteria) when is_tuple(term_criteria)
  do
    positional = [
      term_criteria: Evision.Internal.Structurise.from_struct(term_criteria)
    ]
    :evision_nif.detail_detail_BundleAdjusterBase_setTermCriteria(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  termCriteria

  ##### Positional Arguments
  - **self**: `Evision.Detail.BundleAdjusterBase.t()`

  ##### Return
  - **retval**: `TermCriteria`

  Python prototype (for reference only):
  ```python3
  termCriteria() -> retval
  ```
  """
  @spec termCriteria(Evision.Detail.BundleAdjusterBase.t()) :: {integer(), integer(), number()} | {:error, String.t()}
  def termCriteria(self) do
    positional = [
    ]
    :evision_nif.detail_detail_BundleAdjusterBase_termCriteria(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
