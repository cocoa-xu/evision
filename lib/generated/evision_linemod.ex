defmodule Evision.LineMod do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `LineMod` struct.

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
  def to_struct({:ok, %{class: Evision.LineMod, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.LineMod, ref: ref}) do
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
  colormap

  ##### Positional Arguments
  - **quantized**: `Evision.Mat`

  ##### Return
  - **dst**: `Evision.Mat.t()`.

   \\brief Debug function to colormap a quantized image for viewing.

  Python prototype (for reference only):
  ```python3
  colormap(quantized[, dst]) -> dst
  ```
  """
  @spec colormap(Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def colormap(quantized, opts) when (is_struct(quantized, Evision.Mat) or is_struct(quantized, Nx.Tensor) or is_number(quantized) or is_tuple(quantized)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      quantized: Evision.Internal.Structurise.from_struct(quantized)
    ]
    :evision_nif.linemod_colormap(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  colormap

  ##### Positional Arguments
  - **quantized**: `Evision.Mat`

  ##### Return
  - **dst**: `Evision.Mat.t()`.

   \\brief Debug function to colormap a quantized image for viewing.

  Python prototype (for reference only):
  ```python3
  colormap(quantized[, dst]) -> dst
  ```
  """
  @spec colormap(Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def colormap(quantized) when (is_struct(quantized, Evision.Mat) or is_struct(quantized, Nx.Tensor) or is_number(quantized) or is_tuple(quantized))
  do
    positional = [
      quantized: Evision.Internal.Structurise.from_struct(quantized)
    ]
    :evision_nif.linemod_colormap(positional)
    |> to_struct()
  end

  @doc """
  drawFeatures

  ##### Positional Arguments
  - **templates**: `[Evision.LineMod.Template]`.

    see @ref Detector::addTemplate

  - **tl**: `Point2i`.

    template bbox top-left offset see @ref Detector::addTemplate

  ##### Keyword Arguments
  - **size**: `integer()`.

    marker size see @ref cv::drawMarker

  ##### Return
  - **img**: `Evision.Mat.t()`.

   \\brief Debug function to draw linemod features

  Python prototype (for reference only):
  ```python3
  drawFeatures(img, templates, tl[, size]) -> img
  ```
  """
  @spec drawFeatures(Evision.Mat.maybe_mat_in(), list(Evision.LineMod.Template.t()), {integer(), integer()}, [{:size, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def drawFeatures(img, templates, tl, opts) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img)) and is_list(templates) and is_tuple(tl) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:size])
    positional = [
      img: Evision.Internal.Structurise.from_struct(img),
      templates: Evision.Internal.Structurise.from_struct(templates),
      tl: Evision.Internal.Structurise.from_struct(tl)
    ]
    :evision_nif.linemod_drawFeatures(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  drawFeatures

  ##### Positional Arguments
  - **templates**: `[Evision.LineMod.Template]`.

    see @ref Detector::addTemplate

  - **tl**: `Point2i`.

    template bbox top-left offset see @ref Detector::addTemplate

  ##### Keyword Arguments
  - **size**: `integer()`.

    marker size see @ref cv::drawMarker

  ##### Return
  - **img**: `Evision.Mat.t()`.

   \\brief Debug function to draw linemod features

  Python prototype (for reference only):
  ```python3
  drawFeatures(img, templates, tl[, size]) -> img
  ```
  """
  @spec drawFeatures(Evision.Mat.maybe_mat_in(), list(Evision.LineMod.Template.t()), {integer(), integer()}) :: Evision.Mat.t() | {:error, String.t()}
  def drawFeatures(img, templates, tl) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img)) and is_list(templates) and is_tuple(tl)
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img),
      templates: Evision.Internal.Structurise.from_struct(templates),
      tl: Evision.Internal.Structurise.from_struct(tl)
    ]
    :evision_nif.linemod_drawFeatures(positional)
    |> to_struct()
  end

  @doc """
  getDefaultLINE
  ##### Return
  - **retval**: `Evision.LineMod.Detector.t()`

   \\brief Factory function for detector using LINE algorithm with color gradients.
   Default parameter settings suitable for VGA images.

  Python prototype (for reference only):
  ```python3
  getDefaultLINE() -> retval
  ```
  """
  @spec getDefaultLINE() :: Evision.LineMod.Detector.t() | {:error, String.t()}
  def getDefaultLINE() do
    positional = [
    ]
    :evision_nif.linemod_getDefaultLINE(positional)
    |> to_struct()
  end

  @doc """
  getDefaultLINEMOD
  ##### Return
  - **retval**: `Evision.LineMod.Detector.t()`

   \\brief Factory function for detector using LINE-MOD algorithm with color gradients
   and depth normals.
   Default parameter settings suitable for VGA images.

  Python prototype (for reference only):
  ```python3
  getDefaultLINEMOD() -> retval
  ```
  """
  @spec getDefaultLINEMOD() :: Evision.LineMod.Detector.t() | {:error, String.t()}
  def getDefaultLINEMOD() do
    positional = [
    ]
    :evision_nif.linemod_getDefaultLINEMOD(positional)
    |> to_struct()
  end
end
