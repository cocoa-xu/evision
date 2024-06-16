defmodule Evision.LineMod.QuantizedPyramid do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `LineMod.QuantizedPyramid` struct.

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
  def to_struct({:ok, %{class: Evision.LineMod.QuantizedPyramid, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.LineMod.QuantizedPyramid, ref: ref}) do
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
  extractTemplate

  ##### Positional Arguments
  - **self**: `Evision.LineMod.QuantizedPyramid.t()`

  ##### Return
  - **retval**: `bool`
  - **templ**: `Evision.LineMod.Template.t()`

   \\brief Extract most discriminant features at current pyramid level to form a new template.
   \\param[out] templ The new template.

  Python prototype (for reference only):
  ```python3
  extractTemplate() -> retval, templ
  ```
  """
  @spec extractTemplate(Evision.LineMod.QuantizedPyramid.t()) :: Evision.LineMod.Template.t() | false | {:error, String.t()}
  def extractTemplate(self) do
    positional = [
    ]
    :evision_nif.linemod_linemod_QuantizedPyramid_extractTemplate(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  pyrDown

  ##### Positional Arguments
  - **self**: `Evision.LineMod.QuantizedPyramid.t()`

   \\brief Go to the next pyramid level.
   \\todo Allow pyramid scale factor other than 2

  Python prototype (for reference only):
  ```python3
  pyrDown() -> None
  ```
  """
  @spec pyrDown(Evision.LineMod.QuantizedPyramid.t()) :: Evision.LineMod.QuantizedPyramid.t() | {:error, String.t()}
  def pyrDown(self) do
    positional = [
    ]
    :evision_nif.linemod_linemod_QuantizedPyramid_pyrDown(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  quantize

  ##### Positional Arguments
  - **self**: `Evision.LineMod.QuantizedPyramid.t()`

  ##### Return
  - **dst**: `Evision.Mat.t()`.

   \\brief Compute quantized image at current pyramid level for online detection.
   \\param[out] dst The destination 8-bit image. For each pixel at most one bit is set,
                   representing its classification.

  Python prototype (for reference only):
  ```python3
  quantize([, dst]) -> dst
  ```
  """
  @spec quantize(Evision.LineMod.QuantizedPyramid.t(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def quantize(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    positional = [
    ]
    :evision_nif.linemod_linemod_QuantizedPyramid_quantize(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  quantize

  ##### Positional Arguments
  - **self**: `Evision.LineMod.QuantizedPyramid.t()`

  ##### Return
  - **dst**: `Evision.Mat.t()`.

   \\brief Compute quantized image at current pyramid level for online detection.
   \\param[out] dst The destination 8-bit image. For each pixel at most one bit is set,
                   representing its classification.

  Python prototype (for reference only):
  ```python3
  quantize([, dst]) -> dst
  ```
  """
  @spec quantize(Evision.LineMod.QuantizedPyramid.t()) :: Evision.Mat.t() | {:error, String.t()}
  def quantize(self) do
    positional = [
    ]
    :evision_nif.linemod_linemod_QuantizedPyramid_quantize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
