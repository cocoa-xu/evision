defmodule Evision.LineMod.Modality do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `LineMod.Modality` struct.

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
  def to_struct({:ok, %{class: Evision.LineMod.Modality, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.LineMod.Modality, ref: ref}) do
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
  #### Variant 1:
  create

  ##### Positional Arguments
  - **func**: `Evision.FileNode`

  ##### Return
  - **retval**: `Modality`

   \\brief Load a modality from file.

  Python prototype (for reference only):
  ```python3
  create(fn) -> retval
  ```
  #### Variant 2:
  create

  ##### Positional Arguments
  - **modality_type**: `String`

  ##### Return
  - **retval**: `Modality`

   \\brief Create modality by name.
   The following modality types are supported:
  - "ColorGradient"
  - "DepthNormal"

  Python prototype (for reference only):
  ```python3
  create(modality_type) -> retval
  ```

  """
  @spec create(Evision.FileNode.t()) :: Evision.LineMod.Modality.t() | {:error, String.t()}
  def create(func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.linemod_linemod_Modality_create_static(positional)
    |> to_struct()
  end
  @spec create(binary()) :: Evision.LineMod.Modality.t() | {:error, String.t()}
  def create(modality_type) when is_binary(modality_type)
  do
    positional = [
      modality_type: Evision.Internal.Structurise.from_struct(modality_type)
    ]
    :evision_nif.linemod_linemod_Modality_create_static(positional)
    |> to_struct()
  end

  @doc """
  name

  ##### Positional Arguments
  - **self**: `Evision.LineMod.Modality.t()`

  ##### Return
  - **retval**: `String`

  Python prototype (for reference only):
  ```python3
  name() -> retval
  ```
  """
  @spec name(Evision.LineMod.Modality.t()) :: binary() | {:error, String.t()}
  def name(self) do
    positional = [
    ]
    :evision_nif.linemod_linemod_Modality_name(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  process

  ##### Positional Arguments
  - **self**: `Evision.LineMod.Modality.t()`
  - **src**: `Evision.Mat`

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.

  ##### Return
  - **retval**: `QuantizedPyramid`

   \\brief Form a quantized image pyramid from a source image.
   \\param[in] src  The source image. Type depends on the modality.
   \\param[in] mask Optional mask. If not empty, unmasked pixels are set to zero
                   in quantized image and cannot be extracted as features.

  Python prototype (for reference only):
  ```python3
  process(src[, mask]) -> retval
  ```
  """
  @spec process(Evision.LineMod.Modality.t(), Evision.Mat.maybe_mat_in(), [{:mask, term()}] | nil) :: Evision.LineMod.QuantizedPyramid.t() | {:error, String.t()}
  def process(self, src, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:mask])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.linemod_linemod_Modality_process(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  process

  ##### Positional Arguments
  - **self**: `Evision.LineMod.Modality.t()`
  - **src**: `Evision.Mat`

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.

  ##### Return
  - **retval**: `QuantizedPyramid`

   \\brief Form a quantized image pyramid from a source image.
   \\param[in] src  The source image. Type depends on the modality.
   \\param[in] mask Optional mask. If not empty, unmasked pixels are set to zero
                   in quantized image and cannot be extracted as features.

  Python prototype (for reference only):
  ```python3
  process(src[, mask]) -> retval
  ```
  """
  @spec process(Evision.LineMod.Modality.t(), Evision.Mat.maybe_mat_in()) :: Evision.LineMod.QuantizedPyramid.t() | {:error, String.t()}
  def process(self, src) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.linemod_linemod_Modality_process(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  read

  ##### Positional Arguments
  - **self**: `Evision.LineMod.Modality.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.LineMod.Modality.t(), Evision.FileNode.t()) :: Evision.LineMod.Modality.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.linemod_linemod_Modality_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
