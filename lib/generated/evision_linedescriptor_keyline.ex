defmodule Evision.LineDescriptor.KeyLine do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `LineDescriptor.KeyLine` struct.

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
  def to_struct({:ok, %{class: Evision.LineDescriptor.KeyLine, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.LineDescriptor.KeyLine, ref: ref}) do
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
  KeyLine
  ##### Return
  - **self**: `Evision.LineDescriptor.KeyLine.t()`

  constructor

  Python prototype (for reference only):
  ```python3
  KeyLine() -> <line_descriptor_KeyLine object>
  ```
  """
  @spec keyLine() :: Evision.LineDescriptor.KeyLine.t() | {:error, String.t()}
  def keyLine() do
    positional = [
    ]
    :evision_nif.line_descriptor_line_descriptor_KeyLine_KeyLine(positional)
    |> to_struct()
  end

  @doc """
  getEndPoint

  ##### Positional Arguments
  - **self**: `Evision.LineDescriptor.KeyLine.t()`

  ##### Return
  - **retval**: `Point2f`

  Returns the end point of the line in the original image

  Python prototype (for reference only):
  ```python3
  getEndPoint() -> retval
  ```
  """
  @spec getEndPoint(Evision.LineDescriptor.KeyLine.t()) :: {number(), number()} | {:error, String.t()}
  def getEndPoint(self) do
    positional = [
    ]
    :evision_nif.line_descriptor_line_descriptor_KeyLine_getEndPoint(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getEndPointInOctave

  ##### Positional Arguments
  - **self**: `Evision.LineDescriptor.KeyLine.t()`

  ##### Return
  - **retval**: `Point2f`

  Returns the end point of the line in the octave it was extracted from

  Python prototype (for reference only):
  ```python3
  getEndPointInOctave() -> retval
  ```
  """
  @spec getEndPointInOctave(Evision.LineDescriptor.KeyLine.t()) :: {number(), number()} | {:error, String.t()}
  def getEndPointInOctave(self) do
    positional = [
    ]
    :evision_nif.line_descriptor_line_descriptor_KeyLine_getEndPointInOctave(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getStartPoint

  ##### Positional Arguments
  - **self**: `Evision.LineDescriptor.KeyLine.t()`

  ##### Return
  - **retval**: `Point2f`

  Returns the start point of the line in the original image

  Python prototype (for reference only):
  ```python3
  getStartPoint() -> retval
  ```
  """
  @spec getStartPoint(Evision.LineDescriptor.KeyLine.t()) :: {number(), number()} | {:error, String.t()}
  def getStartPoint(self) do
    positional = [
    ]
    :evision_nif.line_descriptor_line_descriptor_KeyLine_getStartPoint(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getStartPointInOctave

  ##### Positional Arguments
  - **self**: `Evision.LineDescriptor.KeyLine.t()`

  ##### Return
  - **retval**: `Point2f`

  Returns the start point of the line in the octave it was extracted from

  Python prototype (for reference only):
  ```python3
  getStartPointInOctave() -> retval
  ```
  """
  @spec getStartPointInOctave(Evision.LineDescriptor.KeyLine.t()) :: {number(), number()} | {:error, String.t()}
  def getStartPointInOctave(self) do
    positional = [
    ]
    :evision_nif.line_descriptor_line_descriptor_KeyLine_getStartPointInOctave(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec get_angle(Evision.LineDescriptor.KeyLine.t()) :: number()
  def get_angle(self) do
    :evision_nif.line_descriptor_KeyLine_get_angle(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_angle(Evision.LineDescriptor.KeyLine.t(), number()) :: Evision.LineDescriptor.KeyLine.t()
  def set_angle(self, prop) do
    :evision_nif.line_descriptor_KeyLine_set_angle(
        Evision.Internal.Structurise.from_struct(self),
        [angle: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_class_id(Evision.LineDescriptor.KeyLine.t()) :: integer()
  def get_class_id(self) do
    :evision_nif.line_descriptor_KeyLine_get_class_id(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_class_id(Evision.LineDescriptor.KeyLine.t(), integer()) :: Evision.LineDescriptor.KeyLine.t()
  def set_class_id(self, prop) do
    :evision_nif.line_descriptor_KeyLine_set_class_id(
        Evision.Internal.Structurise.from_struct(self),
        [class_id: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_ePointInOctaveX(Evision.LineDescriptor.KeyLine.t()) :: number()
  def get_ePointInOctaveX(self) do
    :evision_nif.line_descriptor_KeyLine_get_ePointInOctaveX(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_ePointInOctaveX(Evision.LineDescriptor.KeyLine.t(), number()) :: Evision.LineDescriptor.KeyLine.t()
  def set_ePointInOctaveX(self, prop) do
    :evision_nif.line_descriptor_KeyLine_set_ePointInOctaveX(
        Evision.Internal.Structurise.from_struct(self),
        [ePointInOctaveX: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_ePointInOctaveY(Evision.LineDescriptor.KeyLine.t()) :: number()
  def get_ePointInOctaveY(self) do
    :evision_nif.line_descriptor_KeyLine_get_ePointInOctaveY(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_ePointInOctaveY(Evision.LineDescriptor.KeyLine.t(), number()) :: Evision.LineDescriptor.KeyLine.t()
  def set_ePointInOctaveY(self, prop) do
    :evision_nif.line_descriptor_KeyLine_set_ePointInOctaveY(
        Evision.Internal.Structurise.from_struct(self),
        [ePointInOctaveY: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_endPointX(Evision.LineDescriptor.KeyLine.t()) :: number()
  def get_endPointX(self) do
    :evision_nif.line_descriptor_KeyLine_get_endPointX(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_endPointX(Evision.LineDescriptor.KeyLine.t(), number()) :: Evision.LineDescriptor.KeyLine.t()
  def set_endPointX(self, prop) do
    :evision_nif.line_descriptor_KeyLine_set_endPointX(
        Evision.Internal.Structurise.from_struct(self),
        [endPointX: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_endPointY(Evision.LineDescriptor.KeyLine.t()) :: number()
  def get_endPointY(self) do
    :evision_nif.line_descriptor_KeyLine_get_endPointY(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_endPointY(Evision.LineDescriptor.KeyLine.t(), number()) :: Evision.LineDescriptor.KeyLine.t()
  def set_endPointY(self, prop) do
    :evision_nif.line_descriptor_KeyLine_set_endPointY(
        Evision.Internal.Structurise.from_struct(self),
        [endPointY: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_lineLength(Evision.LineDescriptor.KeyLine.t()) :: number()
  def get_lineLength(self) do
    :evision_nif.line_descriptor_KeyLine_get_lineLength(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_lineLength(Evision.LineDescriptor.KeyLine.t(), number()) :: Evision.LineDescriptor.KeyLine.t()
  def set_lineLength(self, prop) do
    :evision_nif.line_descriptor_KeyLine_set_lineLength(
        Evision.Internal.Structurise.from_struct(self),
        [lineLength: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_numOfPixels(Evision.LineDescriptor.KeyLine.t()) :: integer()
  def get_numOfPixels(self) do
    :evision_nif.line_descriptor_KeyLine_get_numOfPixels(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_numOfPixels(Evision.LineDescriptor.KeyLine.t(), integer()) :: Evision.LineDescriptor.KeyLine.t()
  def set_numOfPixels(self, prop) do
    :evision_nif.line_descriptor_KeyLine_set_numOfPixels(
        Evision.Internal.Structurise.from_struct(self),
        [numOfPixels: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_octave(Evision.LineDescriptor.KeyLine.t()) :: integer()
  def get_octave(self) do
    :evision_nif.line_descriptor_KeyLine_get_octave(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_octave(Evision.LineDescriptor.KeyLine.t(), integer()) :: Evision.LineDescriptor.KeyLine.t()
  def set_octave(self, prop) do
    :evision_nif.line_descriptor_KeyLine_set_octave(
        Evision.Internal.Structurise.from_struct(self),
        [octave: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_pt(Evision.LineDescriptor.KeyLine.t()) :: {number(), number()}
  def get_pt(self) do
    :evision_nif.line_descriptor_KeyLine_get_pt(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_pt(Evision.LineDescriptor.KeyLine.t(), {number(), number()}) :: Evision.LineDescriptor.KeyLine.t()
  def set_pt(self, prop) do
    :evision_nif.line_descriptor_KeyLine_set_pt(
        Evision.Internal.Structurise.from_struct(self),
        [pt: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_response(Evision.LineDescriptor.KeyLine.t()) :: number()
  def get_response(self) do
    :evision_nif.line_descriptor_KeyLine_get_response(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_response(Evision.LineDescriptor.KeyLine.t(), number()) :: Evision.LineDescriptor.KeyLine.t()
  def set_response(self, prop) do
    :evision_nif.line_descriptor_KeyLine_set_response(
        Evision.Internal.Structurise.from_struct(self),
        [response: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_sPointInOctaveX(Evision.LineDescriptor.KeyLine.t()) :: number()
  def get_sPointInOctaveX(self) do
    :evision_nif.line_descriptor_KeyLine_get_sPointInOctaveX(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_sPointInOctaveX(Evision.LineDescriptor.KeyLine.t(), number()) :: Evision.LineDescriptor.KeyLine.t()
  def set_sPointInOctaveX(self, prop) do
    :evision_nif.line_descriptor_KeyLine_set_sPointInOctaveX(
        Evision.Internal.Structurise.from_struct(self),
        [sPointInOctaveX: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_sPointInOctaveY(Evision.LineDescriptor.KeyLine.t()) :: number()
  def get_sPointInOctaveY(self) do
    :evision_nif.line_descriptor_KeyLine_get_sPointInOctaveY(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_sPointInOctaveY(Evision.LineDescriptor.KeyLine.t(), number()) :: Evision.LineDescriptor.KeyLine.t()
  def set_sPointInOctaveY(self, prop) do
    :evision_nif.line_descriptor_KeyLine_set_sPointInOctaveY(
        Evision.Internal.Structurise.from_struct(self),
        [sPointInOctaveY: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_size(Evision.LineDescriptor.KeyLine.t()) :: number()
  def get_size(self) do
    :evision_nif.line_descriptor_KeyLine_get_size(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_size(Evision.LineDescriptor.KeyLine.t(), number()) :: Evision.LineDescriptor.KeyLine.t()
  def set_size(self, prop) do
    :evision_nif.line_descriptor_KeyLine_set_size(
        Evision.Internal.Structurise.from_struct(self),
        [size: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_startPointX(Evision.LineDescriptor.KeyLine.t()) :: number()
  def get_startPointX(self) do
    :evision_nif.line_descriptor_KeyLine_get_startPointX(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_startPointX(Evision.LineDescriptor.KeyLine.t(), number()) :: Evision.LineDescriptor.KeyLine.t()
  def set_startPointX(self, prop) do
    :evision_nif.line_descriptor_KeyLine_set_startPointX(
        Evision.Internal.Structurise.from_struct(self),
        [startPointX: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_startPointY(Evision.LineDescriptor.KeyLine.t()) :: number()
  def get_startPointY(self) do
    :evision_nif.line_descriptor_KeyLine_get_startPointY(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_startPointY(Evision.LineDescriptor.KeyLine.t(), number()) :: Evision.LineDescriptor.KeyLine.t()
  def set_startPointY(self, prop) do
    :evision_nif.line_descriptor_KeyLine_set_startPointY(
        Evision.Internal.Structurise.from_struct(self),
        [startPointY: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
end
