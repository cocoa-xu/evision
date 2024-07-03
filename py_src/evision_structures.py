#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from string import Template

videocapture_struct_elixir = '  @typedoc """\n' + \
"""  Type that represents an `Evision.VideoCapture` struct.

  - **fps**. `double`.

    Frames per second.

  - **frame_count**. `double`.

    Total number of frames.
  
  - **frame_width**. `double`.

    Width of each frame.
  
  - **frame_height**. `double`.

    Height of each frame.
  
  - **isOpened**. `boolean`.

    Is successfully opened the video source.
  
  - **ref**. `reference`.

    The underlying erlang resource variable.

""" + \
  '  """' + \
"""
  @type t :: %__MODULE__{
    fps: number(),
    frame_count: number(),
    frame_width: number(),
    frame_height: number(),
    isOpened: boolean(),
    ref: reference()
  }
  @enforce_keys [:fps, :frame_count, :frame_width, :frame_height, :isOpened, :ref]
  defstruct [:fps, :frame_count, :frame_width, :frame_height, :isOpened, :ref]
  alias __MODULE__, as: T

  @doc false
  def to_struct(cap = %{:class => Evision.VideoCapture, :ref => ref}) do
    %T{
      fps: cap.fps,
      frame_count: cap.frame_count,
      frame_width: cap.frame_width,
      frame_height: cap.frame_height,
      isOpened: cap.isOpened,
      ref: ref
    }
  end

  @doc false
  def to_struct({:ok, cap = %{:class => Evision.VideoCapture}}) do
    {:ok, to_struct(cap)}
  end

  @doc false
  def to_struct(pass_through) do
    Evision.Internal.Structurise.to_struct(pass_through)
  end
"""

videocapture_struct_erlang = """
to_struct(#{class := 'Elixir.Evision.VideoCapture', ref := Ref}) ->
  #evision_videocapture{
      ref = Ref
  };
to_struct(Any) ->
  evision_internal_structurise:to_struct(Any).

from_struct(#evision_videocapture{ref = Ref}) ->
  Ref.
"""

videocapture_gleam_typed = """
import gleam/erlang.{type Reference}

pub type VideoCapture {
  VideoCapture(
    fps: Float,
    frame_count: Float,
    frame_width: Float,
    frame_height: Float,
    is_opened: Bool,
    ref: Reference,
  )
}
"""

gpumat_struct_elixir = '  @typedoc """\n' + \
"""  Type that represents an `Evision.CUDA.GpuMat` struct.

  - **channels**: `int`.

    The number of matrix channels.

  - **type**: `Evision.Mat.mat_type()`.

    Type of the matrix elements, following `:nx`'s convention.

  - **raw_type**: `int`.

    The raw value returned from `int cv::Mat::type()`.

  - **shape**: `tuple`.

    The shape of the matrix.
  
  - **elemSize**: `integer()`.

    Element size in bytes.
    
  - **step**: `integer()`.

    Number of bytes between two consecutive rows.
    
    When there are no gaps between successive rows, the value of `step` 
    is equal to the number of columns times the element size.
    
  - **device_id**: `integer() | nil`.
  
    `nil` if currently there's no GPU memory allocated for the GpuMat resource.

  - **ref**: `reference`.

    The underlying erlang resource variable.

""" + \
  '  """' + \
"""
  @type t :: %__MODULE__{
    channels: integer(),
    type: Evision.Mat.mat_type(),
    raw_type: integer(),
    shape: tuple(),
    elemSize: integer(),
    step: integer(),
    device_id: integer() | nil,
    ref: reference()
  }
  @enforce_keys [:channels, :type, :raw_type, :shape, :elemSize, :step, :device_id, :ref]
  defstruct [:channels, :type, :raw_type, :shape, :elemSize, :step, :device_id, :ref]
  alias __MODULE__, as: T

  @doc false
  def to_struct(%{
        :class => Evision.CUDA.GpuMat,
        :channels => channels,
        :type => type,
        :raw_type => raw_type,
        :shape => shape,
        :step => step,
        :device_id => device_id,
        :elemSize => elemSize,
        :ref => ref
      }) do
    %T{
      channels: channels,
      type: type,
      raw_type: raw_type,
      shape: shape,
      step: step,
      device_id: device_id,
      elemSize: elemSize,
      ref: ref
    }
  end

  @doc false
  def to_struct({:ok, mat = %{:class => Evision.CUDA.GpuMat}}) do
    {:ok, to_struct(mat)}
  end

  @doc false
  def to_struct(pass_through) do
    Evision.Internal.Structurise.to_struct(pass_through)
  end
"""

gpumat_struct_erlang = """
to_struct(#{class := 'Elixir.Evision.CUDA.GpuMat', ref := Ref, channels := Channels, type := Type, raw_type := RawType, shape := Shape, step := Step, device_id := DeviceID, elemSize := ElemSize}) ->
  #evision_cuda_gpumat{
      channels = Channels,
      type = Type,
      raw_type = RawType,
      shape = Shape,
      elemSize = ElemSize,
      step = Step,
      device_id = DeviceID,
      ref = Ref
  };
to_struct(Any) ->
    evision_internal_structurise:to_struct(Any).

from_struct(#evision_cuda_gpumat{ref = Ref}) ->
  Ref.
"""

gpumat_gleam_typed = """
import evision/types.{type DType}
import gleam/erlang.{type Reference}

pub type GpuMat {
  GpuMat(
    channels: Int,
    dtype: DType,
    raw_type: Int,
    shape: List(Int),
    elem_size: Int,
    step: Int,
    device_id: Int,
    ref: Reference
  )
}

@external(erlang, "evision_cuda_gpumat", "to_pointer")
pub fn to_pointer(mat: GpuMat) -> any

@external(erlang, "evision_cuda_gpumat", "to_pointer")
pub fn to_pointer_mode(mat: GpuMat, mode: mode) -> any
"""

generic_struct_template_elixir = Template(
  '  @typedoc """\n'
  '  Type that represents an `${elixir_module_name}` struct.\n\n'
  '  - **ref**. `reference()`\n\n'
  '    The underlying erlang resource variable.\n\n'
  '  """\n'
  """  @type t :: %__MODULE__{
    ref: reference()
  }
  @enforce_keys [:ref]
  defstruct [:ref]
  alias __MODULE__, as: T

  @doc false
  def to_struct({:ok, %{class: ${atom_elixir_module_name}, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: ${atom_elixir_module_name}, ref: ref}) do
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
"""
)

generic_struct_template_erlang = Template(
  """
to_struct(#{class := '${atom_elixir_module_name}', ref := Ref}) ->
  #${atom_erlang_module_name}{
      ref = Ref
  };
to_struct(Any) ->
    evision_internal_structurise:to_struct(Any).

from_struct(#${atom_erlang_module_name}{ref = Ref}) ->
  Ref.
"""
)

evision_structs = {
    "VideoCapture": {
      "elixir": videocapture_struct_elixir, 
      "erlang": videocapture_struct_erlang,
      "gleam": (videocapture_struct_erlang, videocapture_gleam_typed)
    },
    "CUDA.GpuMat": {
      "elixir": gpumat_struct_elixir, 
      "erlang": gpumat_struct_erlang,
      "gleam": (gpumat_struct_erlang, gpumat_gleam_typed)
    }
}

evision_structrised_classes = list(evision_structs.keys())
evision_structrised_classes.append('Mat')
