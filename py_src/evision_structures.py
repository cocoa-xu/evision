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
  def __to_struct__(cap = %{:class => Evision.VideoCapture, :ref => ref}) do
    %T{
      fps: cap.fps,
      frame_count: cap.frame_count,
      frame_width: cap.frame_width,
      frame_height: cap.frame_height,
      isOpened: cap.isOpened,
      ref: ref
    }
  end

  def __to_struct__({:ok, cap = %{:class => Evision.VideoCapture}}) do
    {:ok, __to_struct__(cap)}
  end

  def __to_struct__(pass_through) do
    Evision.Internal.Structurise.to_struct(pass_through)
  end
"""

videocapture_struct_erlang = """
'__to_struct__'(#{class := 'VideoCapture', ref := Ref}) ->
  #evision_videocapture{
      ref = Ref
  };
'__to_struct__'(Any) ->
    evision_internal_structurise:to_struct(Any).
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
    ref: reference()
  }
  @enforce_keys [:channels, :type, :raw_type, :shape, :elemSize, :ref]
  defstruct [:channels, :type, :raw_type, :shape, :elemSize, :ref]
  alias __MODULE__, as: T

  @doc false
  def __to_struct__(%{
        :class => Evision.CUDA.GpuMat,
        :channels => channels,
        :type => type,
        :raw_type => raw_type,
        :shape => shape,
        :elemSize => elemSize,
        :ref => ref
      }) do
    %T{
      channels: channels,
      type: type,
      raw_type: raw_type,
      shape: shape,
      elemSize: elemSize,
      ref: ref
    }
  end

  def __to_struct__({:ok, mat = %{:class => Evision.CUDA.GpuMat}}) do
    {:ok, __to_struct__(mat)}
  end

  def __to_struct__(pass_through) do
    Evision.Internal.Structurise.to_struct(pass_through)
  end
"""

gpumat_struct_erlang = """
'__to_struct__'(#{class := 'Elixir.Evision.CUDA.GpuMat', ref := Ref, channels := Channels, type := Type, raw_type := RawType, shape := Shape, elemSize := ElemSize}) ->
  #evision_cuda_gpumat{
      channels = Channels,
      type = Type,
      raw_type = RawType,
      shape = Shape,
      ref = Ref,
      elemSize = ElemSize
  };
'__to_struct__'(Any) ->
    evision_internal_structurise:to_struct(Any).
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
  def __to_struct__({:ok, %{class: ${atom_elixir_module_name}, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  def __to_struct__(%{class: ${atom_elixir_module_name}, ref: ref}) do
    %T{
      ref: ref
    }
  end

  def __to_struct__(ret) do
    Evision.Internal.Structurise.to_struct(ret)
  end
"""
)

generic_struct_template_erlang = Template(
  """
'__to_struct__'(#{class := '${atom_elixir_module_name}', ref := Ref}) ->
  #${atom_erlang_module_name}{
      ref = Ref
  };
'__to_struct__'(Any) ->
    evision_internal_structurise:to_struct(Any).

"""
)

evision_structs = {
    "VideoCapture": {
      "elixir": videocapture_struct_elixir, 
      "erlang": videocapture_struct_erlang
    },
    "CUDA.GpuMat": {
      "elixir": gpumat_struct_elixir, 
      "erlang": gpumat_struct_erlang
    }
}

evision_structrised_classes = list(evision_structs.keys())
evision_structrised_classes.append('Mat')
