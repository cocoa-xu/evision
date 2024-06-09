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

  def to_struct({:ok, cap = %{:class => Evision.VideoCapture}}) do
    {:ok, to_struct(cap)}
  end

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
  def to_struct(%{
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

  def to_struct({:ok, mat = %{:class => Evision.CUDA.GpuMat}}) do
    {:ok, to_struct(mat)}
  end

  def to_struct(pass_through) do
    Evision.Internal.Structurise.to_struct(pass_through)
  end
"""

gpumat_struct_erlang = """
to_struct(#{class := 'Elixir.Evision.CUDA.GpuMat', ref := Ref, channels := Channels, type := Type, raw_type := RawType, shape := Shape, elemSize := ElemSize}) ->
  #evision_cuda_gpumat{
      channels = Channels,
      type = Type,
      raw_type = RawType,
      shape = Shape,
      ref = Ref,
      elemSize = ElemSize
  };
to_struct(Any) ->
    evision_internal_structurise:to_struct(Any).

from_struct(#evision_cuda_gpumat{ref = Ref}) ->
  Ref.
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

  def to_struct(%{class: ${atom_elixir_module_name}, ref: ref}) do
    %T{
      ref: ref
    }
  end

  def to_struct(ret) do
    Evision.Internal.Structurise.to_struct(ret)
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
      "erlang": videocapture_struct_erlang
    },
    "CUDA.GpuMat": {
      "elixir": gpumat_struct_elixir, 
      "erlang": gpumat_struct_erlang
    }
}

evision_structrised_classes = list(evision_structs.keys())
evision_structrised_classes.append('Mat')
