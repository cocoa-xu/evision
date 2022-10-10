#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from string import Template

videocapture_struct = '  @typedoc """\n' + \
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
  def __to_struct__(cap = %{:class => :VideoCapture, :ref => ref}) do
    %T{
      fps: cap.fps,
      frame_count: cap.frame_count,
      frame_width: cap.frame_width,
      frame_height: cap.frame_height,
      isOpened: cap.isOpened,
      ref: ref
    }
  end

  def __to_struct__({:ok, cap = %{:class => :VideoCapture}}) do
    {:ok, __to_struct__(cap)}
  end

  def __to_struct__(pass_through) do
    Evision.Internal.Structurise.to_struct(pass_through)
  end
"""


generic_struct_template = Template(
  '  @typedoc """\n'
  '  Type that represents an `Evision.${elixir_module_name}` struct.\n\n'
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
  def __to_struct__({:ok, %{class: :${atom_elixir_module_name}, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  def __to_struct__(%{class: :${atom_elixir_module_name}, ref: ref}) do
    %T{
      ref: ref
    }
  end

  def __to_struct__(ret) do
    Evision.Internal.Structurise.to_struct(ret)
  end
"""
)

evision_structs = {
    "VideoCapture": videocapture_struct
}

evision_structrised_classes = list(evision_structs.keys())
evision_structrised_classes.append('Mat')
