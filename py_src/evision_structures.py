#!/usr/bin/env python3
# -*- coding: utf-8 -*-

videocapture_struct = """
  defstruct [:fps, :frame_count, :frame_width, :frame_height, :isOpened, :ref]
  alias __MODULE__, as: T

  @doc false
  def __make_struct__(cap = %{""" + """:class => :VideoCapture, :ref => ref}) do
    %T{
      fps: cap.fps,
      frame_count: cap.frame_count,
      frame_width: cap.frame_width,
      frame_height: cap.frame_height,
      isOpened: cap.isOpened,
      ref: ref
    }
  end
"""

evision_structs = {
    "VideoCapture": videocapture_struct
}

evision_structrised_classes = list(evision_structs.keys())
evision_structrised_classes.append('Mat')
