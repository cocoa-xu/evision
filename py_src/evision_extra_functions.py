#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# This file contains extra functions that were added in evision

gpumat_to_pointer_elixir = '''  @doc """
  Get raw pointers
  """
  def to_pointer(%{ref: ref}) do
    :evision_nif.cuda_cuda_GpuMat_to_pointer(img: ref, mode: :local)
  end

  def to_pointer(%{ref: ref}, opts) when is_list(opts) do
    opts = Keyword.validate!(opts || [], [mode: :local])
    :evision_nif.cuda_cuda_GpuMat_to_pointer([img: ref] ++ opts)
  end

'''

gpumat_to_pointer_erlang = """
to_pointer(#{class := 'Elixir.Evision.CUDA.GpuMat', ref := Ref}) ->
  :evision_nif.cuda_cuda_GpuMat_to_pointer([{img, Ref}, {mode, local}]).
to_pointer(#{class := 'Elixir.Evision.CUDA.GpuMat', ref := Ref}, Mode) ->
  :evision_nif.cuda_cuda_GpuMat_to_pointer([{img, Ref}, {mode, Mode}]).
"""

extra_functions = {
    "CUDA.GpuMat": {
      "elixir": gpumat_to_pointer_elixir, 
      "erlang": gpumat_to_pointer_erlang
    }
}

evision_extra_functions = list(extra_functions.keys())
