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
  
  defp compact_type(type) when is_atom(type), do: type
  defp compact_type({:s, 8}), do: :s8
  defp compact_type({:u, 8}), do: :u8
  defp compact_type({:s, 16}), do: :s16
  defp compact_type({:u, 16}), do: :u16
  defp compact_type({:s, 32}), do: :s32
  defp compact_type({:u, 32}), do: :u32
  defp compact_type({:s, 64}), do: :s64
  defp compact_type({:u, 64}), do: :u64
  defp compact_type({:f, 16}), do: :f16
  defp compact_type({:f, 32}), do: :f32
  defp compact_type({:f, 64}), do: :f64

  @doc """
  Create CUDA GpuMat from a shared CUDA device pointer
  """
  @spec from_pointer(list(integer()), atom() | {atom(), integer()}, tuple()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def from_pointer(device_pointer, dtype, shape) when is_list(device_pointer) and is_tuple(shape) do
    :evision_nif.cuda_cuda_GpuMat_from_pointer([device_pointer: device_pointer, dtype: compact_type(dtype), shape: shape])
    |> to_struct()
  end
'''

gpumat_to_pointer_erlang = """
to_pointer(#{class := 'Elixir.Evision.CUDA.GpuMat', ref := Ref}) ->
  evision_nif:cuda_cuda_GpuMat_to_pointer([{img, Ref}, {mode, local}]).
to_pointer(#{class := 'Elixir.Evision.CUDA.GpuMat', ref := Ref}, Mode) ->
  evision_nif:cuda_cuda_GpuMat_to_pointer([{img, Ref}, {mode, Mode}]).
  
from_pointer(DevicePointer, DataType, Shape) when is_list(DevicePointer) ->
  evision_nif:cuda_cuda_GpuMat_from_pointer([{device_pointer, DevicePointer}, {dtype, DataType}, {shape, Shape}]).
"""

extra_functions = {
    "CUDA.GpuMat": {
      "elixir": gpumat_to_pointer_elixir, 
      "erlang": gpumat_to_pointer_erlang,
      "gleam": gpumat_to_pointer_erlang
    }
}

evision_extra_functions = list(extra_functions.keys())
