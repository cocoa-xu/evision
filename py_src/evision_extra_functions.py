#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# This file contains extra functions that were added in evision

gpumat_to_pointer_elixir = '''  @doc """
  Get raw pointers
  
  ##### Positional Arguments

  - **self**. `Evision.CUDA.GpuMat.t()`
  
  ##### Keyword Arguments

  - **mode**, either `:local` or `:cuda_ipc`
  """
  @spec to_pointer(Evision.CUDA.GpuMat.t()) :: {:ok, list(integer())} | {:error, String.t()}
  def to_pointer(%{ref: ref}) do
    :evision_nif.cuda_cuda_GpuMat_to_pointer(img: ref, mode: :local)
  end

  @spec to_pointer(Evision.CUDA.GpuMat.t(), [mode: :local | :cuda_ipc]) :: {:ok, list(integer())} | {:error, String.t()}
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
  
  ##### Positional Arguments

  - **device_pointer**, `list(integer())`.
  
    This can be either a local pointer or an IPC pointer.
    
    However, please note that IPC pointers have to be generated from 
    another OS process (Erlang process doesn't count).
  
  - **dtype**, `tuple() | atom()`
  
    Data type.
    
  - **shape**, `tuple()`
  
    The shape of the shared image. It's expected to be either
    
    - `{height, width, channels}`, for any 2D image that has 1 or multiple channels
    - `{height, width}`, for any 1-channel 2D image
    - `{rows}`
  """
  @spec from_pointer(list(integer()), atom() | {atom(), integer()}, tuple()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def from_pointer(device_pointer, dtype, shape) when is_list(device_pointer) and is_tuple(shape) do
    positional = [
      device_pointer: device_pointer,
      dtype: compact_type(dtype),
      shape: shape
    ]
    :evision_nif.cuda_cuda_GpuMat_from_pointer(positional)
    |> to_struct()
  end

  @doc """
  Create CUDA GpuMat from a shared CUDA device pointer
  
  ##### Positional Arguments

  - **device_pointer**, `list(integer())`.
  
    This can be either a local pointer or an IPC pointer.
    
    However, please note that IPC pointers have to be generated from 
    another OS process (Erlang process doesn't count).
  
  - **dtype**, `tuple() | atom()`
  
    Data type.
    
  - **shape**, `tuple()`
  
    The shape of the shared image. It's expected to be either
    
    - `{height, width, channels}`, for any 2D image that has 1 or multiple channels
    - `{height, width}`, for any 1-channel 2D image
    - `{rows}`
    
  ##### Keyword Arguments

  - **device_id**, `non_neg_integer`. 
  
    GPU Device ID, default to `0`.
  """
  @spec from_pointer(list(integer()), atom() | {atom(), integer()}, tuple(), [device_id: non_neg_integer()]) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def from_pointer(device_pointer, dtype, shape, opts) when is_list(device_pointer) and is_tuple(shape) and is_list(opts) do
    opts = Keyword.validate!(opts || [], [device_id: 0])
    positional = [
      device_pointer: device_pointer,
      dtype: compact_type(dtype),
      shape: shape
    ]
    :evision_nif.cuda_cuda_GpuMat_from_pointer(positional ++ Evision.Internal.Structurise.from_struct(opts))
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
