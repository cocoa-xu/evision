#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# This file contains extra functions that were added in evision

gpumat_to_pointer_elixir = '''  @doc """
  Get raw pointers
  
  ##### Positional Arguments

  - **self**. `Evision.CUDA.GpuMat.t()`
  
  ##### Keyword Arguments

  - **mode**, one of `:local`, `:cuda_ipc`.
  
    - `:local`: Get a local CUDA pointer that can be used within current OS process.
    - `:cuda_ipc`: Get a CUDA IPC pointer that can be used across OS processes.
    - `:host_ipc`: Get a host IPC pointer that can be used across OS processes.
  
    Defaults to `:local`.
  """
  @spec to_pointer(Evision.CUDA.GpuMat.t()) :: {:ok, %Evision.IPCHandle.Local{}} | {:error, String.t()}
  def to_pointer(%{ref: ref}) do
    with {:ok, {handle, step, rows, cols, channels, type, device_id}} <- :evision_nif.cuda_cuda_GpuMat_to_pointer(img: ref, mode: :local) do
      {:ok, %Evision.IPCHandle.Local{
        handle: handle,
        step: step,
        rows: rows,
        cols: cols,
        channels: channels,
        type: type,
        device_id: device_id
      }}
    end
  end

  @spec to_pointer(Evision.CUDA.GpuMat.t(), [mode: :local | :cuda_ipc | :host_ipc]) :: 
    {:ok, %Evision.IPCHandle.Local{} | %Evision.IPCHandle.CUDA{} | %Evision.IPCHandle.Host{}} | {:error, String.t()}
  def to_pointer(%{ref: ref}, [mode: mode] = opts)
  when is_list(opts) and mode in [:local, :cuda_ipc, :host_ipc] do
    opts = Keyword.validate!(opts || [], [mode: :local])
    with {:ok, handle} <- :evision_nif.cuda_cuda_GpuMat_to_pointer([img: ref] ++ opts) do
      mode = opts[:mode]
      case {mode, handle} do
        {:local, {handle, step, rows, cols, channels, type, device_id}} ->
          {:ok, %Evision.IPCHandle.Local{
            handle: handle,
            step: step,
            rows: rows,
            cols: cols,
            channels: channels,
            type: type,
            device_id: device_id
          }}
        {:cuda_ipc, {handle, step, rows, cols, channels, type, device_id}} ->
          {:ok, %Evision.IPCHandle.CUDA{
            handle: handle,
            step: step,
            rows: rows,
            cols: cols,
            channels: channels,
            type: type,
            device_id: device_id
          }}
        {:host_ipc, {name, fd, size, rows, cols, channels, type}} ->
          {:ok, %Evision.IPCHandle.Host{
            name: name,
            fd: fd,
            size: size,
            rows: rows,
            cols: cols,
            channels: channels,
            type: type
          }}
      end
    end
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
  
  defp dtype_byte_size(:s8), do: 1
  defp dtype_byte_size(:u8), do: 1
  defp dtype_byte_size(:s16), do: 2
  defp dtype_byte_size(:u16), do: 2
  defp dtype_byte_size(:s32), do: 4
  defp dtype_byte_size(:u32), do: 4
  defp dtype_byte_size(:s64), do: 8
  defp dtype_byte_size(:u64), do: 8
  defp dtype_byte_size(:f16), do: 2
  defp dtype_byte_size(:f32), do: 4
  defp dtype_byte_size(:f64), do: 8
  defp dtype_byte_size(dtype) do
    raise ArgumentError, "Unsupported data type: #{inspect(dtype)}"
  end

  @doc """
  Create CUDA GpuMat from a shared CUDA device pointer with new shape
  
  ##### Positional Arguments

  - **handle**, either an `%Evision.IPCHandle.Local{}` or an `%Evision.IPCHandle.CUDA{}`.

  - **new_shape**, `tuple()`
  
    The shape of the shared image. It's expected to be either
    
    - `{height, width, channels}`, for any 2D image that has 1 or multiple channels
    - `{height, width}`, for any 1-channel 2D image
    - `{rows}`
  """
  @spec from_pointer(%Evision.IPCHandle.Local{} | %Evision.IPCHandle.CUDA{}, Keyword.t()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def from_pointer(handle, opts \\\\ [])
  
  def from_pointer(%Evision.IPCHandle.Local{}=handle, opts) when is_list(opts) do
    shape = opts[:shape]
    do_from_pointer(:local,
      handle.handle,
      handle.step,
      handle.rows,
      handle.cols, 
      handle.channels,
      handle.type,
      shape: shape
    )
  end
  
  def from_pointer(%Evision.IPCHandle.CUDA{}=handle, opts) when is_list(opts) do
    shape = opts[:shape]
    do_from_pointer(:cuda_ipc, 
      handle.handle, 
      handle.step, 
      handle.rows, 
      handle.cols, 
      handle.channels,
      handle.type,
      shape: shape,
      device_id: handle.device_id
    )
  end

  def from_pointer(%Evision.IPCHandle.Host{}, opts) when is_list(opts) do
    raise ArgumentError, "Host IPC handle is not supported for reading yet."
  end

  defp do_from_pointer(kind, handle, step, rows, cols, channels, dtype, opts) when is_list(opts) do
    shape = {rows, cols, channels}
    opts = Keyword.validate!(opts, [device_id: 0, shape: shape])
    expected_step_size = cols * dtype_byte_size(dtype)
    if expected_step_size > step do
      {:error, "New step size is greater than original step size: new = #{expected_step_size}, original = #{step}"}
    else
      positional = [
        kind: kind,
        dtype: compact_type(dtype),
        handle: handle
      ]
      :evision_nif.cuda_cuda_GpuMat_from_pointer(positional ++ opts)
      |> to_struct()
    end
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
