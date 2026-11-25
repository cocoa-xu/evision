defmodule Evision.CUDA.GpuMat.Pointer.Test do
  use ExUnit.Case, async: true

  # from_pointer only wraps the address and never reads it, so a made-up one is safe here.
  test "a local pointer survives from_pointer and to_pointer" do
    pointer = 0x7F12_3456_7000

    handle = %Evision.IPCHandle.Local{
      handle: pointer,
      step: 3,
      rows: 2,
      cols: 3,
      channels: 1,
      type: :u8,
      device_id: nil
    }

    gpumat = Evision.CUDA.GpuMat.from_pointer(handle)

    assert {:ok, %Evision.IPCHandle.Local{handle: ^pointer, rows: 2, cols: 3, channels: 1} = back} =
             Evision.CUDA.GpuMat.to_pointer(gpumat)

    assert {:ok, %Evision.IPCHandle.Local{handle: ^pointer}} =
             back |> Evision.CUDA.GpuMat.from_pointer() |> Evision.CUDA.GpuMat.to_pointer()
  end
end
