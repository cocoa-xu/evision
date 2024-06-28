defmodule Evision.CUDA.GpuMat.Handle do
  @moduledoc """
  Represents a handle for sharing data allocated on a CUDA device between OS processes.

  - `:type` can be either `:local` or `:cuda_ipc`.
  - `:handle`, 
  
    when `:type` is `:local`, is a pointer to the memory that can be accessed
    within the same OS process.

    when `:type` is `:cuda_ipc`, is a pointer to the memory that can be accessed
    by other OS processes via CUDA IPC.

  - `:device_id` is the CUDA device ID that the memory is allocated on.
  - `:size` is the size of the memory in bytes.
  """

  defstruct [:type, :handle, :device_id, :size]
end
