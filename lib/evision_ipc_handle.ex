defmodule Evision.IPCHandle.Local do
  @moduledoc """
  Represents a local CUDA handle for sharing data allocated on the same CUDA device within the same OS processes.

  - `:handle`, a pointer to the memory that can be accessed within the same OS process.

    when `:type` is `:cuda_ipc`, is a pointer to the memory that can be accessed
    by other OS processes via CUDA IPC.

  - `:step`, the number of bytes between two consecutive rows.
  
    When there are no gaps between successive rows, the value of `step` 
    is equal to the number of columns times the size of the data type. 

  - `:rows`, the number of rows.
  - `:cols`, the number of columns.
  - `:channels`, the number of channels.
  - `:type`, the data type.
  """

  defstruct [:handle, :step, :rows, :cols, :channels, :type]
end

defmodule Evision.IPCHandle.CUDA do
  @moduledoc """
  Represents a CUDA IPC handle for sharing data allocated on the same CUDA device between different OS processes.

  - `:handle`, a binary-encoded `cudaIpcMemHandle_t` that can be loaded
    at the native level via `memcpy` or similar functions.

  - `:step`, the number of bytes between two consecutive rows.
  
    When there are no gaps between successive rows, the value of `step` 
    is equal to the number of columns times the size of the data type. 

  - `:rows`, the number of rows.
  - `:cols`, the number of columns.
  - `:channels`, the number of channels.
  - `:type`, the data type.
  - `:device_id` is the CUDA device ID that the memory is allocated on.
  """

  defstruct [:handle, :step, :rows, :cols, :channels, :type, :device_id]
end
