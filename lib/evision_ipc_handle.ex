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
  - `:device_id` is the CUDA device ID that the memory is allocated on.
  """

  defstruct [:handle, :step, :rows, :cols, :channels, :type, :device_id]
  alias __MODULE__, as: T

  defimpl Inspect do
    import Inspect.Algebra

    defp pointer_bytes(addr) when addr > 0xFFFFFFFF do
      4
    end

    defp pointer_bytes(_) do
      8
    end

    defp to_algebra(name, val, opts) when is_binary(name) and is_number(val) do
      concat([
        color("#{name}: ", :atom, opts),
        color("#{val}", :number, opts)
      ])
    end

    defp to_algebra(name, {type, value}, opts)
         when is_binary(name) and is_atom(type) and is_integer(value) do
      concat([
        color("#{name}: ", :atom, opts),
        color("{", :list, opts),
        color(":#{type}", :atom, opts),
        color(", ", :list, opts),
        color("#{value}", :number, opts),
        color("}", :list, opts)
      ])
    end

    defp to_algebra(name, val, opts) when is_binary(name) and is_atom(val) do
      concat([
        color("#{name}: ", :atom, opts),
        if(val, do: color(":#{val}", :atom, opts), else: color("nil", :atom, opts))
      ])
    end

    def inspect(%T{} = handle, opts) do
      sep = color(",", :list, opts)

      handle_address =
        concat([
          color("handle: ", :atom, opts),
          color(
            "0x" <>
              String.pad_leading(
                String.downcase(Integer.to_string(handle.handle, 16)),
                pointer_bytes(handle.handle) * 2,
                "0"
              ),
            :number,
            opts
          )
        ])

      step = to_algebra("step", handle.step, opts)
      rows = to_algebra("rows", handle.rows, opts)
      cols = to_algebra("cols", handle.cols, opts)
      channels = to_algebra("channels", handle.channels, opts)
      type = to_algebra("type", handle.type, opts)
      device_id = to_algebra("device_id", handle.device_id, opts)

      force_unfit(
        concat([
          color("%Evision.IPCHandle.Local{", :map, opts),
          nest(
            concat([
              line(),
              handle_address,
              sep,
              line(),
              step,
              sep,
              line(),
              rows,
              sep,
              line(),
              cols,
              sep,
              line(),
              channels,
              sep,
              line(),
              type,
              sep,
              line(),
              device_id
            ]),
            2
          ),
          line(),
          color("}", :map, opts)
        ])
      )
    end
  end
end

defmodule Evision.IPCHandle.CUDA do
  @moduledoc """
  Represents a CUDA IPC handle for sharing data allocated on CUDA device between different OS processes.

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

defmodule Evision.IPCHandle.Host do
  @moduledoc """
  Represents a host IPC handle for sharing data allocated on the CUDA device between different OS processes.

  - `:name` is the name of the shared memory object.
  - `:fd` is the file descriptor of the shared memory object.
  - `:size` is the size of the mapped shared memory in bytes.
  - `:rows`, the number of rows.
  - `:cols`, the number of columns.
  - `:channels`, the number of channels.
  - `:type`, the data type.
  """

  defstruct [:name, :fd, :size, :rows, :cols, :channels, :type]
end
