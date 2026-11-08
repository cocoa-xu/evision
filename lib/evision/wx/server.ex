if Code.ensure_loaded?(:wx_object) do
  defmodule Evision.Wx.Server do
    @moduledoc false

    use GenServer

    alias Evision.Wx.Window

    @max_queued_keys 64

    def start_link(_opts \\ []) do
      GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
    end

    def window(name, flags \\ nil),
      do: GenServer.call(__MODULE__, {:window, name, flags}, :infinity)

    def destroy(name), do: GenServer.call(__MODULE__, {:destroy, name}, :infinity)

    def destroy_all, do: GenServer.call(__MODULE__, :destroy_all, :infinity)

    def wait_key(timeout), do: GenServer.call(__MODULE__, {:wait_key, timeout}, :infinity)

    @impl true
    def init(:ok) do
      Process.flag(:trap_exit, true)
      {:ok, %{env: nil, windows: %{}, names: %{}, keys: :queue.new(), waiters: []}}
    end

    @impl true
    def handle_call({:window, name, flags}, _from, state) do
      state = forget_dead_window(state, name)

      case {Map.fetch(state.windows, name), flags} do
        {{:ok, pid}, _} ->
          {:reply, {:ok, pid}, state}

        {:error, nil} ->
          {:reply, {:error, "window #{inspect(name)} does not exist"}, state}

        {:error, flags} ->
          with {:ok, state} <- ensure_env(state),
               {:ok, pid} <- Window.start_link(state.env, self(), name, flags) do
            state = %{
              state
              | windows: Map.put(state.windows, name, pid),
                names: Map.put(state.names, pid, name)
            }

            {:reply, {:ok, pid}, state}
          else
            {:error, reason} -> {:reply, {:error, error_message(reason)}, state}
          end
      end
    end

    def handle_call({:destroy, name}, _from, state) do
      state =
        case Map.fetch(state.windows, name) do
          {:ok, pid} -> stop_window(state, pid)
          :error -> state
        end

      {:reply, :ok, state}
    end

    def handle_call(:destroy_all, _from, state) do
      {:reply, :ok, Enum.reduce(Map.keys(state.names), state, &stop_window(&2, &1))}
    end

    def handle_call({:wait_key, timeout}, from, state) do
      case :queue.out(state.keys) do
        {{:value, key}, keys} ->
          {:reply, key, %{state | keys: keys}}

        {:empty, _} when timeout == 0 or map_size(state.windows) == 0 ->
          {:reply, -1, state}

        {:empty, _} ->
          {pid, _} = from

          timer =
            if timeout != :infinity,
              do: Process.send_after(self(), {:wait_key_timeout, from}, timeout)

          waiter = %{from: from, timer: timer, monitor: Process.monitor(pid)}
          {:noreply, %{state | waiters: state.waiters ++ [waiter]}}
      end
    end

    @impl true
    def handle_info({:key, key}, %{waiters: [waiter | waiters]} = state) do
      reply_waiter(waiter, key)
      {:noreply, %{state | waiters: waiters}}
    end

    def handle_info({:key, key}, state) do
      keys = :queue.in(key, state.keys)

      keys =
        if :queue.len(keys) > @max_queued_keys,
          do: :queue.drop(keys),
          else: keys

      {:noreply, %{state | keys: keys}}
    end

    def handle_info({:wait_key_timeout, from}, state) do
      {waiters, timed_out} = Enum.split_with(state.waiters, &(&1.from != from))
      Enum.each(timed_out, &reply_waiter(&1, -1))
      {:noreply, %{state | waiters: waiters}}
    end

    def handle_info({:DOWN, ref, :process, _pid, _reason}, state) do
      {:noreply, %{state | waiters: Enum.reject(state.waiters, &(&1.monitor == ref))}}
    end

    def handle_info({:EXIT, pid, _reason}, state) do
      {:noreply, forget_window(state, pid)}
    end

    def handle_info(_message, state), do: {:noreply, state}

    defp ensure_env(%{env: nil} = state) do
      :wx.new()
      {:ok, %{state | env: :wx.get_env()}}
    catch
      kind, reason -> {:error, {:wx, kind, reason}}
    end

    defp ensure_env(state), do: {:ok, state}

    defp stop_window(state, pid) do
      Process.unlink(pid)

      try do
        :wx_object.stop(pid)
      catch
        :exit, _ -> :ok
      end

      forget_window(state, pid)
    end

    # A window closed by the user may be dead before its exit signal is handled.
    defp forget_dead_window(state, name) do
      case Map.fetch(state.windows, name) do
        {:ok, pid} -> if Process.alive?(pid), do: state, else: forget_window(state, pid)
        :error -> state
      end
    end

    defp forget_window(state, pid) do
      case Map.pop(state.names, pid) do
        {nil, _} ->
          state

        {name, names} ->
          state = %{state | names: names, windows: Map.delete(state.windows, name)}
          if map_size(state.windows) == 0, do: release_waiters(state), else: state
      end
    end

    defp release_waiters(state) do
      Enum.each(state.waiters, &reply_waiter(&1, -1))
      %{state | waiters: []}
    end

    defp reply_waiter(waiter, reply) do
      if waiter.timer, do: Process.cancel_timer(waiter.timer)
      Process.demonitor(waiter.monitor, [:flush])
      GenServer.reply(waiter.from, reply)
    end

    defp error_message({:wx, kind, reason}),
      do: "cannot start wx: " <> Exception.format_banner(kind, reason)

    defp error_message(reason), do: "cannot open window: #{inspect(reason)}"
  end
end
