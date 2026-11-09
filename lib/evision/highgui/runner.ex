defmodule Evision.HighGui.Runner do
  @moduledoc false

  use GenServer

  require Logger

  # The native GUI thread sends to this registered name.
  @name :evision_highgui

  def start_link(_opts), do: GenServer.start_link(__MODULE__, nil, name: @name)

  def running?, do: Process.whereis(@name) != nil

  @impl true
  def init(nil), do: {:ok, nil}

  @impl true
  def handle_info({:evision_highgui_callback, fun, args}, state) do
    try do
      apply(fun, args)
    catch
      kind, reason ->
        Logger.error(
          "Evision.HighGui callback failed: " <> Exception.format(kind, reason, __STACKTRACE__)
        )
    end

    {:noreply, state}
  end

  def handle_info({:evision_highgui_error, message}, state) do
    Logger.error("Evision.HighGui: " <> message)
    {:noreply, state}
  end

  # Callbacks run in this process and may leave messages behind, such as late replies.
  def handle_info(_message, state), do: {:noreply, state}
end
