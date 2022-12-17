defmodule Evision.Application do
  @moduledoc false
  use Application

  def start(_type, _args) do
    if Code.ensure_loaded?(Kino), do: Evision.SmartCell.register_smartcells(Evision.SmartCell.Zoo)
    Supervisor.start_link([], strategy: :one_for_one)
  end
end
