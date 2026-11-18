defmodule Evision.Application do
  @moduledoc false
  use Application

  def start(_type, _args) do
    if Code.ensure_loaded?(Kino.SmartCell) and Code.ensure_loaded?(Evision.SmartCell.Zoo) do
      Evision.SmartCell.register_smartcells(Evision.SmartCell.Zoo)
    end

    Supervisor.start_link([], strategy: :one_for_one)
  end
end
