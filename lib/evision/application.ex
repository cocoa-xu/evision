defmodule Evision.Application do
  @moduledoc false
  use Application

  def start(_type, _args) do
    if Code.ensure_loaded?(Kino.SmartCell) and Code.ensure_loaded?(Evision.SmartCell.Zoo) do
      Evision.SmartCell.register_smartcells(Evision.SmartCell.Zoo)
    end

    wx = if Code.ensure_loaded?(Evision.Wx.Server), do: [Evision.Wx.Server], else: []
    Supervisor.start_link([Evision.HighGui.Runner | wx], strategy: :one_for_one)
  end
end
