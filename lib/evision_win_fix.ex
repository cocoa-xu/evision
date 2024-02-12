defmodule :evision_windows_fix do
  @moduledoc false
  @on_load :fix_windows
  def fix_windows do
    require Logger
    nif_file = "#{:code.priv_dir(:evision)}/windows_fix"
    case :erlang.load_nif(nif_file, 0) do
      :ok -> :ok
      {:error, {:reload, _}} -> :ok
      {:error, reason} ->
        Logger.warning("Failed to load nif: #{inspect(reason)}")
        {:error, reason}
    end
  end

  def run_once, do: :erlang.nif_error(":evision_windows_fix.run_once/0 not loaded")
end
