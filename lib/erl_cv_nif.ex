defmodule :erl_cv_nif do
  @moduledoc false
  @on_load :load_nif
  def load_nif do
    require Logger
    nif_file = '#{:code.priv_dir(:evision)}/evision'

    case :erlang.load_nif(nif_file, 0) do
      :ok -> :ok
      {:error, {:reload, _}} -> :ok
      {:error, reason} -> Logger.warn("Failed to load nif: #{inspect(reason)}")
    end
  end
end
