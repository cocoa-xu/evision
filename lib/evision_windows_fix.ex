defmodule :evision_windows_fix do
  @moduledoc false
  @on_load :__on_load__
  def __on_load__ do
    case :os.type() do
      {:win32, _} ->
        path = :filename.join(:code.priv_dir(:evision), ~c"windows_fix")
        :erlang.load_nif(path, 0)
        run_once()

      _ ->
        :ok
    end
  end

  def run_once do
    case :os.type() do
      {:win32, _} ->
        :erlang.nif_error(:undefined)

      _ ->
        :ok
    end
  end
end
