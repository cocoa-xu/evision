defmodule Evision.SmartCell do
  @moduledoc """
  Evision SmartCell Collection

  To use smartcell in Livebook, `:kino >= 0.7` is required

  ```elixir
  defp deps do
    [
      # ...
      {:kino, "~> 0.7"},
      # ...
    ]
  end
  ```
  """

  @doc """
  Get all available smartcells.

  To register smartcells to `:kino`, please use `Evision.SmartCell.register_smartcells/1`.
  """
  @spec available_smartcells :: [module()]
  def available_smartcells do
    [
      Evision.SmartCell.ML.TrainData,
      Evision.SmartCell.ML.SVM,
      Evision.SmartCell.ML.DTrees,
      Evision.SmartCell.ML.RTrees,
      Evision.SmartCell.Zoo
    ]
  end

  @compile {:no_warn_undefined, Kino.SmartCell}

  @doc """
  Register Smartcells

  It will register all available smartcells by default.

  To see all available smartcells, please use `Evision.SmartCell.available_smartcells/0`.
  """
  @spec register_smartcells([module()] | module()) :: :ok
  def register_smartcells(smartcells \\ available_smartcells())

  def register_smartcells(smartcells) when is_list(smartcells) do
    if Code.ensure_loaded?(Kino.SmartCell) do
      Enum.each(smartcells, fn sc ->
        if Code.ensure_loaded?(sc) do
          Kino.SmartCell.register(sc)
        else
          raise RuntimeError, "Cannot register smartcell: #{inspect(sc)}"
        end
      end)
    else
      raise RuntimeError, """
      `:kino >= 0.7` is required to use smartcells in Livebook.
      Please add `{:kino, "~> 0.7"}` to the dependency list.

      Note that `Evision.SmartCell.Zoo` has a another optional dependency,
      `{:progress_bar,  "~> 2.0}`, which is used for displaying the
      model download progress.
      """
    end
  end

  def register_smartcells(smartcell) when is_atom(smartcell) do
    register_smartcells([smartcell])
  end
end
