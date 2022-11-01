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
      Evision.SmartCell.ML.RTrees
    ]
  end

  @compile {:no_warn_undefined, Kino.SmartCell}

  @doc """
  Register Smartcells

  It will register all available smartcells by default.

  To see all available smartcells, please use `Evision.SmartCell.available_smartcells/0`.
  """
  @spec register_smartcells([module()]) :: :ok
  def register_smartcells(smartcells \\ available_smartcells()) when is_list(smartcells) do
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
      """
    end
  end
end
