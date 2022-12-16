defmodule Evision.Zoo.ImageSegmentation do
  @moduledoc """
  Image segmentation model collection.
  """

  import Bitwise

  @modules %{
    "pp_humanseg" => Evision.Zoo.ImageSegmentation.PPHumanSeg,
    "pp_humanseg_quant" => Evision.Zoo.ImageSegmentation.PPHumanSeg,
  }
  @module_list Enum.uniq(Map.values(@modules))

  def modules, do: @modules
  def module_list, do: @module_list

  def id, do: "image_segmentation"

  def label, do: "Image Segmentation"

  def smartcell_tasks do
    %{
      id: id(),
      label: label(),
      variants: Enum.reduce(module_list(), [], fn module, acc -> module.smartcell_tasks() ++ acc end)
    }
  end

  @doc """
  Generate quoted code from smart cell attrs.
  """
  @spec to_quoted(map()) :: list()
  def to_quoted(%{"task_id" => "image_segmentation", "variant_id" => variant_id} = attrs) do
    module = Map.get(modules(), variant_id)
    if module do
      module.to_quoted(attrs)
    else
      []
    end
  end

  def color_map(num_classes) do
    color_map =
      for i <- 0..num_classes, reduce: [] do
        color_map ->
          colors = gen_color_map(i, i, 0, [0, 0, 0])
          [colors | color_map]
      end
    tl(Enum.reverse(color_map))
  end

  defp gen_color_map(0, _, _, colors), do: colors
  defp gen_color_map(lab, i, j, [c1, c2, c3]) do
    c1 = bor(c1, bsl(band(bsr(lab, 0), 1), 7 - j))
    c2 = bor(c2, bsl(band(bsr(lab, 1), 1), 7 - j))
    c3 = bor(c3, bsl(band(bsr(lab, 2), 1), 7 - j))

    gen_color_map(lab >>> 3, i, j + 1, [c1, c2, c3])
  end
end
