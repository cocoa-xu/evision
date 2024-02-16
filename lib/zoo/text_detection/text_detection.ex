defmodule Evision.Zoo.TextDetection do
  @moduledoc """
  Text detection model collection.
  """

  @modules %{
    "db_ic15_resnet18" => Evision.Zoo.TextDetection.DB,
    "db_ic15_resnet50" => Evision.Zoo.TextDetection.DB,
    "db_td500_resnet18" => Evision.Zoo.TextDetection.DB,
    "db_td500_resnet50" => Evision.Zoo.TextDetection.DB,
    "ppocrv3_en" => Evision.Zoo.TextDetection.PPOCRV3,
    "ppocrv3_en_int8" => Evision.Zoo.TextDetection.PPOCRV3,
    "ppocrv3_cn" => Evision.Zoo.TextDetection.PPOCRV3,
    "ppocrv3_cn_int8" => Evision.Zoo.TextDetection.PPOCRV3
  }
  @module_list Enum.uniq(Map.values(@modules))

  def modules, do: @modules
  def module_list, do: @module_list

  def id, do: "text_detection"

  def label, do: "Text detection"

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
  def to_quoted(%{"task_id" => "text_detection", "variant_id" => variant_id} = attrs) do
    module = Map.get(modules(), variant_id)
    if module do
      module.to_quoted(attrs)
    else
      []
    end
  end
end
