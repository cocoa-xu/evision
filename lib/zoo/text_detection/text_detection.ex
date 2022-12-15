defmodule Evision.Zoo.TextDetection do
  @modules %{
    "db_ic15_resnet18" => Evision.Zoo.TextDetection.DB_IC15,
    "db_ic15_resnet50" => Evision.Zoo.TextDetection.DB_IC15,
    # "db_td500" => Evision.Zoo.FaceRecognition.DB_TD500,
    # "db_td500_quant" => Evision.Zoo.FaceRecognition.DB_TD500,
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

  def to_quoted(%{"task_id" => "text_detection", "variant_id" => variant_id} = attrs) do
    module = Map.get(modules(), variant_id)
    if module do
      module.to_quoted(attrs)
    else
      []
    end
  end
end
