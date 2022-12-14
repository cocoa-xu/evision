defmodule Evision.Zoo.ImageClassification do
  @modules %{
    "pp_resnet" => Evision.Zoo.ImageClassification.PPResNet,
    "pp_resnet_quant" => Evision.Zoo.ImageClassification.PPResNet,
  }
  @module_list Enum.uniq(Map.values(@modules))

  def modules, do: @modules
  def module_list, do: @module_list

  def id, do: "image_classification"

  def label, do: "Image Classification"

  def smartcell_tasks do
    %{
      id: id(),
      label: label(),
      variants: Enum.reduce(module_list(), [], fn module, acc -> module.smartcell_tasks() ++ acc end)
    }
  end

  def to_quoted(%{"task_id" => "image_classification", "variant_id" => variant_id} = attrs) do
    module = Map.get(modules(), variant_id)
    if module do
      module.to_quoted(attrs)
    else
      []
    end
  end
end
