defmodule Evision.Zoo.ImageClassification do
  @moduledoc """
  Image classfication model collection.
  """


  @modules %{
    "pp_resnet" => Evision.Zoo.ImageClassification.PPResNet,
    "pp_resnet_quant" => Evision.Zoo.ImageClassification.PPResNet,
    "mobilenet_v1" => Evision.Zoo.ImageClassification.MobileNetV1,
    "mobilenet_v1_quant" => Evision.Zoo.ImageClassification.MobileNetV1,
    "mobilenet_v2" => Evision.Zoo.ImageClassification.MobileNetV2,
    "mobilenet_v2_quant" => Evision.Zoo.ImageClassification.MobileNetV2,
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

  @doc """
  Generate quoted code from smart cell attrs.
  """
  @spec to_quoted(map()) :: list()
  def to_quoted(%{"task_id" => "image_classification", "variant_id" => variant_id} = attrs) do
    module = Map.get(modules(), variant_id)
    if module do
      module.to_quoted(attrs)
    else
      []
    end
  end
end
