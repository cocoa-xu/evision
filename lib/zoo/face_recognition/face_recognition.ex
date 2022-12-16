defmodule Evision.Zoo.FaceRecognition do
  @moduledoc """
  Face recognition model collection.
  """

  @modules %{
    "sface" => Evision.Zoo.FaceRecognition.SFace,
    "sface_quant" => Evision.Zoo.FaceRecognition.SFace,
  }
  @module_list Enum.uniq(Map.values(@modules))

  def modules, do: @modules
  def module_list, do: @module_list

  def id, do: "face_recognition"

  def label, do: "Face recognition"

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
  def to_quoted(%{"task_id" => "face_recognition", "variant_id" => variant_id} = attrs) do
    module = Map.get(modules(), variant_id)
    if module do
      module.to_quoted(attrs)
    else
      []
    end
  end
end
