defmodule Evision.Zoo.TextRecognition do
  @moduledoc """
  Text recognition model collection.
  """

  @modules %{
    "crnn_en" => Evision.Zoo.TextRecognition.CRNN,
    "crnn_en_fp16" => Evision.Zoo.TextRecognition.CRNN,
    "crnn_en_int8" => Evision.Zoo.TextRecognition.CRNN,
    "crnn_ch" => Evision.Zoo.TextRecognition.CRNN,
    "crnn_ch_fp16" => Evision.Zoo.TextRecognition.CRNN,
    "crnn_ch_int8" => Evision.Zoo.TextRecognition.CRNN,
    "crnn_cn" => Evision.Zoo.TextRecognition.CRNN,
    "crnn_cn_int8" => Evision.Zoo.TextRecognition.CRNN,
  }
  @module_list Enum.uniq(Map.values(@modules))

  def modules, do: @modules
  def module_list, do: @module_list

  def id, do: "text_recognition"

  def label, do: "Text recognition"

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
  def to_quoted(%{"task_id" => "text_recognition", "variant_id" => variant_id} = attrs) do
    module = Map.get(modules(), variant_id)
    if module do
      module.to_quoted(attrs)
    else
      []
    end
  end
end
