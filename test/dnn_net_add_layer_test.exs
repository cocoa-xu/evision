defmodule Evision.DNN.Net.AddLayer.Test do
  use ExUnit.Case

  alias Evision.DNN.Net

  @tag :dnn
  test "addLayer and addLayerToPrev take LayerParams as a map" do
    net = Net.net()
    params = %{"negative_slope" => 0.5}
    assert 1 == Net.addLayerToPrev(net, "leaky", "ReLU", Evision.Constant.cv_32F(), params)

    [output] =
      net
      |> Net.setInput(Evision.Mat.literal([[-2.0, 4.0]], :f32))
      |> Net.forward(outputName: "leaky")

    assert Evision.Mat.to_nx(output, Nx.BinaryBackend) == Nx.tensor([[-1.0, 4.0]], type: :f32)

    assert 2 == Net.addLayer(net, "relu", "ReLU", Evision.Constant.cv_32F(), %{})
    assert ["leaky", "relu"] == Net.getLayerNames(net)
  end
end
