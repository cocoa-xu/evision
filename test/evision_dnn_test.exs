defmodule Evision.DNN.Test do
  use ExUnit.Case

  test "nmsBoxes/{4,5}" do
    assert [0] == Evision.DNN.nmsBoxes([{0,1,2,3}], [1], 0.4, 0.3)
    assert [0] == Evision.DNN.nmsBoxes(Nx.tensor([[0,1,2,3]]), [1], 0.4, 0.3)
    assert [0] == Evision.DNN.nmsBoxes(Evision.Mat.literal([[0,1,2,3]], :f64), [1], 0.4, 0.3)
  end

  test "nmsBoxesBatched/{5,6}" do
    assert [0] == Evision.DNN.nmsBoxesBatched([{0,1,2,3}], [1], [1], 0.4, 0.3)
    assert [0] == Evision.DNN.nmsBoxesBatched(Nx.tensor([[0,1,2,3]]), [1], [1], 0.4, 0.3)
    assert [0] == Evision.DNN.nmsBoxesBatched(Evision.Mat.literal([[0,1,2,3]], :f64), [1], [1], 0.4, 0.3)
  end

  test "softNMSBoxes/{4,5}" do
    assert {[1.0], [0]} == Evision.DNN.softNMSBoxes([{0,1,2,3}], [1], [1], 0.4, 0.3)
    assert {[1.0], [0]} == Evision.DNN.softNMSBoxes(Nx.tensor([[0,1,2,3]]), [1], [1], 0.4, 0.3)
    assert {[1.0], [0]} == Evision.DNN.softNMSBoxes(Evision.Mat.literal([[0,1,2,3]], :f64), [1], [1], 0.4, 0.3)
  end
end
