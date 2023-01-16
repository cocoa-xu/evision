defmodule Evision.KeyPoint.Test do
  use ExUnit.Case

  test "Keypoints" do
    kp = Evision.KeyPoint.convert([{23.3, 23.3}])
    assert Enum.count(kp) == 1
  end
end
