defmodule Evision.Mat.Test do
  use ExUnit.Case

  test "Keypoints" do
    kp = Evision.KeyPoint.convert([{23.3, 23.3}])
    IO.inspect(kp)
  end
end
