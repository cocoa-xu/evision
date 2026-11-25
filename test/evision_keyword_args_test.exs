defmodule Evision.KeywordArgs.Test do
  use ExUnit.Case, async: true

  test "all-keyword calls take the keys the positional form passes" do
    img = Evision.Mat.literal([[0, 50, 100], [150, 200, 250], [25, 75, 125]], :u8)
    method = Evision.Constant.cv_ADAPTIVE_THRESH_MEAN_C()
    type = Evision.Constant.cv_THRESH_BINARY()

    expected = Evision.adaptiveThreshold(img, 255.0, method, type, 3, 10.0)

    got =
      Evision.adaptiveThreshold(
        src: img,
        maxValue: 255.0,
        adaptiveMethod: method,
        thresholdType: type,
        blockSize: 3,
        c: 10.0
      )

    assert Evision.Mat.to_binary(got) == Evision.Mat.to_binary(expected)
  end
end
