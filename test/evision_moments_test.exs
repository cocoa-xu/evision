defmodule Evision.MomentsTest do
  use ExUnit.Case, async: true

  test "moments returns raw, central, and normalized values" do
    image = Evision.Mat.ones({2, 2}, :u8)

    assert Evision.moments(image) == %{
             m00: 4.0,
             m10: 2.0,
             m01: 2.0,
             m20: 2.0,
             m11: 1.0,
             m02: 2.0,
             m30: 2.0,
             m21: 1.0,
             m12: 1.0,
             m03: 2.0,
             mu20: 1.0,
             mu11: 0.0,
             mu02: 1.0,
             mu30: 0.0,
             mu21: 0.0,
             mu12: 0.0,
             mu03: 0.0,
             nu20: 0.0625,
             nu11: 0.0,
             nu02: 0.0625,
             nu30: 0.0,
             nu21: 0.0,
             nu12: 0.0,
             nu03: 0.0
           }
  end

  test "binary image moments count nonzero pixels" do
    image = Evision.Mat.from_binary_by_shape(<<0, 1, 2, 3>>, :u8, {2, 2})

    assert %{m00: 6.0, m10: 4.0, m01: 5.0} = Evision.moments(image)
    assert %{m00: 3.0, m10: 2.0, m01: 2.0} = Evision.moments(image, binaryImage: true)
    assert Evision.Mat.to_binary(image) == <<0, 1, 2, 3>>
  end
end
