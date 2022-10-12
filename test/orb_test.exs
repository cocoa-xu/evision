defmodule Evision.ORB.Test do
  use ExUnit.Case

  @moduletag timeout: 120_000

  test "detect keypoints in an image" do
    img = %Evision.Mat{} = Evision.imread(Path.join(__DIR__, "pca_test.jpg"), flags: 0)
    orb = %Evision.ORB{} = Evision.ORB.create()
    kp = Evision.ORB.detect(orb, img)
    {kp, _des} = Evision.ORB.compute(orb, img, kp)
    assert Enum.count(kp) != 0
  end
end
