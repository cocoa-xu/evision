defmodule Evision.NewTypesTest do
  use ExUnit.Case

  # OpenCV 5.0 adds native CV_64S/CV_32U/CV_64U, so these round-trip losslessly
  # instead of being downcast to s32 (which truncated values > 2^31). Closes #48.

  test "s64 round-trips losslessly through cv::Mat" do
    big = 5_000_000_000
    bin = <<big::native-signed-64, -big::native-signed-64>>
    mat = Evision.Mat.from_binary_by_shape(bin, {:s, 64}, {2})
    assert Evision.Mat.type(mat) == {:s, 64}
    assert Evision.Mat.to_binary(mat) == bin
  end

  test "u32 round-trips losslessly through cv::Mat" do
    bin = <<4_000_000_000::native-unsigned-32, 1::native-unsigned-32>>
    mat = Evision.Mat.from_binary_by_shape(bin, {:u, 32}, {2})
    assert Evision.Mat.type(mat) == {:u, 32}
    assert Evision.Mat.to_binary(mat) == bin
  end

  test "u64 round-trips losslessly through cv::Mat" do
    bin = <<10_000_000_000::native-unsigned-64, 0::native-unsigned-64>>
    mat = Evision.Mat.from_binary_by_shape(bin, {:u, 64}, {2})
    assert Evision.Mat.type(mat) == {:u, 64}
    assert Evision.Mat.to_binary(mat) == bin
  end

  test "bf16 round-trips through cv::Mat" do
    # bf16 bit patterns for 1.0 (0x3F80) and 2.0 (0x4000)
    bin = <<0x3F80::native-unsigned-16, 0x4000::native-unsigned-16>>
    mat = Evision.Mat.from_binary_by_shape(bin, {:bf, 16}, {2})
    assert Evision.Mat.type(mat) == {:bf, 16}
    assert Evision.Mat.to_binary(mat) == bin
  end

  test "Mat.at returns full-width 64-bit values (no truncation)" do
    big = 5_000_000_000
    mat = Evision.Mat.from_binary_by_shape(<<big::native-signed-64>>, {:s, 64}, {1})
    assert Evision.Mat.at(mat, 0) == big
  end
end
