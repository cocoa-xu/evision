defmodule Evision.HighGui.ConvertToShow.Test do
  use ExUnit.Case, async: true

  alias Evision.HighGui
  alias Evision.Mat

  defp show(mat) do
    %Mat{dims: 2, channels: 3, type: {:u, 8}} = rgb = HighGui.convertToShow(mat)
    Mat.to_binary(rgb)
  end

  test "converts GRAY, BGR and BGRA to RGB" do
    assert <<3, 2, 1, 6, 5, 4>> = show(Mat.from_binary(<<1, 2, 3, 4, 5, 6>>, {:u, 8}, 1, 2, 3))
    assert <<7, 7, 7, 9, 9, 9>> = show(Mat.from_binary(<<7, 9>>, {:u, 8}, 1, 2, 1))
    assert <<3, 2, 1>> = show(Mat.from_binary(<<1, 2, 3, 255>>, {:u, 8}, 1, 1, 4))
  end

  test "scales other depths like imshow" do
    f32 = <<0.5::float-32-native, -1.0::float-32-native, 2.0::float-32-native>>
    assert <<255, 0, 128>> = show(Mat.from_binary(f32, {:f, 32}, 1, 1, 3))

    f64 = <<0.5::float-64-native, -1.0::float-64-native, 2.0::float-64-native>>
    assert <<255, 0, 128>> = show(Mat.from_binary(f64, {:f, 64}, 1, 1, 3))

    for type <- [:f16, :bf16] do
      assert <<255, 0, 128>> = show(Nx.tensor([[[0.5, 0.0, 1.0]]], type: type))
    end

    u16 = <<25500::16-native, 0::16-native, 255::16-native>>
    assert <<1, 0, 100>> = show(Mat.from_binary(u16, {:u, 16}, 1, 1, 3))

    s16 = <<-25500::signed-16-native, 0::signed-16-native, 25500::signed-16-native>>
    assert <<227, 127, 27>> = show(Mat.from_binary(s16, {:s, 16}, 1, 1, 3))

    s8 = <<-127::signed-8, 0::signed-8, 100::signed-8>>
    assert <<227, 127, 0>> = show(Mat.from_binary(s8, {:s, 8}, 1, 1, 3))
  end

  test "accepts a tensor and a 3-D Mat" do
    assert <<3, 2, 1>> = show(Nx.tensor([[[1, 2, 3]]], type: :u8))
    assert <<3, 2, 1>> = show(Mat.from_binary_by_shape(<<1, 2, 3>>, {:u, 8}, {1, 1, 3}))
  end

  test "rejects images it cannot show" do
    assert {:error, _} = HighGui.convertToShow(Mat.from_binary(<<1, 2>>, {:u, 8}, 1, 1, 2))

    assert {:error, _} =
             HighGui.convertToShow(Mat.from_binary(<<1::32-native>>, {:s, 32}, 1, 1, 1))
  end
end
