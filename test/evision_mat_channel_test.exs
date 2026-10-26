defmodule Evision.MatChannelTest do
  use ExUnit.Case, async: true

  @types [
    {:u, 8},
    {:s, 8},
    {:u, 16},
    {:s, 16},
    {:u, 32},
    {:s, 32},
    {:u, 64},
    {:s, 64},
    {:f, 16},
    {:bf, 16},
    {:f, 32},
    {:f, 64}
  ]

  defp bytes(values, bits) do
    for value <- values, into: <<>>, do: <<value::unsigned-native-size(bits)>>
  end

  test "channel reinterpretation preserves every supported type and its bytes" do
    for {_, bits} = type <- @types do
      binary = bytes(1..12, bits)
      source = Evision.Mat.from_binary_by_shape(binary, type, {2, 2, 3})

      for input <- [source, Evision.Mat.clone(source)] do
        assert %Evision.Mat{channels: 3, dims: 2, shape: {2, 2, 3}, type: ^type} =
                 image = Evision.Mat.last_dim_as_channel(input)

        assert Evision.Mat.to_binary(image) == binary

        assert %Evision.Mat{channels: 1, dims: 3, shape: {2, 2, 3}, type: ^type} =
                 restored = Evision.Mat.channel_as_last_dim(image)

        assert Evision.Mat.to_binary(restored) == binary
        assert Evision.Mat.to_binary(input) == binary
      end
    end
  end

  test "channel reinterpretation rejects inputs that already have channels" do
    for {_, bits} = type <- @types do
      binary = bytes(1..12, bits)
      image = Evision.Mat.from_binary(binary, type, 2, 2, 3)

      assert {:error, "image already has channel info"} = Evision.Mat.last_dim_as_channel(image)
      assert Evision.Mat.to_binary(image) == binary
    end
  end

  test "ROI updates restore channels for every supported type" do
    for {_, bits} = type <- @types do
      binary = bytes(1..12, bits)
      patch_binary = bytes(21..23, bits)
      image = Evision.Mat.from_binary(binary, type, 2, 2, 3)
      patch = Evision.Mat.from_binary(patch_binary, type, 1, 1, 3)

      assert %Evision.Mat{channels: 3, dims: 2, shape: {2, 2, 3}, type: ^type} =
               result = Evision.Mat.update_roi(image, [{1, 2}, {0, 1}], patch)

      assert Evision.Mat.to_binary(result) ==
               bytes([1, 2, 3, 4, 5, 6, 21, 22, 23, 10, 11, 12], bits)

      assert Evision.Mat.to_binary(image) == binary
      assert Evision.Mat.to_binary(patch) == patch_binary
    end
  end
end
