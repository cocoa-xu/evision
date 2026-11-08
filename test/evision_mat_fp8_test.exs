defmodule Evision.Mat.FP8.Test do
  use ExUnit.Case, async: true

  @all_bytes for byte <- 0..255, into: <<>>, do: <<byte>>

  test "CV_8F_E4M3FN maps to Nx's {:f8_e4m3fn, 8}" do
    for type <- [{:f8_e4m3fn, 8}, :f8_e4m3fn] do
      mat = Evision.Mat.from_binary_by_shape(@all_bytes, type, {16, 16})
      assert mat.type == {:f8_e4m3fn, 8}
      assert Evision.Mat.type(mat.ref) == {:f8_e4m3fn, 8}
      assert Evision.Mat.depth(mat) == Evision.Constant.cv_8F_E4M3FN()
      assert Evision.Mat.to_binary(mat) == @all_bytes
    end
  end

  test "all 256 encodings decode like Nx" do
    mat = Evision.Mat.from_binary_by_shape(@all_bytes, :f8_e4m3fn, {256})
    decoded = mat |> Evision.Mat.as_type(:f32) |> Evision.Mat.to_nx(Nx.BinaryBackend)
    expected = @all_bytes |> Nx.from_binary(:f8_e4m3fn) |> Nx.as_type(:f32)

    assert Nx.to_list(decoded) == Nx.to_list(expected)
  end

  test "Nx tensors round-trip through Mat on both backends" do
    mat = Evision.Mat.from_binary_by_shape(@all_bytes, :f8_e4m3fn, {256})

    for backend <- [Nx.BinaryBackend, Evision.Backend] do
      tensor = Evision.Mat.to_nx(mat, backend)
      assert Nx.type(tensor) == {:f8_e4m3fn, 8}
      assert Nx.to_binary(tensor) == @all_bytes
      assert Evision.Mat.to_binary(Evision.Mat.from_nx(tensor)) == @all_bytes
    end
  end

  test "f32 narrows with OpenCV's round-to-nearest-even and saturation" do
    narrowed =
      [1.0, 1.0625, 1.9375, 448.0, 500.0, -0.0]
      |> Evision.Mat.literal(:f32)
      |> Evision.Mat.as_type(:f8_e4m3fn)
      |> Evision.Mat.to_binary()

    assert narrowed == <<0x38, 0x38, 0x40, 0x7E, 0x7E, 0x80>>
  end

  test "Mat.at decodes FP8 elements" do
    mat = Evision.Mat.from_binary_by_shape(<<0x38, 0x40, 0x81, 0x7E>>, :f8_e4m3fn, {4})
    assert Enum.map(0..3, &Evision.Mat.at(mat, &1)) == [1.0, 2.0, -0.001953125, 448.0]
  end
end
