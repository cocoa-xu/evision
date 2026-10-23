defmodule Evision.BackendWindowSafetyTest do
  use ExUnit.Case, async: true

  defp floats(values) do
    binary = for value <- values, into: <<>>, do: <<value::float-native-32>>

    %{ref: ref} =
      :evision_nif.mat_from_binary_by_shape(
        binary: binary,
        shape: [length(values)],
        t: "f",
        l: 32
      )

    ref
  end

  test "window reduction validates every vector and the source size" do
    src = floats([1, 2, 3, 4])

    opts = [
      src: src,
      in_dims: [4],
      out_dims: [3],
      win_dims: [2],
      strides: [1],
      pad_lo: [0],
      dilations: [1],
      op: 0
    ]

    for change <- [
          [in_dims: [4096]],
          [strides: []],
          [win_dims: []],
          [out_dims: [0]],
          [dilations: [0]],
          [out_dims: [2_147_483_647, 2]]
        ] do
      assert {:error, _} = :evision_nif.mat_window_reduce(Keyword.merge(opts, change))
    end

    %{ref: out} = :evision_nif.mat_window_reduce(opts)

    assert :evision_nif.mat_to_binary(img: out) ==
             <<3::float-native-32, 5::float-native-32, 7::float-native-32>>
  end

  test "window scatter validates source values before writing the result" do
    src = floats([1, 2, 3, 4])

    opts = [
      src: src,
      source: floats([10, 20, 30]),
      in_dims: [4],
      grid: [3],
      win_dims: [2],
      strides: [1],
      pad_lo: [0],
      init: 0.0,
      op: 0
    ]

    for change <- [
          [in_dims: [4096]],
          [source: floats([1])],
          [grid: []],
          [pad_lo: []],
          [strides: [0]]
        ] do
      assert {:error, _} = :evision_nif.mat_window_scatter(Keyword.merge(opts, change))
    end

    %{ref: out} = :evision_nif.mat_window_scatter(opts)

    assert :evision_nif.mat_to_binary(img: out) ==
             <<0::float-native-32, 10::float-native-32, 20::float-native-32, 30::float-native-32>>
  end

  test "convolution rejects inconsistent shapes and invalid groups" do
    opts = [
      input: floats([1, 2, 3, 4]),
      kernel: floats([1, 1]),
      in_shape: [1, 1, 4],
      k_shape: [1, 1, 2],
      out_shape: [1, 1, 3],
      strides: [1],
      pad_lo: [0],
      input_dilation: [1],
      kernel_dilation: [1],
      feature_groups: 1,
      batch_groups: 1,
      im2row_budget: 0
    ]

    for change <- [
          [in_shape: [1, 1, 4096]],
          [k_shape: []],
          [out_shape: [1, 2, 3]],
          [strides: []],
          [input_dilation: [0]],
          [feature_groups: 0],
          [feature_groups: 2],
          [batch_groups: 0],
          [batch_groups: 2],
          [kernel: floats([1])]
        ] do
      assert {:error, _} = :evision_nif.mat_conv(Keyword.merge(opts, change))
    end

    %{ref: out} = :evision_nif.mat_conv(opts)

    assert :evision_nif.mat_to_binary(img: out) ==
             <<3::float-native-32, 5::float-native-32, 7::float-native-32>>
  end

  test "2D convolution handles wide coordinates without integer overflow" do
    opts = [
      input: floats([2]),
      kernel: floats([3]),
      in_shape: [1, 1, 1, 1],
      k_shape: [1, 1, 1, 1],
      out_shape: [1, 1, 3, 1],
      strides: [2_147_483_647, 1],
      pad_lo: [0, 0],
      input_dilation: [1, 1],
      kernel_dilation: [1, 1],
      feature_groups: 1,
      batch_groups: 1,
      im2row_budget: 64
    ]

    %{ref: out} = :evision_nif.mat_conv(opts)

    assert :evision_nif.mat_to_binary(img: out) ==
             <<6::float-native-32, 0::float-native-32, 0::float-native-32>>
  end
end
