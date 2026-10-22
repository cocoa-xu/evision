defmodule Evision.BackendArithmeticSafetyTest do
  use ExUnit.Case, async: true

  defp mat(binary, t, bits) do
    %{ref: ref} =
      :evision_nif.mat_from_binary_by_shape(
        binary: binary,
        shape: [1, div(byte_size(binary), div(bits, 8))],
        t: t,
        l: bits
      )

    ref
  end

  test "binary kernels reject mismatched sizes and types" do
    a = mat(<<1, 2, 3>>, "u", 8)

    for b <- [mat(<<1, 2>>, "u", 8), mat(<<1::signed-native-64>>, "s", 64)] do
      assert {:error, _} = :evision_nif.mat_binop(l: a, r: b, op: 1)
      assert {:error, _} = :evision_nif.mat_shift(l: a, r: b, op: 0)
    end
  end

  test "signed minimum quotient and remainder cannot trap the VM" do
    for bits <- [32, 64] do
      minimum = -Integer.pow(2, bits - 1)
      a = mat(<<minimum::signed-native-size(bits)>>, "s", bits)
      b = mat(<<-1::signed-native-size(bits)>>, "s", bits)
      %{ref: quotient} = :evision_nif.mat_binop(l: a, r: b, op: 2)
      %{ref: remainder} = :evision_nif.mat_binop(l: a, r: b, op: 3)
      assert :evision_nif.mat_to_binary(img: quotient) == <<minimum::signed-native-size(bits)>>
      assert :evision_nif.mat_to_binary(img: remainder) == <<0::signed-native-size(bits)>>
    end
  end

  test "u16 powers wrap without signed promotion overflow" do
    a = mat(<<65_535::unsigned-native-16>>, "u", 16)
    b = mat(<<2::unsigned-native-16>>, "u", 16)
    %{ref: out} = :evision_nif.mat_binop(l: a, r: b, op: 1)
    assert :evision_nif.mat_to_binary(img: out) == <<1::unsigned-native-16>>
  end

  test "signed reductions wrap in both row and column kernels" do
    maximum = 9_223_372_036_854_775_807

    for {values, op, expected} <- [
          {[maximum, 1], 0, -9_223_372_036_854_775_808},
          {[maximum, 2], 1, -2}
        ],
        {shape, axis} <- [{[1, 2], 1}, {[2, 1], 0}] do
      binary = for value <- values, into: <<>>, do: <<value::signed-native-64>>

      %{ref: src} =
        :evision_nif.mat_from_binary_by_shape(binary: binary, shape: shape, t: "s", l: 64)

      %{ref: out} = :evision_nif.mat_reduce(src: src, op: op, axis: axis)
      assert :evision_nif.mat_to_binary(img: out) == <<expected::signed-native-64>>
    end
  end

  test "integer windows wrap in serial and parallel kernels" do
    for {t, bits, values, op, expected} <- [
          {"s", 32, [2_147_483_647, 1], 0, -2_147_483_648},
          {"s", 64, [9_223_372_036_854_775_807, 1], 0, -9_223_372_036_854_775_808},
          {"s", 64, [9_223_372_036_854_775_807, 2], 1, -2},
          {"u", 16, [65_535, 65_535], 1, 1}
        ],
        repeats <- [1, 131_072] do
      pair = for value <- values, into: <<>>, do: <<value::native-size(bits)>>
      src = mat(:binary.copy(pair, repeats), t, bits)

      %{ref: out} =
        :evision_nif.mat_window_reduce(
          src: src,
          in_dims: [repeats * 2],
          out_dims: [repeats],
          win_dims: [2],
          strides: [2],
          pad_lo: [0],
          dilations: [1],
          op: op
        )

      assert :evision_nif.mat_to_binary(img: out) ==
               :binary.copy(<<expected::native-size(bits)>>, repeats)
    end
  end

  test "window scatter wraps duplicate integer updates" do
    src = mat(<<0::signed-native-64, 1::signed-native-64, 0::signed-native-64>>, "s", 64)
    updates = mat(<<9_223_372_036_854_775_807::signed-native-64, 1::signed-native-64>>, "s", 64)

    opts = [
      src: src,
      source: updates,
      in_dims: [3],
      win_dims: [2],
      strides: [1],
      pad_lo: [0],
      grid: [2],
      init: 0.0,
      op: 0
    ]

    %{ref: out} = :evision_nif.mat_window_scatter(opts)

    assert :evision_nif.mat_to_binary(img: out) ==
             <<0::signed-native-64, -9_223_372_036_854_775_808::signed-native-64,
               0::signed-native-64>>
  end

  test "window scatter rejects initial values outside the integer range" do
    for t <- ["s", "u"], bits <- [8, 16, 32, 64] do
      src = mat(<<0::native-size(bits)>>, t, bits)
      limit = Integer.pow(2, bits - if(t == "s", do: 1, else: 0)) * 1.0

      opts = [
        src: src,
        source: src,
        in_dims: [1],
        win_dims: [1],
        strides: [1],
        pad_lo: [0],
        grid: [1],
        op: 0
      ]

      for value <- [limit, -limit * 2, 1.0e300, -1.0e300] do
        assert {:error, _} = :evision_nif.mat_window_scatter(Keyword.put(opts, :init, value))
      end

      %{ref: out} = :evision_nif.mat_window_scatter(Keyword.put(opts, :init, 1.5))
      assert :evision_nif.mat_to_binary(img: out) == <<1::native-size(bits)>>
    end
  end

  test "row kernels reject multi-channel matrices before exposing unwritten bytes" do
    %{ref: ref} =
      :evision_nif.mat_from_binary(
        binary: <<1::float-native-32, 2::float-native-32>>,
        rows: 1,
        cols: 1,
        channels: 2,
        t: "f",
        l: 32
      )

    assert {:error, _} = :evision_nif.mat_sort_rows(src: ref, descending: 0)
    assert {:error, _} = :evision_nif.mat_argsort_rows(src: ref, descending: 0)
    assert {:error, _} = :evision_nif.mat_cumulative(src: ref, op: 0, reverse: 0)
    assert {:error, _} = :evision_nif.mat_reduce(src: ref, op: 0, axis: 1)
    assert {:error, _} = :evision_nif.mat_predicate(src: ref, op: 0)
  end
end
