defmodule Evision.NativeConversionSafetyTest do
  use ExUnit.Case, async: true

  defp mat(binary, shape, t \\ "u", bits \\ 8) do
    %{ref: ref} =
      :evision_nif.mat_from_binary_by_shape(binary: binary, shape: shape, t: t, l: bits)

    ref
  end

  test "forged tensor maps cannot read beyond their binary" do
    for {shape, type, data} <- [
          {{4096}, :u8, <<1>>},
          {{1}, :f64, <<1>>},
          {{}, :f64, <<>>},
          {{0}, :u8, <<>>},
          {{-1}, :u8, <<1>>},
          {{4_294_967_297}, :u8, <<1>>},
          {{2_147_483_647, 2_147_483_647, 8}, :f64, <<0::64>>}
        ] do
      forged = %{__struct__: :nx_tensor, shape: shape, type: type, data: data}
      assert_raise ArgumentError, fn -> :evision_nif.mat_clone(img: forged) end
    end
  end

  test "valid tensor maps and scalars retain their bytes" do
    for shape <- [{4}, {2, 2}] do
      tensor = %{__struct__: :nx_tensor, shape: shape, type: :u8, data: <<1, 2, 3, 4>>}
      %{ref: ref} = :evision_nif.mat_clone(img: tensor)
      assert :evision_nif.mat_to_binary(img: ref) == <<1, 2, 3, 4>>
    end

    ref = mat(<<7>>, [])
    assert :evision_nif.mat_to_binary(img: ref) == <<7>>
  end

  test "shape reinterpretation must preserve the actual byte extent" do
    ref = mat(<<1, 2, 3, 4>>, [4])

    for shape <- [
          [4096],
          [2, 3],
          [0],
          [-1],
          List.duplicate(1, 33),
          [2_147_483_647, 2_147_483_647, 8]
        ] do
      assert {:error, _} = :evision_nif.mat_as_shape(img: ref, as_shape: shape)
    end

    %{ref: reshaped} = :evision_nif.mat_as_shape(img: ref, as_shape: [2, 2])
    assert :evision_nif.mat_to_binary(img: reshaped) == <<1, 2, 3, 4>>
  end

  test "broadcast rejects a forged source shape" do
    ref = mat(<<1, 2, 3, 4>>, [4])

    assert {:error, _} =
             :evision_nif.mat_broadcast_to(img: ref, force_src_shape: [4096], to_shape: [4096])

    assert {:error, _} =
             :evision_nif.mat_broadcast_to(img: ref, force_src_shape: [4], to_shape: [0, 4])

    %{ref: out} =
      :evision_nif.mat_broadcast_to(img: ref, force_src_shape: [1, 4], to_shape: [2, 4])

    assert :evision_nif.mat_to_binary(img: out) == <<1, 2, 3, 4, 1, 2, 3, 4>>
  end

  test "transpose rejects overflowing byte counts and non-permutations" do
    ref = mat(<<1::float-native-64>>, [1], "f", 64)

    assert {:error, _} =
             :evision_nif.mat_transpose(
               img: ref,
               as_shape: [2_147_483_647, 2_147_483_647, 8],
               axes: [0, 1, 2],
               as_shaped: true
             )

    for axes <- [[0, 0], [-1, 0], [0, 2]] do
      assert {:error, _} =
               :evision_nif.mat_transpose(img: ref, as_shape: [1, 1], axes: axes, as_shaped: true)
    end
  end

  test "batch size cannot narrow or wrap before allocation" do
    ref = mat(<<1, 2>>, [2])

    for size <- [0, 2_147_483_648, 4_294_967_297, 18_446_744_073_709_551_615] do
      assert {:error, _} =
               :evision_nif.mat_to_batched(
                 img: ref,
                 as_shape: [2],
                 batch_size: size,
                 leftover: "repeat"
               )
    end

    assert [%{ref: out}] =
             :evision_nif.mat_to_batched(
               img: ref,
               as_shape: [2],
               batch_size: 5,
               leftover: "repeat"
             )

    assert :evision_nif.mat_to_binary(img: out) == <<1, 2, 1, 2, 1>>
  end

  test "binary constructor rejects unsupported ranks" do
    assert {:error, _} =
             :evision_nif.mat_from_binary_by_shape(
               binary: <<1>>,
               shape: List.duplicate(1, 33),
               t: "u",
               l: 8
             )
  end

  test "allocating constructors reject negative dimensions and overflow" do
    for shape <- [[-1, 2], [0], List.duplicate(1, 33), [2_147_483_647, 2_147_483_647, 8]] do
      for function <- [:mat_zeros, :mat_ones, :mat_full] do
        assert {:error, _} =
                 apply(:evision_nif, function, [[shape: shape, t: "u", l: 8, number: 1.0]])
      end
    end

    assert {:error, _} = :evision_nif.mat_eye(n: 4_294_967_297, t: "u", l: 8)
  end

  test "arange does not overflow after emitting its final element" do
    for {from, to, step} <- [
          {2_147_483_646, 2_147_483_647, 2},
          {-2_147_483_647, -2_147_483_648, -2}
        ] do
      %{ref: out} = :evision_nif.mat_arange(from: from, to: to, step: step, t: "s", l: 32)
      assert :evision_nif.mat_to_binary(img: out) == <<from::signed-native-32>>
    end
  end
end
