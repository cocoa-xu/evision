defmodule Evision.BackendLinalgSafetyTest do
  use ExUnit.Case, async: true

  defp doubles(values, shape) do
    binary = for value <- values, into: <<>>, do: <<value::float-native-64>>

    %{ref: ref} =
      :evision_nif.mat_from_binary_by_shape(binary: binary, shape: shape, t: "f", l: 64)

    ref
  end

  test "factorizations check dimensions against the actual matrix" do
    a = doubles([4, 0, 0, 9], [2, 2])

    for size <- [-1, 0, 3, 65_536] do
      assert {:error, _} = :evision_nif.mat_cholesky(a: a, n: size)
      assert {:error, _} = :evision_nif.mat_lu(a: a, n: size)
    end

    assert {:error, _} = :evision_nif.mat_qr(a: a, m: 3, n: 2, complete: 0)
    assert {:error, _} = :evision_nif.mat_qr(a: a, m: 2, n: 3, complete: 0)
    %{ref: out} = :evision_nif.mat_cholesky(a: a, n: 2)

    assert :evision_nif.mat_to_binary(img: out) ==
             <<2::float-native-64, 0::float-native-64, 0::float-native-64, 3::float-native-64>>
  end

  test "typed factorizations reject byte matrices" do
    %{ref: ref} =
      :evision_nif.mat_from_binary_by_shape(binary: <<1, 0, 0, 1>>, shape: [2, 2], t: "u", l: 8)

    assert {:error, _} = :evision_nif.mat_cholesky(a: ref, n: 2)
    assert {:error, _} = :evision_nif.mat_lu(a: ref, n: 2)
    assert {:error, _} = :evision_nif.mat_qr(a: ref, m: 2, n: 2, complete: 1)
  end

  test "triangular solve validates the side-dependent right-hand shape" do
    a = doubles([2, 0, 0, 3], [2, 2])
    short = doubles([1], [1, 1])

    for side <- [0, 1] do
      assert {:error, _} =
               :evision_nif.mat_triangular_solve(
                 a: a,
                 b: short,
                 n: 2,
                 lower: 1,
                 left_side: side,
                 transpose: 0
               )
    end

    b = doubles([4, 9], [2, 1])

    %{ref: out} =
      :evision_nif.mat_triangular_solve(a: a, b: b, n: 2, lower: 1, left_side: 1, transpose: 0)

    assert :evision_nif.mat_to_binary(img: out) == <<2::float-native-64, 3::float-native-64>>
  end
end
