defmodule Evision.NativeDispatchSafetyTest do
  use ExUnit.Case, async: true

  test "exceptions in hand-written operations return errors and leave the VM usable" do
    %{ref: img} = :evision_nif.mat_zeros(shape: [2, 2], t: "u", l: 8)
    %{ref: mask} = :evision_nif.mat_ones(shape: [1, 1], t: "f", l: 64)
    assert {:error, _} = :evision_nif.mat_set_to(img: img, value: 1.0, mask: mask)
    assert %{ref: _} = :evision_nif.mat_zeros(shape: [1, 1], t: "u", l: 8)
  end
end
