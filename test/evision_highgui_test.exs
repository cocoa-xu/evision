defmodule Evision.HighGui.Test do
  use ExUnit.Case, async: false

  import ExUnit.CaptureLog

  alias Evision.HighGui

  @moduletag :highgui

  setup do
    on_exit(fn -> HighGui.destroyAllWindows() end)
    %{image: Evision.Mat.zeros({120, 160}, :u8)}
  end

  test "trackbar callbacks run in the evision_highgui process", %{image: image} do
    test = self()
    assert :ok = HighGui.imshow("trackbar", image)

    assert :ok =
             HighGui.createTrackbar("pos", "trackbar", 3, 10, fn pos ->
               send(test, {:pos, pos, self()})
             end)

    assert :ok = HighGui.waitKey(1)
    assert_receive {:pos, 3, pid}, 5_000
    assert pid == Process.whereis(:evision_highgui)
  end

  test "creating a trackbar again replaces its callback", %{image: image} do
    test = self()
    assert :ok = HighGui.imshow("replace", image)
    assert :ok = HighGui.createTrackbar("pos", "replace", 1, 10, &send(test, {:old, &1}))
    assert :ok = HighGui.createTrackbar("pos", "replace", 2, 10, &send(test, {:new, &1}))
    assert :ok = HighGui.waitKey(1)

    assert_receive {:old, 1}, 5_000
    assert_receive {:new, 2}, 5_000
    refute_received {:old, 2}
  end

  test "a callback can queue imshow while waitKey is waiting", %{image: image} do
    test = self()
    assert :ok = HighGui.imshow("busy", image)

    assert :ok =
             HighGui.createTrackbar("pos", "busy", 1, 10, fn _ ->
               # Let the GUI thread enter waitKey(3_000) first.
               Process.sleep(200)
               :ok = HighGui.imshow("busy", image)
               send(test, :shown)
             end)

    assert :ok = HighGui.waitKey(3_000)
    assert_receive :shown, 1_000
  end

  test "a failing callback is logged and later callbacks still run", %{image: image} do
    test = self()
    assert :ok = HighGui.imshow("failing", image)

    log =
      capture_log(fn ->
        assert :ok = HighGui.createTrackbar("bad", "failing", 1, 10, fn _ -> raise "boom" end)
        assert :ok = HighGui.createTrackbar("good", "failing", 2, 10, &send(test, {:good, &1}))
        assert :ok = HighGui.waitKey(1)
        assert_receive {:good, 2}, 5_000
      end)

    assert log =~ "Evision.HighGui callback failed"
    assert log =~ "boom"
  end

  test "setting and removing a mouse callback returns :ok", %{image: image} do
    assert :ok = HighGui.imshow("mouse", image)
    assert :ok = HighGui.setMouseCallback("mouse", fn _event, _x, _y, _flags -> :ok end)
    assert :ok = HighGui.setMouseCallback("mouse", nil)
    assert :ok = HighGui.waitKey(1)
  end
end
