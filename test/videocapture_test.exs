defmodule OpenCV.VideoCapture.Test do
  use ExUnit.Case

  @moduletag timeout: 120_000

  test "open a video file and read one frame" do
    ret =
      [__DIR__, "videocapture_test.mp4"]
      |> Path.join
      |> Path.expand
      |> OpenCV.VideoCapture.videocapture
    assert :error != ret
    assert :ok == elem(ret, 0)
    video = elem(ret, 1)

    ret = OpenCV.VideoCapture.read(video)
    assert :error != ret
    assert :ok == elem(ret, 0)
    frame = elem(ret, 1)

    {:ok, shape} = OpenCV.Mat.shape(frame)
    assert {1080, 1920, 3} = shape

    ret = OpenCV.VideoCapture.release(video)
    assert :error != ret
    assert :ok == elem(ret, 0)

    ret = OpenCV.VideoCapture.read(video)
    assert :error == ret
  end
end
