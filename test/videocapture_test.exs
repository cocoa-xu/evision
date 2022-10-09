defmodule Evision.VideoCapture.Test do
  use ExUnit.Case

  @moduletag timeout: 120_000

  @tag :video
  @tag :require_ffmpeg
  test "open a video file and read one frame" do
    {:ok, video=%Evision.VideoCapture{}} = Evision.VideoCapture.videoCapture(Path.join(__DIR__, "videocapture_test.mp4"))

    assert true == Evision.VideoCapture.isOpened(video)

    {:ok, fps} = Evision.VideoCapture.get(video, Evision.cv_CAP_PROP_FPS())
    {:ok, frame_count} = Evision.VideoCapture.get(video, Evision.cv_CAP_PROP_FRAME_COUNT())
    {:ok, height} = Evision.VideoCapture.get(video, Evision.cv_CAP_PROP_FRAME_HEIGHT())
    {:ok, width} = Evision.VideoCapture.get(video, Evision.cv_CAP_PROP_FRAME_WIDTH())
    {:ok, fourcc} = Evision.VideoCapture.get(video, Evision.cv_CAP_PROP_FOURCC())
    assert 43.2 == fps
    assert 18.0 == frame_count
    assert 1080 == height
    assert 1920 == width
    assert 828_601_960.0 == fourcc

    ret = Evision.VideoCapture.read(video)
    assert :error != ret
    assert :ok == elem(ret, 0)
    frame = elem(ret, 1)

    {:ok, shape} = Evision.Mat.shape(frame)
    assert {1080, 1920, 3} = shape

    ret = Evision.VideoCapture.release(video)
    assert :error != ret
    assert :ok == elem(ret, 0)

    ret = Evision.VideoCapture.read(video)
    assert :error == ret
  end
end
