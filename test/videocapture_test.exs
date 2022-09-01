defmodule Evision.VideoCapture.Test do
  alias Evision, as: Cv
  use ExUnit.Case

  @moduletag timeout: 120_000

  @tag :video
  @tag :require_ffmpeg
  test "open a video file and read one frame" do
    ret =
      Path.join(__DIR__, "videocapture_test.mp4")
      |> Cv.VideoCapture.videoCapture()

    assert :error != ret
    assert :ok == elem(ret, 0)
    video = elem(ret, 1)

    assert :ok == Cv.VideoCapture.isOpened(video)

    {:ok, fps} = Cv.VideoCapture.get(video, Cv.cv_CAP_PROP_FPS())
    {:ok, frame_count} = Cv.VideoCapture.get(video, Cv.cv_CAP_PROP_FRAME_COUNT())
    {:ok, height} = Cv.VideoCapture.get(video, Cv.cv_CAP_PROP_FRAME_HEIGHT())
    {:ok, width} = Cv.VideoCapture.get(video, Cv.cv_CAP_PROP_FRAME_WIDTH())
    {:ok, fourcc} = Cv.VideoCapture.get(video, Cv.cv_CAP_PROP_FOURCC())
    assert 43.2 == fps
    assert 18.0 == frame_count
    assert 1080 == height
    assert 1920 == width
    assert 828_601_960.0 == fourcc

    ret = Cv.VideoCapture.read(video)
    assert :error != ret
    assert :ok == elem(ret, 0)
    frame = elem(ret, 1)

    {:ok, shape} = Cv.Mat.shape(frame)
    assert {1080, 1920, 3} = shape

    ret = Cv.VideoCapture.release(video)
    assert :error != ret
    assert :ok == elem(ret, 0)

    ret = Cv.VideoCapture.read(video)
    assert :error == ret
  end
end
