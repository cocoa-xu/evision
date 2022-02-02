defmodule Evision.VideoCapture.Test do
  alias Evision, as: OpenCV
  use ExUnit.Case

  @moduletag timeout: 120_000

  @tag :video
  @tag :require_ffmpeg
  test "open a video file and read one frame" do
    ret =
      Path.join(__DIR__, "videocapture_test.mp4")
      |> OpenCV.VideoCapture.videoCapture()

    assert :error != ret
    assert :ok == elem(ret, 0)
    video = elem(ret, 1)

    assert :ok == OpenCV.VideoCapture.isOpened(video)

    {:ok, fps} = OpenCV.VideoCapture.get(video, OpenCV.cv_CAP_PROP_FPS())
    {:ok, frame_count} = OpenCV.VideoCapture.get(video, OpenCV.cv_CAP_PROP_FRAME_COUNT())
    {:ok, height} = OpenCV.VideoCapture.get(video, OpenCV.cv_CAP_PROP_FRAME_HEIGHT())
    {:ok, width} = OpenCV.VideoCapture.get(video, OpenCV.cv_CAP_PROP_FRAME_WIDTH())
    {:ok, fourcc} = OpenCV.VideoCapture.get(video, OpenCV.cv_CAP_PROP_FOURCC())
    assert 43.2 == fps
    assert 18.0 == frame_count
    assert 1080 == height
    assert 1920 == width
    assert 828_601_960.0 == fourcc

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
