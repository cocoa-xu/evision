defmodule Evision.VideoCapture.Test do
  use ExUnit.Case

  @moduletag timeout: 120_000

  @tag :video
  @tag :require_ffmpeg
  test "open a video file and read one frame" do
    video =
      Evision.VideoCapture.videoCapture(Path.join([__DIR__, "testdata", "videocapture_test.mp4"]))

    %Evision.VideoCapture{
      isOpened: true,
      frame_height: 1080.0,
      frame_width: 1920.0
    } = video

    %Evision.Mat{shape: {1080, 1920, 3}} = Evision.VideoCapture.read(video)

    video = Evision.VideoCapture.release(video)
    false = Evision.VideoCapture.read(video)
  end
end
