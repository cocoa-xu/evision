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
      frame_count: 18.0,
      frame_height: 1080.0,
      frame_width: 1920.0
    } = video

    fourcc = Evision.VideoCapture.get(video, Evision.Constant.cv_CAP_PROP_FOURCC())

    # "\x63\x76\x65\x68"
    # "cveh" => "hevc"
    assert 1_668_703_592.0 == fourcc

    %Evision.Mat{shape: {1080, 1920, 3}} = Evision.VideoCapture.read(video)

    video = Evision.VideoCapture.release(video)
    false = Evision.VideoCapture.read(video)
  end
end
