defmodule Evision.VideoWriter.Test do
  use ExUnit.Case

  @moduletag timeout: 120_000

  defmodule WriteVideo do
    def given(input_video_file, output_video_file, output_fps, output_seconds) do
      reader = Evision.VideoCapture.videoCapture(input_video_file)
      # mp4v
      fourcc = Evision.VideoWriter.fourcc(109, 112, 52, 118)
      height = Evision.VideoCapture.get(reader, Evision.cv_CAP_PROP_FRAME_HEIGHT()) |> trunc()
      width = Evision.VideoCapture.get(reader, Evision.cv_CAP_PROP_FRAME_WIDTH()) |> trunc()

      writer =
        Evision.VideoWriter.videoWriter(
          output_video_file,
          fourcc,
          output_fps / 1,
          {width, height}
        )

      assert true == Evision.VideoWriter.isOpened(writer)
      frame = Evision.VideoCapture.read(reader)
      Evision.VideoCapture.release(reader)
      encode(frame, writer, output_fps * output_seconds)

      # verify
      reader = Evision.VideoCapture.videoCapture(output_video_file)
      ^output_fps = Evision.VideoCapture.get(reader, Evision.cv_CAP_PROP_FPS()) |> trunc()
      ^height = Evision.VideoCapture.get(reader, Evision.cv_CAP_PROP_FRAME_HEIGHT()) |> trunc()
      ^width = Evision.VideoCapture.get(reader, Evision.cv_CAP_PROP_FRAME_WIDTH()) |> trunc()
      w_frames_count = Evision.VideoCapture.get(reader, Evision.cv_CAP_PROP_FRAME_COUNT())
      Evision.VideoCapture.release(reader)

      assert w_frames_count == output_fps * output_seconds
    end

    defp encode(_, writer, 0) do
      Evision.VideoWriter.release(writer)
    end

    defp encode(frame, writer, frames_count) do
      writer = Evision.VideoWriter.write(writer, frame)
      encode(frame, writer, frames_count - 1)
    end
  end

  @tag :video
  @tag :require_ffmpeg
  test "open a video file, read one frame and write a video@30FPS, duration 2 seconds" do
    input_video_file = Path.join([__DIR__, "testdata", "videocapture_test.mp4"])
    output_video_file = Path.join([__DIR__, "testdata", "videowriter_test.mp4"])
    WriteVideo.given(input_video_file, output_video_file, 30, 2)
    File.rm!(output_video_file)
  end
end
