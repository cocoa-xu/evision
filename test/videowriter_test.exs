defmodule Evision.VideoWriter.Test do
  alias Evision, as: Cv
  use ExUnit.Case

  @moduletag timeout: 120_000

  defmodule WriteVideo do
    def given(input_video_file, output_video_file, output_fps, output_seconds) do
      {:ok, reader} = Cv.VideoCapture.videoCapture(input_video_file)
      {:ok, fourcc} = Cv.VideoWriter.fourcc("m", "p", "4", "v")
      {:ok, height} = Cv.VideoCapture.get(reader, Cv.cv_CAP_PROP_FRAME_HEIGHT())
      {:ok, width} = Cv.VideoCapture.get(reader, Cv.cv_CAP_PROP_FRAME_WIDTH())
      height = trunc(height)
      width = trunc(width)

      {:ok, writer} =
        Cv.VideoWriter.videoWriter(output_video_file, fourcc, output_fps / 1, [width, height])

      assert :ok == Cv.VideoWriter.isOpened(writer)
      {:ok, frame} = Cv.VideoCapture.read(reader)
      Cv.VideoCapture.release(reader)
      encode(frame, writer, output_fps * output_seconds)

      # verify
      {:ok, reader} = Cv.VideoCapture.videoCapture(output_video_file)
      {:ok, w_fourcc} = Cv.VideoWriter.fourcc("m", "p", "4", "v")
      {:ok, w_frames_count} = Cv.VideoCapture.get(reader, Cv.cv_CAP_PROP_FRAME_COUNT())
      {:ok, w_fps} = Cv.VideoCapture.get(reader, Cv.cv_CAP_PROP_FPS())
      {:ok, w_height} = Cv.VideoCapture.get(reader, Cv.cv_CAP_PROP_FRAME_HEIGHT())
      {:ok, w_width} = Cv.VideoCapture.get(reader, Cv.cv_CAP_PROP_FRAME_WIDTH())
      Cv.VideoCapture.release(reader)

      assert w_fourcc == fourcc
      assert w_frames_count == output_fps * output_seconds
      assert w_fps == output_fps
      assert w_height == height
      assert w_width == width
    end

    defp encode(_, writer, 0) do
      Cv.VideoWriter.release(writer)
    end

    defp encode(frame, writer, frames_count) do
      {:ok, writer} = Cv.VideoWriter.write(writer, frame)
      encode(frame, writer, frames_count - 1)
    end
  end

  @tag :video
  @tag :require_ffmpeg
  test "open a video file, read one frame and write a video@30FPS, duration 2 seconds" do
    input_video_file = Path.join(__DIR__, "videocapture_test.mp4")
    output_video_file = Path.join(__DIR__, "videowriter_test.mp4")
    WriteVideo.given(input_video_file, output_video_file, 30, 2)
    File.rm!(output_video_file)
  end
end
