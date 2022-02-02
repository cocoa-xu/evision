defmodule Evision.VideoWriter.Test do
  alias Evision, as: OpenCV
  use ExUnit.Case

  @moduletag timeout: 120_000

  defmodule WriteVideo do
    def given(input_video_file, output_video_file, output_fps, output_seconds) do
      {:ok, reader} = OpenCV.VideoCapture.videoCapture(input_video_file)
      {:ok, fourcc} = OpenCV.VideoWriter.fourcc("m", "p", "4", "v")
      {:ok, height} = OpenCV.VideoCapture.get(reader, OpenCV.cv_CAP_PROP_FRAME_HEIGHT())
      {:ok, width} = OpenCV.VideoCapture.get(reader, OpenCV.cv_CAP_PROP_FRAME_WIDTH())
      height = trunc(height)
      width = trunc(width)

      {:ok, writer} =
        OpenCV.VideoWriter.videoWriter(output_video_file, fourcc, output_fps / 1, [width, height])

      assert :ok == OpenCV.VideoWriter.isOpened(writer)
      {:ok, frame} = OpenCV.VideoCapture.read(reader)
      OpenCV.VideoCapture.release(reader)
      encode(frame, writer, output_fps * output_seconds)

      # verify
      {:ok, reader} = OpenCV.VideoCapture.videoCapture(output_video_file)
      {:ok, w_fourcc} = OpenCV.VideoWriter.fourcc("m", "p", "4", "v")
      {:ok, w_frames_count} = OpenCV.VideoCapture.get(reader, OpenCV.cv_CAP_PROP_FRAME_COUNT())
      {:ok, w_fps} = OpenCV.VideoCapture.get(reader, OpenCV.cv_CAP_PROP_FPS())
      {:ok, w_height} = OpenCV.VideoCapture.get(reader, OpenCV.cv_CAP_PROP_FRAME_HEIGHT())
      {:ok, w_width} = OpenCV.VideoCapture.get(reader, OpenCV.cv_CAP_PROP_FRAME_WIDTH())
      OpenCV.VideoCapture.release(reader)

      assert w_fourcc == fourcc
      assert w_frames_count == output_fps * output_seconds
      assert w_fps == output_fps
      assert w_height == height
      assert w_width == width
    end

    defp encode(_, writer, 0) do
      OpenCV.VideoWriter.release(writer)
    end

    defp encode(frame, writer, frames_count) do
      {:ok, writer} = OpenCV.VideoWriter.write(writer, frame)
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
