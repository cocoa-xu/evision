small = Evision.resize(Evision.Mat.zeros({4, 4}, :u8), {2, 2})
IO.puts("resize: #{inspect(small.shape)}")

path = Path.join([__DIR__, "test", "testdata", "videocapture_test.mp4"])
ffmpeg = Evision.VideoCapture.videoCapture(path, apiPreference: Evision.Constant.cv_CAP_FFMPEG())
any = Evision.VideoCapture.videoCapture(path)
IO.inspect(ffmpeg, label: "videoCapture with CAP_FFMPEG")
IO.inspect(any, label: "videoCapture with CAP_ANY")

loaded =
  File.read!("/proc/self/maps")
  |> String.split("\n", trim: true)
  |> Enum.map(&List.last(String.split(&1, " ", trim: true)))
  |> Enum.filter(&String.match?(Path.basename(&1), ~r/^(libopencv_videoio_ffmpeg|libav)/))
  |> Enum.uniq()

IO.puts("loaded: #{inspect(loaded)}")

closed? = fn cap -> match?(%Evision.VideoCapture{isOpened: false}, cap) or match?({:error, _}, cap) end
System.halt(if small.shape == {2, 2} and closed?.(ffmpeg) and closed?.(any) and loaded == [], do: 0, else: 1)
