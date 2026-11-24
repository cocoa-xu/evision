ExUnit.start(autorun: false)

for file <- ~w(videocapture_test.exs videowriter_test.exs) do
  Code.require_file(Path.join([__DIR__, "test", file]))
end

%{total: total, failures: failures} = ExUnit.run()

loaded =
  File.read!("/proc/self/maps")
  |> String.split("\n", trim: true)
  |> Enum.map(&List.last(String.split(&1, " ", trim: true)))
  |> Enum.filter(&String.match?(Path.basename(&1), ~r/^(libopencv_videoio_ffmpeg|libavcodec\.so)/))
  |> Enum.uniq()

plugin = for p <- loaded, [_, m] <- [Regex.run(~r/ffmpeg_avcodec(\d+)\.so$/, p)], do: m
system = for p <- loaded, [_, m] <- [Regex.run(~r/libavcodec\.so\.(\d+)/, p)], do: m
IO.puts("loaded: #{inspect(loaded)}")
IO.puts("tests: #{total}, failures: #{failures}, plugin avcodec: #{inspect(plugin)}, system avcodec: #{inspect(system)}")

System.halt(if total == 2 and failures == 0 and length(plugin) == 1 and plugin == system, do: 0, else: 1)
