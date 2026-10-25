defmodule Evision.BackendBenchmarkTest do
  use ExUnit.Case, async: false

  import ExUnit.CaptureIO

  defmodule UnexpectedDefaultBackend do
    def init(opts), do: opts

    def iota(_, _, _) do
      raise "benchmark reference construction used the caller's backend"
    end
  end

  test "benchmark correctness oracle is independent of the caller's default backend" do
    previous_torchx = System.get_env("ENABLE_TORCHX_FOR_BENCHMARKING")
    System.put_env("ENABLE_TORCHX_FOR_BENCHMARKING", "false")

    output =
      Path.join(System.tmp_dir!(), "evision-benchmark-#{System.unique_integer([:positive])}.json")

    on_exit(fn ->
      File.rm(output)

      if previous_torchx do
        System.put_env("ENABLE_TORCHX_FOR_BENCHMARKING", previous_torchx)
      else
        System.delete_env("ENABLE_TORCHX_FOR_BENCHMARKING")
      end
    end)

    Nx.with_default_backend(UnexpectedDefaultBackend, fn ->
      capture_io(fn ->
        Mix.Tasks.Evision.Backend.Bench.run([
          "--profile",
          "smoke",
          "--only",
          "elementwise_sin_f32",
          "--warmup",
          "0",
          "--iterations",
          "1",
          "--output",
          output
        ])
      end)

      assert Nx.default_backend() == {UnexpectedDefaultBackend, []}
    end)

    report = File.read!(output)
    assert report =~ ~s("reference_backend":"Nx.BinaryBackend")
    assert report =~ ~s("verification":"passed")
  end
end
