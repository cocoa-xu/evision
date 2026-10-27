# Run with MIX_ENV=test mix run benchmarks/half_product.exs after building the NIF.
Nx.default_backend(Nx.BinaryBackend)

workloads = [
  {"small", {32, 32}, [1]},
  {"global", {1024, 1024}, nil},
  {"long_rows", {128, 8192}, [1]},
  {"columns", {8192, 128}, [0]},
  {"short_rows", {131_072, 8}, [1]},
  {"singletons", {1_048_576, 1}, [1]}
]

IO.puts("type,workload,op,median_us")

for type <- [:f16, :bf16], {label, shape, axes} <- workloads, op <- [:product] do
  count = Tuple.product(shape)
  unit = Nx.tensor([-1, 1, -1, 1], type: type) |> Nx.to_binary()
  bytes = :binary.copy(unit, div(count, 4))
  source = Nx.from_binary(bytes, type) |> Nx.reshape(shape)
  input = Evision.Mat.from_nx(source) |> Evision.Mat.clone() |> Evision.Mat.to_nx()
  run = fn -> apply(Nx, op, [input, [axes: axes]]) |> Nx.to_binary() end

  # These exact integer inputs avoid differences between native and Nx rounding.
  expected = apply(Nx, op, [source, [axes: axes]]) |> Nx.to_binary()
  ^expected = run.()
  for _ <- 1..3, do: run.()
  repetitions = if count < 4096, do: 40, else: 1

  samples =
    for _ <- 1..9 do
      :erlang.garbage_collect()
      start = System.monotonic_time(:nanosecond)

      for _ <- 1..repetitions do
        result = run.()
        true = byte_size(result) == byte_size(expected)
      end

      (System.monotonic_time(:nanosecond) - start) / repetitions / 1000
    end

  ^expected = run.()
  ^bytes = Nx.to_binary(input)
  median = samples |> Enum.sort() |> Enum.at(4)
  IO.puts("#{type},#{label},#{op},#{Float.round(median, 2)}")
end
