defmodule Mix.Tasks.Evision.Backend.Bench do
  @shortdoc "Benchmark Evision.Backend Nx operations"

  @moduledoc """
  Benchmarks representative Evision.Backend Nx operations.

      mix evision.backend.bench
      mix evision.backend.bench --profile smoke --output /tmp/evision-backend.json
      mix evision.backend.bench --only conv,reduce --iterations 10 --warmup 3

  Options:

    * `--output` - JSON output path. Defaults to `benchmarks/evision_backend/latest.json`.
    * `--profile` - one of `smoke`, `default`, or `full`. Defaults to `default`.
    * `--warmup` - warmup iterations per case.
    * `--iterations` - measured iterations per case.
    * `--only` - comma-separated case names or categories to run.
    * `--list` - print available benchmark cases and exit.
    * `--no-verify` - skip the correctness check before measuring.

  Timings include output materialization to keep the measured work eager and comparable.
  """

  use Mix.Task

  @requirements ["app.start"]
  @default_output "benchmarks/evision_backend/latest.json"
  @switches [
    output: :string,
    profile: :string,
    warmup: :integer,
    iterations: :integer,
    only: :string,
    list: :boolean,
    no_verify: :boolean
  ]
  @aliases [o: :output]

  @impl true
  def run(args) do
    {opts, positional, invalid} = OptionParser.parse(args, strict: @switches, aliases: @aliases)

    if positional != [] do
      Mix.raise("unexpected positional arguments: #{Enum.join(positional, " ")}")
    end

    if invalid != [] do
      invalid_switches = Enum.map_join(invalid, ", ", fn {switch, _} -> to_string(switch) end)
      Mix.raise("unknown or invalid switches: #{invalid_switches}")
    end

    profile = Keyword.get(opts, :profile, "default")
    config = profile_config(profile)
    output = Keyword.get(opts, :output, @default_output)
    warmup = Keyword.get(opts, :warmup, config.warmup)
    iterations = Keyword.get(opts, :iterations, config.iterations)
    verify? = not Keyword.get(opts, :no_verify, false)

    validate_count!("warmup", warmup, 0)
    validate_count!("iterations", iterations, 1)

    cases =
      profile
      |> cases()
      |> filter_cases(Keyword.get(opts, :only))

    if cases == [] do
      Mix.raise("no benchmark cases matched --only")
    end

    if Keyword.get(opts, :list, false) do
      list_cases(cases)
    else
      report = run_suite(cases, profile, output, warmup, iterations, verify?)
      File.mkdir_p!(Path.dirname(output))
      File.write!(output, [encode_json(report), "\n"])
      print_summary(report)
      Mix.shell().info("Wrote #{Path.relative_to_cwd(output)}")
    end
  end

  defp run_suite(cases, profile, output, warmup, iterations, verify?) do
    metadata = %{
      "elixir" => System.version(),
      "evision" => application_version(:evision),
      "backend" => "Evision.Backend",
      "generated_at" => DateTime.utc_now() |> DateTime.truncate(:second) |> DateTime.to_iso8601(),
      "git_revision" => git_revision(),
      "iterations" => iterations,
      "materialized" => true,
      "nx" => application_version(:nx),
      "otp" => :erlang.system_info(:otp_release) |> to_string(),
      "output" => output,
      "profile" => profile,
      "schedulers_online" => :erlang.system_info(:schedulers_online),
      "system_architecture" => :erlang.system_info(:system_architecture) |> to_string(),
      "verified" => verify?,
      "warmup" => warmup
    }

    results =
      Enum.map(cases, fn bench ->
        Mix.shell().info("Running #{bench.name}")
        run_case(bench, warmup, iterations, verify?)
      end)

    %{"metadata" => metadata, "results" => results}
  end

  defp run_case(bench, warmup, iterations, verify?) do
    binary_input = bench.input.()
    evision_input = map_tensors(binary_input, &Nx.backend_copy(&1, Evision.Backend))

    if verify? do
      verify_case!(bench, bench.run.(evision_input), bench.run.(binary_input))
    end

    evision_stats =
      measure(fn -> bench.run.(evision_input) |> materialize() end, warmup, iterations)

    %{
      "category" => bench.category,
      "details" => bench.details,
      "evision_backend" => evision_stats,
      "name" => bench.name,
      "verification" => if(verify?, do: "passed", else: "skipped")
    }
  end

  defp cases(profile) do
    config = profile_config(profile)

    [
      %{
        name: "elementwise_sin_f32",
        category: "elementwise",
        details: %{"op" => "Nx.sin/1", "shape" => shape_list(config.elementwise), "type" => "f32"},
        input: fn -> make_f32(config.elementwise) end,
        run: fn tensor -> Nx.sin(tensor) end
      },
      %{
        name: "elementwise_binary_chain_f32",
        category: "elementwise",
        details: %{
          "op" => "add(subtract(a, b), a)",
          "shape" => shape_list(config.elementwise),
          "type" => "f32"
        },
        input: fn -> {make_f32(config.elementwise), make_f32(config.elementwise, 2053, 19)} end,
        run: fn {a, b} -> Nx.add(Nx.subtract(a, b), a) end
      },
      %{
        name: "reduce_sum_axis1_f32",
        category: "reduce",
        details: %{
          "axes" => [1],
          "op" => "Nx.sum/2",
          "shape" => shape_list(config.reduce),
          "type" => "f32"
        },
        input: fn -> make_f32(config.reduce) end,
        run: fn tensor -> Nx.sum(tensor, axes: [1]) end
      },
      %{
        name: "reduce_max_axis0_f32",
        category: "reduce",
        details: %{
          "axes" => [0],
          "op" => "Nx.reduce_max/2",
          "shape" => shape_list(config.reduce),
          "type" => "f32"
        },
        input: fn -> make_f32(config.reduce) end,
        run: fn tensor -> Nx.reduce_max(tensor, axes: [0]) end
      },
      %{
        name: "cumulative_sum_axis1_f32",
        category: "cumulative",
        details: %{
          "axis" => 1,
          "op" => "Nx.cumulative_sum/2",
          "shape" => shape_list(config.reduce),
          "type" => "f32"
        },
        input: fn -> make_f32(config.reduce) end,
        run: fn tensor -> Nx.cumulative_sum(tensor, axis: 1) end
      },
      %{
        name: "sort_axis1_f32",
        category: "sort",
        details: %{
          "axis" => 1,
          "op" => "Nx.sort/2",
          "shape" => shape_list(config.sort),
          "type" => "f32"
        },
        input: fn -> make_f32(config.sort, 8191, 37) end,
        run: fn tensor -> Nx.sort(tensor, axis: 1) end
      },
      %{
        name: "argsort_axis1_f32",
        category: "sort",
        details: %{
          "axis" => 1,
          "op" => "Nx.argsort/2",
          "shape" => shape_list(config.sort),
          "type" => "f32"
        },
        input: fn -> make_f32(config.sort, 8191, 37) end,
        run: fn tensor -> Nx.argsort(tensor, axis: 1) end
      },
      %{
        name: "take_axis0_f32",
        category: "indexing",
        details: %{
          "axis" => 0,
          "count" => config.take_count,
          "op" => "Nx.take/3",
          "shape" => shape_list(config.take_source),
          "type" => "f32"
        },
        input: fn ->
          {make_f32(config.take_source),
           make_indices(config.take_count, elem(config.take_source, 0), 11)}
        end,
        run: fn {tensor, indices} -> Nx.take(tensor, indices, axis: 0) end
      },
      %{
        name: "gather_2d_f32",
        category: "indexing",
        details: %{
          "count" => config.gather_count,
          "op" => "Nx.gather/3",
          "shape" => shape_list(config.gather_source),
          "type" => "f32"
        },
        input: fn ->
          {rows, cols} = config.gather_source
          row_indices = make_indices(config.gather_count, rows, 5)
          col_indices = make_indices(config.gather_count, cols, 7)
          {make_f32(config.gather_source), Nx.stack([row_indices, col_indices], axis: 1)}
        end,
        run: fn {tensor, indices} -> Nx.gather(tensor, indices) end
      },
      %{
        name: "window_sum_2d_same_f32",
        category: "window",
        details: %{
          "op" => "Nx.window_sum/3",
          "padding" => "same",
          "shape" => shape_list(config.window),
          "type" => "f32",
          "window" => [5, 5]
        },
        input: fn -> make_f32(config.window) end,
        run: fn tensor -> Nx.window_sum(tensor, {5, 5}, padding: :same) end
      },
      %{
        name: "window_max_2d_same_f32",
        category: "window",
        details: %{
          "op" => "Nx.window_max/3",
          "padding" => "same",
          "shape" => shape_list(config.window),
          "type" => "f32",
          "window" => [5, 5]
        },
        input: fn -> make_f32(config.window) end,
        run: fn tensor -> Nx.window_max(tensor, {5, 5}, padding: :same) end
      },
      %{
        name: "conv2d_same_f32",
        category: "conv",
        details: %{
          "input_shape" => shape_list(config.conv_input),
          "kernel_shape" => shape_list(config.conv_kernel),
          "op" => "Nx.conv/3",
          "padding" => "same",
          "type" => "f32"
        },
        input: fn -> {make_f32(config.conv_input), make_f32(config.conv_kernel, 1021, 13)} end,
        run: fn {input, kernel} -> Nx.conv(input, kernel, padding: :same) end,
        atol: 1.0e-3,
        rtol: 1.0e-3
      },
      %{
        name: "triangular_solve_f64",
        category: "linear_algebra",
        details: %{
          "op" => "Nx.LinAlg.triangular_solve/3",
          "shape" => [config.triangular_solve, config.triangular_solve],
          "type" => "f64"
        },
        input: fn -> make_triangular_solve_inputs(config.triangular_solve) end,
        run: fn {a, b} -> Nx.LinAlg.triangular_solve(a, b) end,
        atol: 1.0e-3,
        rtol: 1.0e-3
      },
      %{
        name: "concatenate_axis0_f32",
        category: "shape",
        details: %{
          "axis" => 0,
          "op" => "Nx.concatenate/2",
          "part_count" => config.concat_parts,
          "part_shape" => shape_list(config.concat_shape),
          "type" => "f32"
        },
        input: fn ->
          for index <- 1..config.concat_parts do
            make_f32(config.concat_shape, 4096 + index, 17 + index)
          end
        end,
        run: fn tensors -> Nx.concatenate(tensors, axis: 0) end
      }
    ]
  end

  defp profile_config("smoke") do
    %{
      concat_parts: 3,
      concat_shape: {64, 64},
      conv_input: {1, 4, 32, 32},
      conv_kernel: {8, 4, 3, 3},
      elementwise: {128, 128},
      gather_count: 512,
      gather_source: {256, 32},
      iterations: 2,
      reduce: {128, 256},
      sort: {64, 128},
      take_count: 512,
      take_source: {1024, 16},
      triangular_solve: 24,
      warmup: 1,
      window: {64, 64}
    }
  end

  defp profile_config("default") do
    %{
      concat_parts: 8,
      concat_shape: {256, 256},
      conv_input: {1, 8, 64, 64},
      conv_kernel: {16, 8, 3, 3},
      elementwise: {512, 512},
      gather_count: 4096,
      gather_source: {2048, 64},
      iterations: 5,
      reduce: {512, 1024},
      sort: {256, 512},
      take_count: 4096,
      take_source: {8192, 32},
      triangular_solve: 64,
      warmup: 2,
      window: {256, 256}
    }
  end

  defp profile_config("full") do
    %{
      concat_parts: 12,
      concat_shape: {512, 512},
      conv_input: {2, 16, 96, 96},
      conv_kernel: {32, 16, 3, 3},
      elementwise: {1024, 1024},
      gather_count: 16_384,
      gather_source: {8192, 128},
      iterations: 8,
      reduce: {1024, 2048},
      sort: {512, 1024},
      take_count: 16_384,
      take_source: {32_768, 64},
      triangular_solve: 128,
      warmup: 3,
      window: {512, 512}
    }
  end

  defp profile_config(profile) do
    Mix.raise("unknown profile #{inspect(profile)}; expected smoke, default, or full")
  end

  defp make_f32(shape, modulus \\ 4099, multiplier \\ 17) do
    make_float(shape, :f32, modulus, multiplier)
  end

  defp make_f64(shape, modulus, multiplier) do
    make_float(shape, :f64, modulus, multiplier)
  end

  defp make_float(shape, type, modulus, multiplier) do
    shape
    |> Nx.iota(type: :s64)
    |> Nx.multiply(multiplier)
    |> Nx.add(23)
    |> Nx.remainder(modulus)
    |> Nx.as_type(type)
    |> Nx.subtract(modulus / 2)
    |> Nx.divide(modulus / 8)
  end

  defp make_indices(count, limit, multiplier) do
    {count}
    |> Nx.iota(type: :s64)
    |> Nx.multiply(multiplier)
    |> Nx.add(3)
    |> Nx.remainder(limit)
  end

  defp make_triangular_solve_inputs(size) do
    matrix =
      {size, size}
      |> make_f64(257, 19)
      |> Nx.tril()

    a = Nx.add(matrix, Nx.multiply(Nx.eye(size, type: :f64), size * 16.0))
    b = make_f64({size}, 131, 7)
    {a, b}
  end

  defp filter_cases(cases, nil), do: cases

  defp filter_cases(cases, only) do
    selected =
      only
      |> String.split(",", trim: true)
      |> Enum.map(&String.trim/1)
      |> MapSet.new()

    Enum.filter(cases, fn bench ->
      MapSet.member?(selected, bench.name) or MapSet.member?(selected, bench.category)
    end)
  end

  defp list_cases(cases) do
    Enum.each(cases, fn bench ->
      Mix.shell().info("#{bench.name} [#{bench.category}]")
    end)
  end

  defp measure(fun, warmup, iterations) do
    repeat(warmup, fun)

    samples =
      for _ <- 1..iterations do
        :erlang.garbage_collect()
        start = System.monotonic_time(:nanosecond)
        _ = fun.()
        System.monotonic_time(:nanosecond) - start
      end

    stats(samples)
  end

  defp repeat(times, fun) when times > 0 do
    Enum.each(1..times, fn _ -> fun.() end)
  end

  defp repeat(_times, _fun), do: :ok

  defp stats(samples) do
    sorted = Enum.sort(samples)
    count = length(samples)
    mean = Enum.sum(samples) / count

    %{
      "iterations" => count,
      "mean_us" => ns_to_us(mean),
      "median_us" => ns_to_us(percentile(sorted, 0.5)),
      "min_us" => ns_to_us(List.first(sorted)),
      "p95_us" => ns_to_us(percentile(sorted, 0.95)),
      "samples_us" => Enum.map(samples, &ns_to_us/1)
    }
  end

  defp percentile(sorted, percentile) do
    count = length(sorted)
    index = max(ceil(count * percentile), 1) - 1
    Enum.at(sorted, min(index, count - 1))
  end

  defp ns_to_us(ns), do: Float.round(ns / 1000.0, 3)

  defp materialize(%Nx.Tensor{} = tensor) do
    Nx.to_binary(tensor)
  end

  defp materialize(tuple) when is_tuple(tuple) do
    tuple
    |> Tuple.to_list()
    |> Enum.map(&materialize/1)
  end

  defp materialize(list) when is_list(list), do: Enum.map(list, &materialize/1)
  defp materialize(value), do: value

  defp verify_case!(bench, got, expected) do
    case compare(got, expected, Map.get(bench, :atol, 1.0e-4), Map.get(bench, :rtol, 1.0e-4)) do
      :ok -> :ok
      {:error, reason} -> Mix.raise("#{bench.name} verification failed: #{reason}")
    end
  end

  defp compare(%Nx.Tensor{} = got, %Nx.Tensor{} = expected, atol, rtol) do
    got = Nx.backend_copy(got, Nx.BinaryBackend)
    expected = Nx.backend_copy(expected, Nx.BinaryBackend)

    cond do
      Nx.shape(got) != Nx.shape(expected) ->
        {:error, "shape mismatch #{inspect(Nx.shape(got))} != #{inspect(Nx.shape(expected))}"}

      Nx.type(got) != Nx.type(expected) ->
        {:error, "type mismatch #{inspect(Nx.type(got))} != #{inspect(Nx.type(expected))}"}

      float_type?(Nx.type(got)) ->
        if Nx.to_number(Nx.all_close(got, expected, atol: atol, rtol: rtol, equal_nan: true)) == 1 do
          :ok
        else
          {:error, "float values differ beyond atol=#{atol}, rtol=#{rtol}"}
        end

      Nx.to_binary(got) == Nx.to_binary(expected) ->
        :ok

      true ->
        {:error, "values differ"}
    end
  end

  defp compare(got, expected, atol, rtol) when is_tuple(got) and is_tuple(expected) do
    compare(got |> Tuple.to_list(), expected |> Tuple.to_list(), atol, rtol)
  end

  defp compare(got, expected, atol, rtol) when is_list(got) and is_list(expected) do
    if length(got) == length(expected) do
      got
      |> Enum.zip(expected)
      |> Enum.reduce_while(:ok, fn {left, right}, :ok ->
        case compare(left, right, atol, rtol) do
          :ok -> {:cont, :ok}
          error -> {:halt, error}
        end
      end)
    else
      {:error, "list length mismatch #{length(got)} != #{length(expected)}"}
    end
  end

  defp compare(got, expected, _atol, _rtol) do
    if got == expected, do: :ok, else: {:error, "values differ"}
  end

  defp float_type?({:f, _}), do: true
  defp float_type?(_type), do: false

  defp map_tensors(%Nx.Tensor{} = tensor, fun), do: fun.(tensor)

  defp map_tensors(tuple, fun) when is_tuple(tuple) do
    tuple
    |> Tuple.to_list()
    |> Enum.map(&map_tensors(&1, fun))
    |> List.to_tuple()
  end

  defp map_tensors(list, fun) when is_list(list), do: Enum.map(list, &map_tensors(&1, fun))

  defp map_tensors(map, fun) when is_map(map) do
    Map.new(map, fn {key, value} -> {key, map_tensors(value, fun)} end)
  end

  defp map_tensors(value, _fun), do: value

  defp validate_count!(_name, value, minimum) when is_integer(value) and value >= minimum, do: :ok

  defp validate_count!(name, value, minimum) do
    Mix.raise("#{name} must be an integer >= #{minimum}, got #{inspect(value)}")
  end

  defp print_summary(%{"results" => results}) do
    Mix.shell().info("")
    Mix.shell().info("case,evision_median_us,evision_p95_us,evision_min_us,evision_mean_us")

    Enum.each(results, fn result ->
      stats = result["evision_backend"]

      Mix.shell().info(
        Enum.join(
          [
            result["name"],
            format_float(stats["median_us"]),
            format_float(stats["p95_us"]),
            format_float(stats["min_us"]),
            format_float(stats["mean_us"])
          ],
          ","
        )
      )
    end)
  end

  defp format_float(value) when is_float(value), do: :erlang.float_to_binary(value, decimals: 3)
  defp format_float(value), do: to_string(value)

  defp shape_list(shape) when is_tuple(shape), do: Tuple.to_list(shape)

  defp application_version(app) do
    case Application.spec(app, :vsn) do
      nil -> nil
      version -> to_string(version)
    end
  end

  defp git_revision do
    case System.cmd("git", ["rev-parse", "--short", "HEAD"], stderr_to_stdout: true) do
      {revision, 0} -> String.trim(revision)
      _ -> nil
    end
  end

  defp encode_json(value) when is_map(value) do
    entries =
      value
      |> Enum.sort_by(fn {key, _value} -> to_string(key) end)
      |> Enum.map(fn {key, entry_value} ->
        [encode_json(to_string(key)), ":", encode_json(entry_value)]
      end)

    ["{", Enum.intersperse(entries, ","), "}"]
  end

  defp encode_json(value) when is_list(value) do
    ["[", value |> Enum.map(&encode_json/1) |> Enum.intersperse(","), "]"]
  end

  defp encode_json(value) when is_binary(value) do
    ["\"", escape_json_string(value), "\""]
  end

  defp encode_json(value) when is_integer(value), do: Integer.to_string(value)

  defp encode_json(value) when is_float(value),
    do: :erlang.float_to_binary(value, [:compact, decimals: 6])

  defp encode_json(value) when is_boolean(value), do: to_string(value)
  defp encode_json(nil), do: "null"
  defp encode_json(value) when is_atom(value), do: encode_json(to_string(value))

  defp escape_json_string(value) do
    value
    |> String.replace("\\", "\\\\")
    |> String.replace("\"", "\\\"")
    |> String.replace("\n", "\\n")
    |> String.replace("\r", "\\r")
    |> String.replace("\t", "\\t")
  end
end
