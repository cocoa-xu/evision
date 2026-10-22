# Run with MIX_ENV=test mix run --no-compile scripts/bench_native.exs -- [options].
# Test dependencies provide Nx and Jason. Run different NIF builds in separate
# VMs with the same flags, OpenCV libraries, thread count, and benchmark order.
defmodule Evision.NativeBench do
  def run(args) do
    {opts, rest, invalid} =
      OptionParser.parse(args,
        strict: [
          output: :string,
          label: :string,
          samples: :integer,
          threads: :integer,
          only: :string
        ]
      )

    if rest != [] or invalid != [], do: raise("invalid benchmark arguments")
    samples = Keyword.get(opts, :samples, 25)
    threads = Keyword.get(opts, :threads, 1)
    if samples < 1 or threads < 1, do: raise("samples and threads must be positive")
    nif_path = Path.join(:code.priv_dir(:evision), "evision.so")
    nif_hash = :crypto.hash(:sha256, File.read!(nif_path)) |> Base.encode16(case: :lower)
    Evision.setNumThreads(threads)

    results =
      for {name, prepare} <- cases(),
          is_nil(opts[:only]) or String.contains?(name, opts[:only]) do
        {fun, expected, inputs} = prepare.()
        snapshots = Enum.map(inputs, &materialize/1)
        verify!(name, materialize(fun.()), expected)
        repeat(10, fun)
        {elapsed, _} = :timer.tc(fn -> repeat(10, fun) end)
        batch = max(1, min(1000, ceil(30_000 / max(elapsed, 1))))

        times =
          for _ <- 1..samples do
            :erlang.garbage_collect()
            {us, :ok} = :timer.tc(fn -> repeat(batch, fun) end)
            us / batch
          end

        verify!(name, materialize(fun.()), expected)
        verify!(name <> " input immutability", Enum.map(inputs, &materialize/1), snapshots)
        sorted = Enum.sort(times)
        median = Enum.at(sorted, div(length(sorted), 2))
        IO.puts("#{name}: #{Float.round(median, 3)} us")

        %{
          name: name,
          median_us: median,
          p95_us: Enum.at(sorted, ceil(length(sorted) * 0.95) - 1),
          samples_us: times,
          batch_size: batch,
          verified: true
        }
      end

    if results == [], do: raise("no benchmark cases matched")

    verify!(
      "NIF build did not change during measurement",
      :crypto.hash(:sha256, File.read!(nif_path)) |> Base.encode16(case: :lower),
      nif_hash
    )

    path = Keyword.fetch!(opts, :output)
    File.mkdir_p!(Path.dirname(path))

    File.write!(
      path,
      Jason.encode!(
        %{
          metadata: %{
            label: opts[:label],
            threads: threads,
            samples: samples,
            elixir: System.version(),
            otp: to_string(:erlang.system_info(:otp_release)),
            architecture: to_string(:erlang.system_info(:system_architecture)),
            schedulers: :erlang.system_info(:schedulers_online),
            nif_sha256: nif_hash,
            measured:
              "synchronous NIF calls including native output allocation; binary export outside timing"
          },
          results: results
        },
        pretty: true
      )
    )
  end

  defp cases do
    constructors =
      for size <- [16, 262_144] do
        {"from_binary/u8/#{size}",
         fn ->
           data = :binary.copy(<<7>>, size)

           {fn ->
              :evision_nif.mat_from_binary_by_shape(binary: data, shape: [size], t: "u", l: 8)
            end, data, []}
         end}
      end

    arithmetic =
      for storage <- [:binary, :owned], size <- [16, 262_144], op <- [:quotient, :reduce] do
        {"#{op}/s64/#{size}/#{storage}",
         fn ->
           t = Nx.iota({1, size}, type: :s64) |> Nx.remainder(31) |> Nx.add(1)
           a = mat(t, storage)
           b = mat(Nx.tensor([3], type: :s64), storage)

           {fun, expected} =
             case op do
               :quotient ->
                 {fn -> :evision_nif.mat_binop(l: a.ref, r: b.ref, op: 2) end, Nx.quotient(t, 3)}

               :reduce ->
                 {fn -> :evision_nif.mat_reduce(src: a.ref, op: 0, axis: 1) end,
                  Nx.sum(t, axes: [1])}
             end

           {fun, Nx.to_binary(expected), [a, b]}
         end}
      end

    windows =
      for type <- [:s32, :u16], side <- [8, 128] do
        {"window_sum/#{type}/#{side}",
         fn ->
           t = Nx.iota({side, side}, type: type) |> Nx.remainder(31)
           a = mat(t, :owned)

           {fn ->
              :evision_nif.mat_window_reduce(
                src: a.ref,
                in_dims: [side, side],
                out_dims: [side - 2, side - 2],
                win_dims: [3, 3],
                strides: [1, 1],
                pad_lo: [0, 0],
                dilations: [1, 1],
                op: 0
              )
            end, Nx.to_binary(Nx.window_sum(t, {3, 3})), [a]}
         end}
      end

    copies =
      for storage <- [:binary, :owned],
          side <- [8, 1024],
          op <- [:slice, :reverse, :put_slice, :indexed, :as_shape, :batch, :batch_repeat] do
        {"#{op}/f32/#{side}/#{storage}", fn -> copy_case(storage, side, op) end}
      end

    constructors ++ arithmetic ++ windows ++ copies
  end

  defp copy_case(storage, side, op) do
    t = Nx.iota({side, side}, type: :f32)
    a = mat(t, storage)
    half = div(side, 2)

    case op do
      :slice ->
        {fn ->
           :evision_nif.mat_strided_copy(
             src: a.ref,
             in_dims: [side, side],
             out_dims: [half, half],
             starts: [1, 1],
             strides: [1, 1]
           )
         end, reference(half, half, fn row, col -> (row + 1) * side + col + 1 end), [a]}

      :reverse ->
        {fn ->
           :evision_nif.mat_strided_copy(
             src: a.ref,
             in_dims: [side, side],
             out_dims: [side, side],
             starts: [0, side - 1],
             strides: [1, -1]
           )
         end, reference(side, side, fn row, col -> row * side + side - 1 - col end), [a]}

      :put_slice ->
        u = Nx.negate(Nx.iota({half, half}, type: :f32))
        b = mat(u, storage)

        {fn ->
           :evision_nif.mat_put_slice(
             base: a.ref,
             slice: b.ref,
             base_dims: [side, side],
             slice_dims: [half, half],
             starts: [1, 1]
           )
         end,
         reference(side, side, fn row, col ->
           if row in 1..half and col in 1..half,
             do: -1.0 * ((row - 1) * half + col - 1),
             else: row * side + col
         end), [a, b]}

      :indexed ->
        u = Nx.negate(Nx.iota({half, side}, type: :f32))
        b = mat(u, storage)
        i = mat(Nx.iota({half, 1}, type: :s64), storage)
        d = mat(Nx.tensor([side], type: :s64), storage)

        {fn ->
           :evision_nif.mat_indexed(
             src: a.ref,
             indices: i.ref,
             updates: b.ref,
             dims: d.ref,
             inner: side,
             op: 0
           )
         end,
         reference(side, side, fn row, col ->
           if row < half, do: -1.0 * (row * side + col), else: row * side + col
         end), [a, b, i, d]}

      :as_shape ->
        {fn -> :evision_nif.mat_as_shape(img: a.ref, as_shape: [2, half, side]) end,
         Nx.to_binary(t), [a]}

      batch_op when batch_op in [:batch, :batch_repeat] ->
        expected = Nx.to_binary(t)
        batch_size = if batch_op == :batch, do: half, else: half + 1
        bytes = batch_size * side * 4

        expected =
          if batch_op == :batch,
            do: expected,
            else: expected <> binary_part(expected, 0, side * 8)

        {fn ->
           :evision_nif.mat_to_batched(
             img: a.ref,
             as_shape: [side, side],
             batch_size: batch_size,
             leftover: if(batch_op == :batch, do: "discard", else: "repeat")
           )
         end, [binary_part(expected, 0, bytes), binary_part(expected, bytes, bytes)], [a]}
    end
  end

  # Independent formulas avoid quadratic BinaryBackend updates while checking
  # every output byte, including negative zero, outside the measured region.
  defp reference(rows, cols, value) do
    for row <- 0..(rows - 1),
        col <- 0..(cols - 1),
        into: <<>>,
        do: <<value.(row, col)::float-native-32>>
  end

  defp mat(tensor, storage) do
    m = Evision.Mat.from_binary_by_shape(Nx.to_binary(tensor), Nx.type(tensor), Nx.shape(tensor))
    if storage == :owned, do: Evision.Mat.clone(m), else: m
  end

  defp materialize(%{ref: ref}), do: :evision_nif.mat_to_binary(img: ref)
  defp materialize(list) when is_list(list), do: Enum.map(list, &materialize/1)
  defp materialize(other), do: raise("NIF failed: #{inspect(other)}")
  defp verify!(_name, same, same), do: :ok
  defp verify!(name, _got, _expected), do: raise("verification failed for #{name}")
  defp repeat(0, _fun), do: :ok

  defp repeat(n, fun) do
    fun.()
    repeat(n - 1, fun)
  end
end

Evision.NativeBench.run(System.argv() |> Enum.reject(&(&1 == "--")))
