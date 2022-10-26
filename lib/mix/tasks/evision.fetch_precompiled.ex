defmodule Mix.Tasks.Evision.Fetch do
  @shortdoc "Fetch precompiled NIFs and build the checksums"

  @moduledoc """
  A task responsible for downloading the precompiled NIFs for a given module.

  This task must only be used by package creators who want to ship the
  precompiled NIFs. The goal is to download the precompiled packages and
  generate a checksum to check-in alongside the project in the the Hex repository.
  This is done by passing the `--all` flag.

  You can also use the `--only-local` flag to download only the precompiled
  package for use during development.

  You can use the `--ignore-unavailable` flag to ignore any NIFs that are not available.
  This is useful when you are developing a new NIF that does not support all platforms.

  This task also accept the `--print` flag to print the checksums.
  """

  use Mix.Task
  alias Mix.Tasks.Compile.EvisionPrecompiled, as: Precompile
  require Logger

  @switches [
    all: :boolean,
    only_local: :boolean,
    print: :boolean,
    ignore_unavailable: :boolean
  ]

  @impl true
  def run([]) do
    raise "the module name and a flag is expected. Use \"--all\" or \"--only-local\" flags"
  end

  @impl true
  def run(flags) when is_list(flags) do
    {options, _args, _invalid} = OptionParser.parse(flags, strict: @switches)
    nif_version = Precompile.get_nif_version()
    urls =
      cond do
        Keyword.get(options, :all) ->
          Precompile.available_nif_urls(nif_version)

        Keyword.get(options, :only_local) ->
          [Precompile.current_target_nif_url(nif_version)]

        true ->
          raise "you need to specify either \"--all\" or \"--only-local\" flags"
      end

    result =
      Task.async_stream(urls, fn url ->
          filename = basename_from_url(url)
          cache_to = Path.join([Precompile.cache_dir(), filename])
          {:ok, algo, checksum} = Precompile.download!(url, cache_to, true)
          Logger.info("downloaded: url=#{url}, file=#{cache_to}, checksum[#{algo}]=#{checksum}")
          %{
            url: url,
            path: filename,
            checksum: checksum,
            checksum_algo: String.to_atom(algo)
          }
        end,
        timeout: :infinity
      )

    result = Enum.map(result, fn {:ok, r} -> r end)
    if Keyword.get(options, :print) do
      result
      |> Enum.map(fn map ->
        {Path.basename(Map.fetch!(map, :path)), Map.fetch!(map, :checksum)}
      end)
      |> Enum.sort()
      |> Enum.map_join("\n", fn {file, checksum} -> "#{checksum}  #{file}" end)
      |> IO.puts()
    end

    write_checksum!(result)
  end

  defp write_checksum!(result, app \\ Mix.Project.config()[:app]) do
    {elixir_file, erlang_file} = Precompile.checksum_file(app)

    pairs =
      for %{path: path, checksum: checksum, checksum_algo: algo} <- result, into: %{} do
        basename = Path.basename(path)
        checksum = "#{algo}:#{checksum}"
        {basename, checksum}
      end

    lines =
      for {filename, checksum} <- Enum.sort(pairs) do
        ~s(  "#{filename}" => #{inspect(checksum, limit: :infinity)},\n)
      end

    File.write!(elixir_file, ["%{\n", lines, "}\n"])

    lines =
      for {filename, checksum} <- Enum.sort(pairs) do
        "    \"#{filename}\" => #{inspect(checksum, limit: :infinity)}"
      end
    lines = Enum.join(lines, ",\n")

    File.write!(erlang_file, [
      """
      -module(checksum_evision).
      -export([checksum/0]).

      checksum() ->
        \#{
      """, lines, "\n  }.\n"])
  end

  defp basename_from_url(url) do
    uri = URI.parse(url)

    uri.path
    |> String.split("/")
    |> List.last()
  end
end
