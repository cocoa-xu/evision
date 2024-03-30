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
    ignore_unavailable: :boolean,
    dry_run: :boolean
  ]

  @impl true
  def run([]) do
    raise "the module name and a flag is expected. Use \"--all\" or \"--only-local\" flags"
  end

  @impl true
  def run(flags) when is_list(flags) do
    {options, _args, _invalid} = OptionParser.parse(flags, strict: @switches)
    urls =
      cond do
        Keyword.get(options, :all) ->
          {_, urls} =
            Enum.map_reduce(Precompile.get_available_nif_versions(), [], fn nif_version, urls ->
              {nil, [Precompile.available_nif_urls(nif_version) | urls]}
            end)
          List.flatten(urls)

        Keyword.get(options, :only_local) ->
          [Precompile.current_target_nif_url(Precompile.get_nif_version())]

        true ->
          raise "you need to specify either \"--all\" or \"--only-local\" flags"
      end
    checksum_algo = Precompile.checksum_algo()
    checksum_urls = Enum.map(urls, fn url -> "#{url}.#{Atom.to_string(checksum_algo)}" end)

    if Keyword.get(options, :dry_run) do
      Enum.each(checksum_urls, &IO.puts/1)
    else
      result =
        Task.async_stream(checksum_urls, fn url ->
            filename = basename_from_url(url)
            filename = String.replace(filename, ".#{Atom.to_string(checksum_algo)}", "")
            {:ok, checksum_content} = Precompile.download!(url)
            [checksum, _] = String.split(checksum_content, " ", trim: true)
            Logger.info("downloaded: url=#{url}, file=#{filename}, checksum[#{Atom.to_string(checksum_algo)}]=#{checksum}")
            %{
              url: String.replace(url, ".#{Atom.to_string(checksum_algo)}", ""),
              path: filename,
              checksum: checksum,
              checksum_algo: checksum_algo
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
      -module(checksum).
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
