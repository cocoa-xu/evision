defmodule Evision.TestHelper do
  @wine_url "https://archive.ics.uci.edu/static/public/109/wine.zip"
  @wine_data_sha256 "6be6b1203f3d51df0b553a70e57b8a723cd405683958204f96d23d7cd6aea659"

  # Same features and labels as Scidata.Wine.download/0, but read from test/testdata once
  # downloaded: Scidata asks the server on every call, so any server error fails the test.
  def wine_dataset do
    zip = Path.join([__DIR__, "testdata", "wine.zip"])
    :ok = download!(@wine_url, zip)
    {:ok, [{_, data}]} = :zip.extract(File.read!(zip), [:memory, file_list: [~c"wine.data"]])

    if Base.encode16(:crypto.hash(:sha256, data), case: :lower) != @wine_data_sha256 do
      raise "#{zip} does not contain the expected wine.data; delete it to download it again"
    end

    rows =
      for row <- String.split(data) do
        [label | values] = String.split(row, ",")
        {String.to_integer(label) - 1, Enum.map(values, &elem(Float.parse("0" <> &1), 0))}
      end

    {Enum.map(rows, &elem(&1, 1)), Enum.map(rows, &elem(&1, 0))}
  end

  def download!(url, save_as, overwrite \\ false)

  def download!(url, save_as, false) do
    unless File.exists?(save_as) do
      download!(url, save_as, true)
    end

    :ok
  end

  def download!(url, save_as, true) do
    http_opts = []
    opts = [body_format: :binary]
    arg = {url, []}

    if proxy = System.get_env("HTTP_PROXY") || System.get_env("http_proxy") do
      %{host: host, port: port} = URI.parse(proxy)

      :httpc.set_options([{:proxy, {{String.to_charlist(host), port}, []}}])
    end

    if proxy = System.get_env("HTTPS_PROXY") || System.get_env("https_proxy") do
      %{host: host, port: port} = URI.parse(proxy)
      :httpc.set_options([{:https_proxy, {{String.to_charlist(host), port}, []}}])
    end

    body =
      case :httpc.request(:get, arg, http_opts, opts) do
        {:ok, {{_, 200, _}, _, body}} ->
          body

        {:error, reason} ->
          raise inspect(reason)
      end

    File.write!(save_as, body)
  end

  @doc """
  This function chunks binary data by every requested `chunk_size`

  To make it more general, this function allows the length of the last chunk
  to be less than the request `chunk_size`.

  For example, if you have a 7-byte binary data, and you'd like to chunk it by every
  4 bytes, then this function will return two chunks with the first gives you the
  byte 0 to 3, and the second one gives byte 4 to 6.
  """
  def chunk_binary(binary, chunk_size) when is_binary(binary) do
    total_bytes = byte_size(binary)
    full_chunks = div(total_bytes, chunk_size)

    chunks =
      if full_chunks > 0 do
        for i <- 0..(full_chunks - 1), reduce: [] do
          acc -> [:binary.part(binary, chunk_size * i, chunk_size) | acc]
        end
      else
        []
      end

    remaining = rem(total_bytes, chunk_size)

    chunks =
      if remaining > 0 do
        [:binary.part(binary, chunk_size * full_chunks, remaining) | chunks]
      else
        chunks
      end

    Enum.reverse(chunks)
  end
end

compiled_modules = Evision.enabled_modules()

ExUnit.configure(
  exclude: [
    # exclude all tests that require downloading test data by default
    require_downloading: true,
    # exclude all tests that require ffmpeg by default
    # at the moment, this excludes video tests by default
    # as video module can compile without decoding capability
    # and there is perhaps no way to test input from a camera
    # (could set up a virtual camera, but let's leave that for now)
    require_ffmpeg: true,
    require_cuda: true,
    require_tesseract: true,
    # opens real windows
    highgui: true,
    wx: true,
    face: !compiled_modules.face,
    xfeatures2d: !compiled_modules.xfeatures2d,
    stitching: !compiled_modules.stitching,
    dnn: !compiled_modules.dnn,
    ml: !compiled_modules.ml,
    photo: !compiled_modules.photo,
    video: !compiled_modules.video
  ]
)

{:ok, _} = Application.ensure_all_started(:inets)
{:ok, _} = Application.ensure_all_started(:ssl)
Application.put_env(:evision, :progress_bar_enabled, false)
ExUnit.start()
