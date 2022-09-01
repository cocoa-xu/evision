defmodule Evision.TestHelper do
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

compiled_modules = Evision.__enabled_modules__()

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
    # exclude dnn tests by default as it will download a large (~200 MB) model file
    dnn: !Enum.member?(compiled_modules, "dnn"),
    ml: !Enum.member?(compiled_modules, "ml"),
    # test Cv.Nx module if we have Nx module
    nx: !Code.ensure_loaded?(Nx),
    photo: !Enum.member?(compiled_modules, "photo"),
    video: !Enum.member?(compiled_modules, "video")
  ]
)

{:ok, _} = Application.ensure_all_started(:inets)
{:ok, _} = Application.ensure_all_started(:ssl)
ExUnit.start()
