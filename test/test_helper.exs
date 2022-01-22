defmodule OpenCV.TestHelper do
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
end

compiled_modules = OpenCV.__enabled_modules__()

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
    # test OpenCV.Nx module if we have Nx module
    nx: !Code.ensure_loaded?(Nx),
    photo: !Enum.member?(compiled_modules, "photo"),
    video: !Enum.member?(compiled_modules, "video")
  ]
)

ExUnit.start()
