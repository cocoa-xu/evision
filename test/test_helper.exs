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

ExUnit.configure(
  exclude: [
    # exclude dnn tests by default as it will download a large (~200 MB) model file
    dnn: true,
    # exclude video tests by default as video module can compile without decoding capability
    # and there is perhaps no way to test input from a camera
    # (could set up a virtual camera, but let's leave that for now)
    video: true,
    # test OpenCV.Nx module if we have Nx module
    nx: !Code.ensure_loaded?(Nx)
    # for other OpenCV modules, use
    # compiled_modules = OpenCV.__enabled_modules__()
    # Enum.member?(compiled_modules, "<MODULE_NAME>"),
  ]
)

ExUnit.start()
