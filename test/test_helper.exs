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
    # true by default as it will download a large (~200 MB) model file
    dnn: true
  ],
  include: [
    video: Enum.member?(compiled_modules, "video")
  ]
)

ExUnit.start()
