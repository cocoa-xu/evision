defmodule Evision.MixProject do
  use Mix.Project

  @version "0.1.0-dev"
  @opencv_version "4.5.4"
  @source_url "https://github.com/cocoa-xu/evision/tree/#{@opencv_version}"

  def project do
    [
      app: :evision,
      name: "Evision",
      version: @version,
      elixir: "~> 1.11-dev",
      deps: deps(),
      docs: docs(),
      compilers: [:elixir_make] ++ Mix.compilers(),
      elixirc_paths: elixirc_paths(Mix.env()),
      source_url: "https://github.com/cocox-xu/evision",
      description: description(),
      package: package(),
      make_env: %{
        "OPENCV_VER" => @opencv_version,
        "MAKE_BUILD_FLAGS" => System.get_env("MAKE_BUILD_FLAGS", "-j#{System.schedulers_online()}")}
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp elixirc_paths(_), do: ~w(lib)

  defp deps do
    [
      {:nx, "~> 0.1.0-dev", github: "elixir-nx/nx", branch: "main", sparse: "nx"},
      {:elixir_make, "~> 0.6"},
      {:ex_doc, "~> 0.23", only: :dev, runtime: false}
    ]
  end

  defp docs do
    [
      main: "OpenCV",
      source_ref: "v#{@version}",
      source_url: @source_url
    ]
  end

  defp description() do
    "OpenCV-Erlang/Elixir bindings."
  end

  defp package() do
    [
      name: "evision",
      # These are the default files included in the package
      files: ~w(lib c_src py_src nerves 3rd_party priv .formatter.exs mix.exs README* readme* LICENSE*
                license* CHANGELOG* changelog* src),
      licenses: ["Apache-2.0"],
      links: %{"GitHub" => "https://github.com/cocoa-xu/evision"}
    ]
  end
end
