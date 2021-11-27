defmodule Evision.MixProject do
  use Mix.Project

  @version "0.1.0-dev"

  def project do
    [
      app: :evision,
      name: "Evision",
      version: @version,
      elixir: "~> 1.12-dev",
      deps: deps(),
      docs: docs(),
      compilers: [:elixir_make] ++ Mix.compilers(),
      elixirc_paths: elixirc_paths(Mix.env())
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # mostly copy and paste from https://github.com/elixir-nx/nx/blob/9f87aa188cc0b1c29146900a4fe1f9c9dbb64b78/torchx/mix.exs
  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  defp deps do
    [
      {:elixir_make, "~> 0.6"},
      {:ex_doc, "~> 0.23", only: :dev}
    ]
  end

  defp docs do
    [
      main: "Evision",
      source_ref: "v#{@version}",
      source_url: "https://github.com/cocoa-xu/evision"
    ]
  end
end
