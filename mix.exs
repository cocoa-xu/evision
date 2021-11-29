defmodule Evision.MixProject do
  use Mix.Project

  @version "0.1.0-dev"

  def project do
    [
      app: :evision,
      name: "Evision",
      version: @version,
      elixir: "~> 1.11-dev",
      deps: deps(),
      docs: docs(),
      compilers: [:elixir_make] ++ Mix.compilers(),
      elixirc_paths: elixirc_paths(Mix.env())
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

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
