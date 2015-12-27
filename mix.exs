defmodule Liquid.Mixfile do
  use Mix.Project

  def project do
    [ app: :liquid,
      version: "0.0.3",
      elixir: "~> 1.0",
      deps: deps,
      name: "Liquid",
      source_url: "https://github.com/nulian/liquid-elixir"]
  end

  # Configuration for the OTP application
  def application do
    []
  end

  # Returns the list of dependencies in the format:
  # { :foobar, "0.1", git: "https://github.com/elixir-lang/foobar.git" }
  defp deps do
    [{:credo, "~> 0.2", only: [:dev, :test]}]
  end
end
