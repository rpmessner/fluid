defmodule Liquescent.Mixfile do
  use Mix.Project

  def project do
    [ app: :liquiscent,
      version: "0.0.3",
      elixir: "~> 1.0",
      deps: deps,
      name: "Liquescent",
      source_url: "https://github.com/nulian/liquiscent"]
  end

  # Configuration for the OTP application
  def application do
    []
  end

  # Returns the list of dependencies in the format:
  # { :foobar, "0.1", git: "https://github.com/elixir-lang/foobar.git" }
  defp deps do
    []
  end
end
