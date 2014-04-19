defmodule T3Web.Mixfile do
  use Mix.Project

  def project do
    [ app: :t3_web,
      version: "0.0.1",
      elixir: "~> 0.12.4 or ~> 0.13.0-dev",
      deps: deps ]
  end

  def application do
    [
      applications: [:gproc],
      mod: { T3Web, [] }
    ]
  end

  defp deps do
    [
      {:jazz, github: "meh/jazz" },
      {:gproc, github: "uwiger/gproc" },
      {:phoenix, github: "phoenixframework/phoenix"}
    ]
  end
end
