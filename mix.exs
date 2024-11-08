defmodule Project do
  use Mix.Project

  def project do
    [
      app: :grammar,
      version: "0.1.0",
      elixirc_options: [warnings_as_errors: true],
      elixirc_paths: elixirc_paths(Mix.env()),
      consolidate_protocols: Mix.env() != :test,
      deps: deps(),
      aliases: aliases(),
      name: "Grammar",
      source_url: "https://github.com/nmichel/ex_grammar",
      docs: docs()
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  defp deps do
    [
      {:credo, "~> 1.7", only: [:dev, :test], runtime: false},
      {:dialyxir, "~> 1.4", only: [:dev, :test], runtime: false},
      {:ex_doc, "~> 0.31", only: :dev, runtime: false}
    ]
  end

  defp aliases do
    [
      check: [
        "format --check-formatted",
        "credo --strict",
        "dialyzer"
      ]
    ]
  end

  defp docs do
    [
      main: "Grammar",
      extras: ["README.md", "LICENSE.md"],
      groups_for_modules: [
        Internal: [~r/^Grammar.CodeGen/, Grammar.Tokenizer]
      ]
    ]
  end
end
