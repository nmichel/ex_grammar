defmodule Project do
  use Mix.Project

  @version "0.3.0"

  def project do
    [
      app: :grammar,
      version: @version,
      elixirc_options: elixirc_options(Mix.env()),
      elixirc_paths: elixirc_paths(Mix.env()),
      consolidate_protocols: Mix.env() != :test,
      deps: deps(),
      aliases: aliases(),
      name: "Grammar",
      source_url: "https://github.com/nmichel/ex_grammar",
      docs: docs(),
      description: description(),
      package: package()
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  defp elixirc_options(:dev), do: [warnings_as_errors: false]
  defp elixirc_options(_), do: [warnings_as_errors: true]

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
      main: "README",
      extras: [
        "README.md",
        "LICENSE.md",
        "CHANGELOG.md",
        "examples/dsl/math_to_french.md",
        "examples/dsl/math_to_function.md",
        "examples/dsl/ip_list.md",
        "notebooks/dsl_intro.livemd",
        "notebooks/grammar_intro.livemd",
        "notebooks/musing_with_codegen.livemd"
      ],
      groups_for_extras: [
        "DSL Examples": Path.wildcard("examples/dsl/*.md"),
        Livebooks: Path.wildcard("notebooks/*.livemd")
      ],
      groups_for_modules: [
        Internal: [Grammar.Clause, Grammar.Rule, Grammar.RulesChecker],
        "Code generation": [~r/^Grammar.CodeGen/]
      ],
      source_ref: "v#{@version}"
    ]
  end

  defp description do
    """
    A simple DSL to define parsers / transformers for LL(1) structured inputs.
    """
  end

  defp package do
    [
      licenses: ["MIT"],
      maintainers: ["Nicolas Michel"],
      links: %{"GitHub" => "https://github.com/nmichel/ex_grammar"}
    ]
  end
end
