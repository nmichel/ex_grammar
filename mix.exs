defmodule Project do
  use Mix.Project

  def project do
    [
      app: :grammar,
      version: "0.0.1",
      elixirc_options: [warnings_as_errors: true],
    ]
  end
end
