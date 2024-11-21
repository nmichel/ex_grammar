defmodule Grammar.Rule do
  @moduledoc """
  TODO:
  """
  defstruct [:name, :clauses, :epsilon?]

  alias Grammar.Clause

  @type name() :: atom()

  @type t :: %__MODULE__{
          name: name(),
          clauses: [Clause.t()],
          epsilon?: boolean()
        }

  @spec new(atom(), boolean()) :: t()
  def new(name, epsilon? \\ false) when is_atom(name) do
    %__MODULE__{name: name, epsilon?: epsilon?, clauses: []}
  end

  @spec add_clause(t(), Clause.t()) :: t()
  def add_clause(%__MODULE__{} = rule, %Clause{} = clause) do
    %{rule | clauses: rule.clauses ++ [clause]}
  end

  @spec epsilon?(t()) :: boolean()
  def epsilon?(%__MODULE__{} = rule), do: rule.epsilon?
end
