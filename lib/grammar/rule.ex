defmodule Grammar.Rule do
  @moduledoc """
  A `Rule` entity represents a single rule in the grammar.

  It has a name, a list of mutually exclusive clauses
  and `epsilon` flag indicating if the rule is mandatory or not.
  """
  defstruct [:name, :clauses, :epsilon?]

  alias Grammar.Clause

  @type name() :: atom()

  @type t :: %__MODULE__{
          name: name(),
          clauses: [Clause.t()],
          epsilon?: boolean()
        }

  @doc """
  Create a new empty rule with a given name and epsilon flag.
  """
  @spec new(atom(), boolean()) :: t()
  def new(name, epsilon? \\ false) when is_atom(name) do
    %__MODULE__{name: name, epsilon?: epsilon?, clauses: []}
  end

  @doc """
  Append a clause to the rule.

  Clauses must be mutually exclusive, and must added in the order of
  their priority if ambiguity during token extraction is possible (which must
  be avoided as much as possible).
  """
  @spec add_clause(t(), Clause.t()) :: t()
  def add_clause(%__MODULE__{} = rule, %Clause{} = clause) do
    %{rule | clauses: rule.clauses ++ [clause]}
  end

  @doc """
  Return `true` is the rule is not mandatory.

  If a `Rule` is optional, and so `epsilon` is true, it is reduced to `nil` during the parsing
  if not entered.
  """
  @spec epsilon?(t()) :: boolean()
  def epsilon?(%__MODULE__{} = rule), do: rule.epsilon?
end
