defmodule Grammar.Clause do
  @moduledoc """
  A Clause entity represents a grammar clause, by its substitution and callback to execute.

  The substitution is a list of steps, which can be either a rule name or a token prototype.

  The parameter `function` is the callback to execute when the clause is fully matched. It is
  given a list as parameter, each element of that list is the value produced by the substitution
  of each term.
  """

  defstruct [:substitution, :function]

  @type substitution :: [term()]
  @type callback :: ([term()] -> term())

  @type t :: %__MODULE__{
          substitution: substitution(),
          function: callback()
        }

  @doc """
  Create a new clause with a given substitution and callback function.
  """
  @spec new(substitution(), callback()) :: t()
  def new(substitution, function) do
    %__MODULE__{substitution: substitution, function: function}
  end
end
