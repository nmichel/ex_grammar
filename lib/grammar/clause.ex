defmodule Grammar.Clause do
  @moduledoc """
  A Clause entity represents a grammar clause, by its substitution and the block to execute.

  The substitution is a list of steps, which can be either a rule name or a token prototype.

  The parameter `function` is the code to execute when the clause is fully matched. In this function,
  the binding `params` is available and contains the results of the clause steps.
  """

  defstruct [:substitution, :function]

  @type substitution :: [term()]
  @type callback :: ([term()] -> term())

  @type t :: %__MODULE__{
          substitution: substitution(),
          function: callback()
        }

  @spec new(substitution(), callback()) :: t()
  def new(substitution, function) do
    %__MODULE__{substitution: substitution, function: function}
  end
end
