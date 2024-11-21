defmodule Grammar.Clause do
  @moduledoc """
  TODO:
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
