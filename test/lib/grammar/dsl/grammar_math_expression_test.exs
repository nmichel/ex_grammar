defmodule Grammar.MathExpressionTest do
  use ExUnit.Case

  defmodule MathExpressionGrammar do
    use Grammar

    rule start(:expression) do
      [program] = params
      "#{program}"
    end

    rule expression(:term, :expression_cont) do
      join_non_nil(params)
    end

    rule? expression_cont("+", :term, :expression_cont) do
      [_, term, cont] = params
      join_non_nil(["plus", term, cont])
    end

    rule? expression_cont("-", :term, :expression_cont) do
      [_, term, cont] = params
      join_non_nil(["moins", term, cont])
    end

    rule term(:factor, :term_cont) do
      join_non_nil(params)
    end

    rule? term_cont("*", :factor, :term_cont) do
      [_, factor, cont] = params
      join_non_nil(["multiplié par", factor, cont])
    end

    rule? term_cont("/", :factor, :term_cont) do
      [_, factor, cont] = params
      join_non_nil(["divisé par", factor, cont])
    end

    rule factor(:number) do
      [number] = params
      number
    end

    rule factor("(", :expression, ")") do
      [_, expression, _] = params
      "(#{expression})"
    end

    rule number(~r/[0-9]+/) do
      [number] = params
      number
    end

    defp join_non_nil(params) when is_list(params) do
      params |> Enum.reject(&is_nil/1) |> Enum.join(" ")
    end
  end

  test "math expression grammar are parsed and rewriten" do
    assert {:ok, "1 plus 2"} = MathExpressionGrammar.parse("1 + 2")
    assert {:ok, "(1 plus 2)"} = MathExpressionGrammar.parse("(  1 +2 )   ")
    assert {:ok, "1 moins 2"} = MathExpressionGrammar.parse("1 - 2")
    assert {:ok, "1 moins (2 multiplié par (3 plus 4)) divisé par 5"} = MathExpressionGrammar.parse("1 - (2 * (3 + 4)) / 5")
  end

  test "catch incomplete expression" do
    assert {:error, {1, 4}, :no_clause_matched} = MathExpressionGrammar.parse("1 +")
  end

  test "catch invalid expression with valid token" do
    assert {:error, {1, 5}, :no_clause_matched} = MathExpressionGrammar.parse("1 + /")
  end

  test "catch invalid expression with invalid token" do
    assert {:error, {1, 5}, :no_clause_matched} = MathExpressionGrammar.parse("1 + toto")
  end
end
