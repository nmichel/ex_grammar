defmodule Grammar.ProgramTest do
  use ExUnit.Case

  defmodule MyGrammar do
    use Grammar

    rule start(:program) do
      [program] = params
      "#{program}"
    end

    rule program(:assignment, :program_cont) do
      [assignment, cont] = params
      "#{assignment}\n#{cont}"
    end

    rule? program_cont(:assignment, :program_cont) do
      [assignment, cont] = params
      "#{assignment}\n#{cont}"
    end

    rule? assignment(:multi_line_comment) do
      [comment] = params
      "#{comment}\n"
    end

    rule? assignment(:identifier, "=", :rhv) do
      [identifier, _, expr] = params
      "#{identifier} égal à #{expr}"
    end

    rule rhv(:expression) do
      [expr] = params
      "#{expr}"
    end

    rule rhv(:string) do
      [string] = params
      "#{string}"
    end

    rule expression(:term, :expression_cont) do
      [term, cont] = params
      "#{term} #{cont}"
    end

    rule? expression_cont("+", :term, :expression_cont) do
      [_, term, cont] = params
      "plus #{term} #{cont}"
    end

    rule? expression_cont("-", :term, :expression_cont) do
      [_, term, cont] = params
      "moins #{term} #{cont}"
    end

    rule term(:factor, :term_cont) do
      [factor, cont] = params
      "#{factor} #{cont}"
    end

    rule? term_cont("*", :factor, :term_cont) do
      [_, factor, cont] = params
      "multiplié par #{factor} #{cont}"
    end

    rule? term_cont("/", :factor, :term_cont) do
      [_, factor, cont] = params
      "divisé par #{factor} #{cont}"
    end

    rule factor(:number) do
      [number] = params
      number
    end

    rule factor(:identifier) do
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

    rule identifier(~r/[a-zA-Z]+[a-zA-Z0-9]*/) do
      [string] = params
      string
    end

    rule string(~r/"[\S\s]*"/) do
      [string] = params
      string
    end

    rule multi_line_comment(~r/""".*?"""/s) do
      [string] = params
      string
    end
  end
end
