defmodule GrammarTest do
  use ExUnit.Case

  test "simplest strict" do
    defmodule MySimpleGrammarStrict do
      use Grammar

      rule start("hello"), do: "world"
    end

    assert {_, "world"} = MySimpleGrammarStrict.parse("hello")

    assert_raise RuntimeError, fn ->
      MySimpleGrammarStrict.parse("     ")
    end
  end

  test "simplest relaxed" do
    defmodule MySimpleGrammarRelaxed do
      use Grammar

      rule? start("hello"), do: "world"
    end

    assert {_, "world"} = MySimpleGrammarRelaxed.parse("hello")
    assert {_, nil} = MySimpleGrammarRelaxed.parse("")
  end

  test "two words and spaces" do
    defmodule MySimpleGrammar do
      use Grammar

      rule? start("hello", "world"), do: "hello world"
    end

    assert {_, "hello world"} =
             MySimpleGrammar.parse("""
               hello
                      world
             """)

    assert {_, nil} = MySimpleGrammar.parse("")

    assert_raise RuntimeError, fn ->
      MySimpleGrammar.parse("hello")
    end
  end
end
