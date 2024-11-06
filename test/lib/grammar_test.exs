defmodule GrammarTest do
  use ExUnit.Case
  doctest Grammar

  test "simplest strict" do
    defmodule MySimpleGrammarStrict do
      use Grammar

      rule start("hello"), do: "world"
    end

    assert {:ok, "world"} = MySimpleGrammarStrict.parse("hello")

    assert {:error, {1, 6}, :no_clause_matched} = MySimpleGrammarStrict.parse("     ")
  end

  test "simplest relaxed" do
    defmodule MySimpleGrammarRelaxed do
      use Grammar

      rule? start("hello"), do: "world"
    end

    assert {:ok, "world"} = MySimpleGrammarRelaxed.parse("hello")
    assert {:ok, nil} = MySimpleGrammarRelaxed.parse("")
  end

  test "two words and spaces" do
    defmodule MySimpleGrammar do
      use Grammar

      rule? start("hello", "world"), do: "hello world"
    end

    assert {:ok, "hello world"} =
             MySimpleGrammar.parse("""
               hello
                      world
             """)

    assert {:ok, nil} = MySimpleGrammar.parse("")

    assert {:error, {1, 6}, :no_token} = MySimpleGrammar.parse("hello")
  end

  test "spaces are meaningful when not dropped" do
    defmodule MySimpleGrammar do
      use Grammar, drop_spaces: false

      rule start("hello", "world"), do: "hello world"
    end

    assert {:error, {1, 1}, :no_clause_matched} =
             MySimpleGrammar.parse("""
               hello
                      world
             """)
  end

  test "spaces are meaningful when not dropped (relaxed version)" do
    defmodule MySimpleGrammar do
      use Grammar, drop_spaces: false

      rule? start("hello", "world"), do: "hello world"
    end

    assert {:ok, nil} =
             MySimpleGrammar.parse("""
               hello
                      world
             """)
  end
end
