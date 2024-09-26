defmodule GrammarTest do
  use ExUnit.Case

  test "simplest" do
    defmodule MySimpleGrammar do
      use Grammar

      rule! start("hello"), do: "world"
    end

    assert {_, "world"} = MySimpleGrammar.parse("hello")
  end

  test "two words and spaces" do
    defmodule MySimpleGrammar do
      use Grammar

      rule! start("hello", "world"), do: "hello world"
    end

    assert {_, "hello world"} = MySimpleGrammar.parse("""
      hello
             world
    """)
  end
end
