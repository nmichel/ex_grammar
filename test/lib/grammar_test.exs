defmodule GrammarTest do
  use ExUnit.Case

  doctest Grammar

  alias Grammar.SpecialTokens.Number
  alias Grammar.Tokenizer

  test "strict matching of token" do
    grammar =
      Grammar.new()
      |> Grammar.add_clause(:start, [%Number{number: 42}, %Number{}], fn [a, b] -> {a, b} end)
      |> Grammar.prepare!()
      |> Grammar.start(:start)

    # second number is loose, so it will match anything
    assert {:ok, {%Number{number: 42}, %Number{number: 42}}} = Grammar.loop(grammar, Tokenizer.new("42 42"))
    assert {:ok, {%Number{number: 42}, %Number{number: 1000}}} = Grammar.loop(grammar, Tokenizer.new("42 1000"))

    # first number is strict, so it will match only 42
    assert {:error, {1, 1}, :no_clause_matched} = Grammar.loop(grammar, Tokenizer.new("43 1000"))
  end

  test "matching prototypes and extracted tokens" do
    grammar =
      Grammar.new()
      # ordering of clauses is important ! if :non_contant is before constant, it will always match
      |> Grammar.add_clause(:choose, [:constant], fn [value] -> {:constant, value} end)
      |> Grammar.add_clause(:choose, [:non_constant], fn [value] -> {:non_constant, value} end)
      |> Grammar.add_clause(:constant, [%Number{number: 12}], &Enum.at(&1, 0))
      |> Grammar.add_clause(:non_constant, [%Number{}], &Enum.at(&1, 0))
      |> Grammar.prepare!()
      |> Grammar.start(:choose)

    assert {:ok, {:non_constant, %Number{number: 123}}} = Grammar.loop(grammar, Tokenizer.new("123"))
    assert {:ok, {:constant, %Number{number: 12}}} = Grammar.loop(grammar, Tokenizer.new("12"))
  end

  test "tokens drive through rules" do
    grammar =
      Grammar.new()
      |> Grammar.add_clause(:foo, [:bar], fn [value] -> {:foo_bar, value} end)
      |> Grammar.add_clause(:foo, [:neh], fn [value] -> {:foo_neh, value} end)
      |> Grammar.add_clause(:foo, [%Number{}], fn [value] -> {:foo_number, value} end)
      |> Grammar.add_clause(:bar, ["bar1"], fn ["bar1" = value] -> value end)
      |> Grammar.add_clause(:bar, ["bar2"], fn ["bar2" = value] -> value end)
      |> Grammar.add_clause(:bar, [~r/bar[0-9]+/], fn [value] -> "caught #{value}" end)
      |> Grammar.add_clause(:neh, ["neh1"], fn ["neh1" = value] -> value end)
      |> Grammar.add_clause(:neh, ["neh2"], fn ["neh2" = value] -> value end)
      |> Grammar.add_clause(:neh, [%Number{number: 12}], fn [value] -> value end)
      |> Grammar.prepare!()
      |> Grammar.start(:foo)

    assert grammar.firsts == %{
             foo: [
               ["bar1", "bar2", ~r/bar[0-9]+/],
               ["neh1", "neh2", %Number{number: 12}],
               [%Number{}]
             ],
             bar: [["bar1"], ["bar2"], [~r/bar[0-9]+/]],
             neh: [["neh1"], ["neh2"], [%Number{number: 12}]]
           }

    assert {:ok, {:foo_bar, "bar1"}} = Grammar.loop(grammar, Tokenizer.new("bar1"))
    assert {:ok, {:foo_bar, "bar2"}} = Grammar.loop(grammar, Tokenizer.new("bar2"))
    assert {:ok, {:foo_bar, "caught bar42"}} = Grammar.loop(grammar, Tokenizer.new("bar42"))

    assert {:ok, {:foo_neh, "neh1"}} = Grammar.loop(grammar, Tokenizer.new("neh1"))
    assert {:ok, {:foo_neh, "neh2"}} = Grammar.loop(grammar, Tokenizer.new("neh2"))
    assert {:ok, {:foo_neh, %Number{number: 12}}} = Grammar.loop(grammar, Tokenizer.new("12"))

    assert {:ok, {:foo_number, %Number{number: 13}}} = Grammar.loop(grammar, Tokenizer.new("13"))
  end

  test "ambiguities on token are resolved by clauses ordering" do
    assert {:ok, "ident foo"}

    Grammar.new()
    |> Grammar.add_clause(:foo, [:ident], fn [ident] -> "ident #{ident}" end)
    |> Grammar.add_clause(:foo, ["foo"], fn _ -> "foo" end)
    |> Grammar.add_clause(:ident, [~r/[a-z]+/], & &1)
    |> Grammar.prepare!()
    |> Grammar.start(:foo)
    |> Grammar.loop(Tokenizer.new("foo"))

    assert {:ok, "foo"}

    Grammar.new()
    |> Grammar.add_clause(:foo, ["foo"], fn _ -> "foo" end)
    |> Grammar.add_clause(:foo, [:ident], fn [ident] -> "ident #{ident}" end)
    |> Grammar.add_clause(:ident, [~r/[a-z]+/], & &1)
    |> Grammar.prepare!()
    |> Grammar.start(:foo)
    |> Grammar.loop(Tokenizer.new("foo"))
  end
end
