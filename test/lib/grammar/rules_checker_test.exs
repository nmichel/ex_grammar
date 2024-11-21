defmodule Grammar.RulesCheckerTest do
  use ExUnit.Case

  alias Grammar.RulesChecker

  test "detect missing rules on simple rules" do
    assert {:error, :missing_rules, [a: :b]} =
             Grammar.new()
             |> Grammar.add_clause(:a, [:b], & &1)
             |> RulesChecker.check_all_rules_exist()

    assert {:error, :missing_rules, [b: :c, d: :c]} =
             Grammar.new()
             |> Grammar.add_clause(:a, [:b], & &1)
             |> Grammar.add_clause(:b, [:c, :d], & &1)
             |> Grammar.add_clause(:d, [:c, :a], & &1)
             |> RulesChecker.check_all_rules_exist()
  end

  test "detect missing rules on multi-clause rules" do
    assert {:error, :missing_rules, [a: :c]} =
             Grammar.new()
             |> Grammar.add_clause(:a, [:b], & &1)
             |> Grammar.add_clause(:a, [:c], & &1)
             |> Grammar.add_clause(:b, ["foo"], & &1)
             |> RulesChecker.check_all_rules_exist()

    assert {:error, :missing_rules, [a: :b, a: :c]} =
             Grammar.new()
             |> Grammar.add_clause(:a, [:b], & &1)
             |> Grammar.add_clause(:a, [:c], & &1)
             |> RulesChecker.check_all_rules_exist()

    assert {:error, :missing_rules, [b: :c, d: :c, d: :e, d: :f]} =
             Grammar.new()
             |> Grammar.add_clause(:a, [:b], & &1)
             |> Grammar.add_clause(:b, [:c, :d], & &1)
             |> Grammar.add_clause(:d, [:c, :a], & &1)
             |> Grammar.add_clause(:d, [:e, :f], & &1)
             |> RulesChecker.check_all_rules_exist()
  end

  test "detect left recursion (cycles) in simple rule definitions" do
    assert {:error, :cycles_found, [[:a, :a]]} =
             Grammar.new()
             |> Grammar.add_clause(:a, [:a], & &1)
             |> RulesChecker.check_rules_are_non_left_recursive()

    assert {:error, :cycles_found, [[:a, :b, :a], [:b, :a, :b]]} =
             Grammar.new()
             |> Grammar.add_clause(:a, [:b, :a], & &1)
             |> Grammar.add_clause(:b, [:a, :b], & &1)
             |> RulesChecker.check_rules_are_non_left_recursive()

    assert {:error, :cycles_found, [[:a, :b, :c, :d, :a], [:b, :c, :d, :a, :b], [:c, :d, :a, :b, :c], [:d, :a, :b, :c, :d]]} =
             Grammar.new()
             |> Grammar.add_clause(:a, [:b, :a], & &1)
             |> Grammar.add_clause(:b, [:c, :b], & &1)
             |> Grammar.add_clause(:c, [:d, :b], & &1)
             |> Grammar.add_clause(:d, [:a, :b], & &1)
             |> RulesChecker.check_rules_are_non_left_recursive()
  end

  test "detect left recursion (cycles) in multi-clause rule definitions" do
    assert {:error, :cycles_found, [[:a, :c, :a], [:a, :b, :a], [:b, :a, :c, :a], [:b, :a, :b], [:c, :a, :c], [:c, :a, :b, :a]]} =
             Grammar.new()
             |> Grammar.add_clause(:a, [:b, :a], & &1)
             |> Grammar.add_clause(:a, [:c, :a], & &1)
             |> Grammar.add_clause(:b, [:a, :b], & &1)
             |> Grammar.add_clause(:c, [:a, :b], & &1)
             |> RulesChecker.check_rules_are_non_left_recursive()
  end

  test "ignore recursion when not on the first term" do
    assert :ok =
             Grammar.new()
             |> Grammar.add_clause(:a, [:b, :a], & &1)
             |> Grammar.add_clause(:b, [:c, :b], & &1)
             |> Grammar.add_clause(:c, ["foo", :c], & &1)
             |> RulesChecker.check_rules_are_non_left_recursive()
  end

  test "detect ambiguities in rule firsts" do
    assert {:error, :ambiguities_found, [{:a, "foo", 2}]} =
             Grammar.new()
             |> Grammar.add_clause(:a, ["foo", :b], & &1)
             |> Grammar.add_clause(:a, ["foo", :c], & &1)
             |> Grammar.add_clause(:b, ["bar"], & &1)
             |> Grammar.add_clause(:c, ["neh"], & &1)
             |> Grammar.compute_first()
             |> RulesChecker.check_rules_are_not_ambiguous()

    assert {:error, :ambiguities_found, [{:a, "foo", 2}]} =
             Grammar.new()
             |> Grammar.add_clause(:a, [:b], & &1)
             |> Grammar.add_clause(:a, [:c], & &1)
             |> Grammar.add_clause(:b, ["foo"], & &1)
             |> Grammar.add_clause(:c, [:d, "foo"], & &1)
             |> Grammar.add_clause(:d, ["foo"], & &1)
             |> Grammar.compute_first()
             |> RulesChecker.check_rules_are_not_ambiguous()

    assert {:error, :ambiguities_found, [{:a, ~r/foo/, 2}]} =
             Grammar.new()
             |> Grammar.add_clause(:a, [:b], & &1)
             |> Grammar.add_clause(:a, [:c], & &1)
             |> Grammar.add_clause(:b, [~r/foo/], & &1)
             |> Grammar.add_clause(:c, [:d, "foo"], & &1)
             |> Grammar.add_clause(:d, [~r/foo/], & &1)
             |> Grammar.compute_first()
             |> RulesChecker.check_rules_are_not_ambiguous()

    assert :ok =
             Grammar.new()
             |> Grammar.add_clause(:a, [:b], & &1)
             |> Grammar.add_clause(:a, [:c], & &1)
             |> Grammar.add_clause(:b, ["foo"], & &1)
             |> Grammar.add_clause(:c, [:d, ~r/foo/], & &1)
             |> Grammar.add_clause(:d, [~r/foo/], & &1)
             |> Grammar.compute_first()
             |> RulesChecker.check_rules_are_not_ambiguous()
  end
end
