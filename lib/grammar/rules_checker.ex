defmodule Grammar.RulesChecker do
  @moduledoc """
  This module exports functions to check the correctness of the grammar rules.
  """

  alias Grammar.Clause
  alias Grammar.Rule

  @type path :: [atom()]
  @type miss :: {atom(), atom()}
  @type ambiguity :: {atom(), term()}

  @typep stack :: [term()]

  @doc """
  Check if all rules exist in the grammar, i.e. a rule referred to in a substitution list is
  actually defined.
  """
  @spec check_all_rules_exist(Grammar.t()) :: :ok | {:error, :missing_rules, [miss()]}
  def check_all_rules_exist(%Grammar{rules: rules}) do
    Enum.map(rules, fn {name, %Rule{name: name, clauses: clauses}} ->
      Enum.map(clauses, fn %Clause{substitution: substitution} ->
        Enum.map(substitution, fn
          step when is_atom(step) and is_map_key(rules, step) -> []
          step when is_atom(step) -> [{name, step}]
          _step -> []
        end)
      end)
    end)
    |> List.flatten()
    |> case do
      [] -> :ok
      misses -> {:error, :missing_rules, misses}
    end
  end

  @doc """
  Check if the rules are non-left-recursive.
  """
  @spec check_rules_are_non_left_recursive(Grammar.t()) :: :ok | {:error, :cycles_found, [path()]}
  def check_rules_are_non_left_recursive(%Grammar{rules: rules}) do
    rules
    |> Enum.map(&elem(&1, 1))
    |> Enum.sort(&(&1.name < &2.name))
    |> Enum.flat_map(&check_rule(&1, rules))
    |> case do
      [] -> :ok
      cycles -> {:error, :cycles_found, cycles}
    end
  end

  @spec check_rule(Rule.t(), Grammar.rules()) :: [path()]
  defp check_rule(%Rule{} = rule, rules), do: check_rule(rule, [], rules, [])

  @spec check_rule(Rule.t(), [path()], Grammar.rules(), stack()) :: [path()]
  defp check_rule(%Rule{} = rule, acc, rules, stack) do
    Enum.reduce(rule.clauses, acc, &check_clause(&1, &2, rules, [rule.name | stack]))
  end

  @spec check_clause(Clause.t(), [path()], Grammar.rules(), stack()) :: [path()]
  defp check_clause(%Clause{substitution: [head | _tail]}, acc, rules, stack) when is_atom(head) do
    if Enum.any?(stack, &(&1 === head)) do
      [Enum.reverse([head | stack]) | acc]
    else
      check_rule(Map.get(rules, head), acc, rules, stack)
    end
  end

  defp check_clause(%Clause{}, acc, _rules, _stack), do: acc

  @doc """
  Check if the rules are not ambiguous, i.e. a given token drives only to one reduction.
  """
  @spec check_rules_are_not_ambiguous(Grammar.t()) :: :ok | {:error, :ambiguities_found, [String.t()]}
  def check_rules_are_not_ambiguous(%Grammar{firsts: firsts}) do
    firsts
    |> Enum.flat_map(fn {rule_name, rule_firsts} ->
      rule_firsts
      |> Enum.concat()
      |> Enum.frequencies()
      |> Enum.reject(&(elem(&1, 1) == 1))
      |> Enum.map(&Tuple.insert_at(&1, 0, rule_name))
    end)
    |> case do
      [] -> :ok
      conflicts -> {:error, :ambiguities_found, conflicts}
    end
  end

  @doc """
  Express an error returned by a checker function in a human-readable form.
  """
  @spec stringify_error(:missing_rules, [miss()]) :: String.t()
  def stringify_error(:missing_rules, misses) do
    map_join_lines(misses, fn {name, step} -> ~s/Rule '#{name}' misses rule '#{step}'/ end)
  end

  @spec stringify_error(:cycles_found, [path()]) :: String.t()
  def stringify_error(:cycles_found, paths) do
    map_join_lines(paths, fn path -> ~s/Cycle found: #{Enum.join(path, " -> ")}/ end)
  end

  @spec stringify_error(:ambiguities_found, [ambiguity()]) :: String.t()
  def stringify_error(:ambiguities_found, ambiguities) do
    map_join_lines(ambiguities, fn {rule_name, first, _count} -> ~s/Rule '#{rule_name}' has ambiguous first '#{inspect(first)}'/ end)
  end

  @spec map_join_lines([term()], (term() -> String.t())) :: String.t()
  defp map_join_lines(data, stringifier_fn), do: Enum.map_join(data, "\n", stringifier_fn)
end
