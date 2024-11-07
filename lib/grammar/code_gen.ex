defmodule Grammar.CodeGen.Clause do
  @moduledoc """
  A Clause entity represents a grammar clause, by its definition and the block to execute.

  The definition is a list of steps, which can be either a rule name or a token prototype.

  The block is the code to execute when the clause is fully matched. In this code block,
  the binding `params` is available and contains the results of the clause steps.
  """
  @enforce_keys [:def, :blk, :epsilon]
  defstruct def: nil, firsts: nil, blk: nil, epsilon: false

  def new(def, blk, epsilon) when is_list(def) and is_atom(epsilon) do
    struct(__MODULE__, def: def, blk: blk, epsilon: epsilon)
  end

  def epsilon?(%__MODULE__{epsilon: epsilon}), do: epsilon

  def set_firsts(%__MODULE__{firsts: nil} = clause, firsts) when is_list(firsts) do
    %{clause | firsts: firsts}
  end
end

defmodule Grammar.CodeGen.Rule do
  @moduledoc """
  A Rule entity represents a grammar rule, by its name and the list of clauses.
  """

  alias Grammar.CodeGen.Clause

  @enforce_keys [:name, :clauses]
  defstruct [:name, :clauses]

  def new(name) when is_atom(name) do
    struct(__MODULE__, name: name, clauses: [])
  end

  def new(name, def, blk, epsilon) when is_atom(name) and is_list(def) and def != [] and is_atom(epsilon) do
    struct(__MODULE__, name: name, clauses: [Clause.new(def, blk, epsilon)])
  end

  def add_clause(%__MODULE__{clauses: clauses} = rule, def, blk, epsilon) when is_list(def) and def != [] and is_atom(epsilon) do
    %{rule | clauses: clauses ++ [Clause.new(def, blk, epsilon)]}
  end

  def add_clause(%__MODULE__{clauses: clauses} = rule, %Clause{} = clause) do
    %{rule | clauses: clauses ++ [clause]}
  end

  def get_firsts(%__MODULE__{clauses: clauses}) do
    Enum.flat_map(clauses, & &1.firsts)
  end
end

defmodule Grammar.CodeGen do
  @moduledoc """
  This module exposes functions required to generate the code for the parser derived from grammar rules.
  """

  alias Grammar.CodeGen.Clause
  alias Grammar.CodeGen.Rule

  def check_all_rules_exist(rules) when is_map(rules) do
    Enum.map(rules, fn {name, %Rule{name: name, clauses: clauses}} ->
      Enum.map(clauses, fn %Clause{def: def} ->
        Enum.map(def, fn
          step when is_atom(step) and is_map_key(rules, step) -> []
          step when is_atom(step) -> [{name, step}]
          _step -> []
        end)
      end)
    end)
    |> List.flatten()
  end

  def check_rules_are_not_ambiguous(rules) do
    Enum.flat_map(rules, fn {rule_name, %Rule{name: rule_name, clauses: clauses}} ->
      clauses
      # Add positionnal index to clauses for easier identification
      |> Enum.with_index()
      # Map each first to its clause index
      |> Enum.flat_map(fn {%Clause{firsts: firsts}, id} ->
        Enum.map(firsts, &{&1, id})
      end)
      # Group by first
      |> Enum.group_by(&elem(&1, 0), &elem(&1, 1))
      # Remove duplicate ids (in case of ambiguous first, it propagates upstream)
      |> Enum.map(fn {first, clause_ids} -> {first, Enum.uniq(clause_ids)} end)
      # Keep only ambiguous firsts (i.e. appearing in more than one clause)
      |> Enum.filter(fn {_first, clause_ids} -> Enum.count(clause_ids) > 1 end)
      |> Enum.map(&Tuple.insert_at(&1, 0, rule_name))
    end)
    # filter out rules without conflicts
    |> Enum.filter(fn {_rule_name, _first, clause_ids} -> Enum.count(clause_ids) > 0 end)
  end

  def first_of_rules(rules) when is_map(rules) do
    Enum.reduce(rules, %{}, &firsts_of_rule(&1, &2, rules))
  end

  defp firsts_of_rule({name, %Rule{name: name}}, acc, _rules) when is_map_key(acc, name) do
    acc
  end

  defp firsts_of_rule({name, %Rule{name: name, clauses: clauses}}, acc, rules) do
    acc = Map.put(acc, name, Rule.new(name))
    Enum.reduce(clauses, acc, &firsts_of_clause(name, &1, &2, rules))
  end

  defp firsts_of_clause(rule_name, %Clause{def: [sub_rule_name | _], firsts: nil} = clause, acc, _rules)
       when is_atom(sub_rule_name) and is_map_key(acc, sub_rule_name) do
    # sub_rule already processed

    sub_rule_firsts = acc |> Map.get(sub_rule_name) |> Rule.get_firsts()
    updated_clause = Clause.set_firsts(clause, sub_rule_firsts)
    Map.update!(acc, rule_name, &Rule.add_clause(&1, updated_clause))
  end

  defp firsts_of_clause(rule_name, %Clause{def: [sub_rule_name | _], firsts: nil} = clause, acc, rules) when is_atom(sub_rule_name) do
    # sub_rule *not* already processed

    sub_rule = Map.get(rules, sub_rule_name)
    acc = firsts_of_rule({sub_rule_name, sub_rule}, acc, rules)
    firsts_of_clause(rule_name, clause, acc, rules)
  end

  defp firsts_of_clause(rule_name, %Clause{def: [first | _]} = clause, acc, _rules) do
    updated_clause = Clause.set_firsts(clause, [first])
    rule = Map.get(acc, rule_name) || Rule.new(rule_name)
    rule = Rule.add_clause(rule, updated_clause)
    Map.put(acc, rule_name, rule)
  end

  def store_clause(module, rule_name, _meta, def, blk, epsilon) do
    def = Macro.escape(def)
    blk = Macro.escape(blk)

    quote do
      rules = Module.get_attribute(unquote(module), :rules)
      if rules == %{}, do: Module.put_attribute(unquote(module), :start_rule_name, unquote(rule_name))

      rules =
        Map.update(
          rules,
          unquote(rule_name),
          Rule.new(unquote(rule_name), unquote(def), unquote(blk), unquote(epsilon)),
          &Rule.add_clause(&1, unquote(def), unquote(blk), unquote(epsilon))
        )

      Module.put_attribute(unquote(module), :rules, rules)
    end
  end

  def build_production_for_rules(rules) when is_map(rules) do
    Enum.map(rules, &build_production_for_rule(&1))
  end

  defp build_production_for_rule({name, %Rule{name: name, clauses: clauses} = rule}) do
    epsilon? = Enum.any?(clauses, &Clause.epsilon?/1)
    no_clause = (epsilon? && epsilon_clause()) || no_clause()
    nested_clauses = Enum.reduce(clauses, what_are_you_doing_here_clause(), &build_production_for_clause(&1, &2))
    firsts = Rule.get_firsts(rule)

    quote do
      def unquote(name)(%Tokenizer{} = tokenizer) do
        case Tokenizer.current_token(tokenizer, unquote(firsts)) do
          {{nil, cursor}, tokenizer} ->
            unquote(no_clause)

          {{current_token, cursor}, tokenizer} ->
            unquote(nested_clauses)
        end
      end
    end
  end

  defp build_production_for_clause(%Clause{firsts: firsts, blk: blk} = clause, nested_ast) do
    quote do
      if Enum.any?(unquote(firsts), &TokenExtractor.match?(&1, current_token)) do
        fun_in = []

        unquote_splicing(build_production_code_for_clause(clause))

        fun_out =
          (fn var!(params) ->
             _ = var!(params)
             unquote(blk)
           end).(fun_in)

        {tokenizer, fun_out}
      else
        unquote(nested_ast)
      end
    end
  end

  defp build_production_code_for_clause(%Clause{def: def}) when is_list(def) do
    Enum.map(def, fn elem ->
      ast = build_production_code_for_clause_elem(elem)

      quote do
        {tokenizer, res} = unquote(ast)
        fun_in = fun_in ++ [res]
      end
    end)
  end

  defp build_production_code_for_clause_elem(rule_name) when is_atom(rule_name) do
    quote do
      unquote(rule_name)(tokenizer)
    end
  end

  defp build_production_code_for_clause_elem(matcher) do
    quote do
      case Tokenizer.next_token(tokenizer, unquote(matcher)) do
        {{nil, cursor}, tokenizer} ->
          throw({cursor, :no_token})

        {{token, cursor}, tokenizer} ->
          {tokenizer, token}
      end
    end
  end

  defp no_clause do
    quote do
      throw({cursor, :no_clause_matched})
    end
  end

  defp epsilon_clause do
    quote do
      {tokenizer, nil}
    end
  end

  defp what_are_you_doing_here_clause do
    quote do
      throw({cursor, :what_are_you_doing_here})
    end
  end
end
