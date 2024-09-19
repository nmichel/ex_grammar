defmodule Tokenizer do
  defstruct tokens: []

  def new(input) when is_binary(input) do
    struct(__MODULE__, tokens: String.split(input, ~r/\s+/, trim: true))
  end

  def current_token(%__MODULE__{tokens: [token | tail]} = tokenizer) do
    {token, tokenizer}
  end

  def current_token(%__MODULE__{tokens: []} = tokenizer) do
    {nil, tokenizer}
  end

  def next_token(%__MODULE__{tokens: [token | tail]} = tokenizer) do
    {token, struct(tokenizer, tokens: tail)}
  end

  def next_token(%__MODULE__{tokens: []} = tokenizer) do
    {nil, tokenizer}
  end
end

defmodule Grammar do
  defmodule Clause do
    @enforce_keys [:def]
    defstruct def: nil, firsts: nil

    def new(def) when is_list(def) do
      struct(__MODULE__, def: def)
    end

    def set_firsts(%__MODULE__{firsts: nil} = clause, firsts) when is_list(firsts) do
      %{clause | firsts: firsts}
    end
  end

  defmodule Rule do
    @enforce_keys [:name, :clauses]
    defstruct [:name, :clauses]

    def new(name) when is_atom(name) do
      struct(__MODULE__, name: name, clauses: [])
    end

    def new(name, def) when is_atom(name) and is_list(def) do
      struct(__MODULE__, name: name, clauses: [Clause.new(def)])
    end

    def add_clause(%__MODULE__{clauses: clauses} = rule, def) when is_list(def) do
      %{rule | clauses: clauses ++ [Clause.new(def)]}
    end

    def add_clause(%__MODULE__{clauses: clauses} = rule, %Clause{} = clause) do
      %{rule | clauses: clauses ++ [clause]}
    end

    def get_firsts(%__MODULE__{clauses: clauses}) do
      Enum.flat_map(clauses, & &1.firsts)
    end
  end

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
        Enum.map(firsts, & {&1, id})
      end)
      # Group by first
      |> Enum.group_by(& elem(&1, 0), & elem(&1, 1))
      # Remove duplicate ids (in case of ambiguous first, it propagates upstream)
      |> Enum.map(fn {first, clause_ids} -> {first, Enum.uniq(clause_ids)} end)
      # Keep only ambiguous firsts (i.e. appearing in more than one clause)
      |> Enum.filter(fn {_first, clause_ids} -> Enum.count(clause_ids) > 1 end)
      |> Enum.map(& Tuple.insert_at(&1, 0, rule_name))
    end)
    # filter out rules without conflicts
    |> Enum.filter(fn {rule_name, _first, clause_ids} -> Enum.count(clause_ids) > 0 end)
  end

  def first_of_rules(rules) when is_map(rules) do
    Enum.reduce(rules, %{}, &firsts_of_rule(&1, &2, rules))
  end

  def firsts_of_rule({name, %Rule{name: name}}, acc, rules) when is_map_key(acc, name) do
    acc
  end

  def firsts_of_rule({name, %Rule{name: name, clauses: clauses}} = rule, acc, rules) do
    acc = Map.put(acc, name, Rule.new(name))
    Enum.reduce(clauses, acc, &firsts_of_clause(name, &1, &2, rules))

    # TODO Check firsts do not overlap
    # if current_rule_firsts && Enum.any?(sub_rule_firsts, & &1 in current_rule_firsts) do
    #   raise "Firsts from #{sub_rule_name} already found for rule #{rule_name}"
    # end
  end

  def firsts_of_clause(rule_name, %Clause{def: [sub_rule_name | _], firsts: nil} = clause, acc, _rules) when is_atom(sub_rule_name) and is_map_key(acc, sub_rule_name) do
    # sub_rule already processed

    sub_rule_firsts = acc |> Map.get(sub_rule_name) |> Rule.get_firsts()
    updated_clause = Clause.set_firsts(clause, sub_rule_firsts)
    Map.update!(acc, rule_name, & Rule.add_clause(&1, updated_clause))
  end

  def firsts_of_clause(rule_name, %Clause{def: [sub_rule_name | _], firsts: nil} = clause, acc, rules) when is_atom(sub_rule_name) do
    # sub_rule *not* already processed

    sub_rule = Map.get(rules, sub_rule_name)
    acc = firsts_of_rule({sub_rule_name, sub_rule}, acc, rules)
    firsts_of_clause(rule_name, clause, acc, rules)
  end

  def firsts_of_clause(rule_name, %Clause{def: [first | _]} = clause, acc, _rules) do
    updated_clause= Clause.set_firsts(clause, [first])
    Map.update(acc, rule_name, Rule.new(rule_name, [first]), & Rule.add_clause(&1, updated_clause))
  end

  # def build_matcher_for(c) when is_binary(c), do: & &1 === c
  # def build_matcher_for(%Regex{} = r), do: & Regex.match?(r, &1)

  defmacro rule({name, _meta, def}, do: _blk) when is_atom(name) do
    quote do
      rules = Module.get_attribute(unquote(__CALLER__.module), :rules)
      if rules == %{}, do: Module.put_attribute(unquote(__CALLER__.module), :start_rule_name, unquote(name))
      rules = Map.update(rules, unquote(name), Rule.new(unquote(name), unquote(def)), & Rule.add_clause(&1, unquote(def)))
      Module.put_attribute(unquote(__CALLER__.module), :rules, rules)
    end
  end

  defmacro __using__(_opts) do
    quote do
      Module.put_attribute(unquote(__CALLER__.module), :rules, %{})

      @before_compile unquote(__MODULE__)

      import unquote(__MODULE__)
    end
  end

  defmacro __before_compile__(_env) do
    all_rules = Module.get_attribute(__CALLER__.module, :rules)

    # Check for missing rule declarations
    #
    case Grammar.check_all_rules_exist(all_rules) do
      [] -> :ok
      errors ->
        errors = Enum.map_join(errors, "\n", fn {name, step} -> "| #{name} misses #{step}" end)
        raise "\n* Production chain errors\n#{errors}"
    end

    # Build firsts map
    #
    rules_with_firsts = first_of_rules(all_rules)

    # Check for ambiguous rules
    case Grammar.check_rules_are_not_ambiguous(rules_with_firsts) do
      [] -> :ok
      errors ->
        errors = Enum.map_join(errors, "\n", fn {name, first, clause_ids} ->
          "| Rule #{name} has conflicts for first #{inspect(first)} on clauses #{inspect(clause_ids)}"
        end)
        raise "\n* Ambiguous rules\n#{errors}"
    end

    IO.inspect(rules_with_firsts, label: "FIRSTS")

    productions =
      for {name, %Rule{name: name, clauses: clauses}} <- rules_with_firsts do
        quote do
          def unquote(name)(%Tokenizer{} = tokenizer) do
            IO.inspect(unquote(name), label: "RULE")
            IO.inspect(tokenizer, label: "TOKENIZER")
          end
        end
      end

    start_rule_name = Module.get_attribute(__CALLER__.module, :start_rule_name)
    start = Map.get(rules_with_firsts, start_rule_name)

    quote do
      def rules do
        unquote(Macro.escape(rules_with_firsts))
      end

      def parse(input) do
        tokenizer = Tokenizer.new(input)
        unquote(start.name)(tokenizer)
      end

      unquote_splicing(productions)
    end
  end
end

defmodule MyGrammar do
  use Grammar

  rule start(:expression) do

  end

  rule expression(:term, :expression_cont) do

  end


  rule expression_cont("+", :term, :expression_cont) do

  end

  rule expression_cont("-", :term, :expression_cont) do

  end

  rule term(:factor, :term_cont) do

  end

  rule term_cont("*", :factor, :term_cont) do

  end

  rule term_cont("/", :factor, :term_cont) do

  end

  rule factor(:number) do

  end

  rule factor("(", :expression, ")") do

  end

  rule number(~r/[0-9]+/) do

  end
end
