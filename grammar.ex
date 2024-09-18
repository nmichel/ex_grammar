defmodule Tokenizer do
  defstruct tokens: []

  def new(input) when is_binary(input) do
    struct(__MODULE__, tokens: String.split(input, ~r/\s+/, trim: true))
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

    def set_first(%__MODULE__{firsts: nil} = clause, firsts) when is_list(firsts) do
      %{clause | firsts: firsts}
    end
  end

  defmodule Rule do
    @enforce_keys [:name, :clauses]
    defstruct [:name, :clauses]

    def new(name, def) when is_atom(name) and is_list(def)do
      struct(__MODULE__, name: name, clauses: [Clause.new(def)])
    end

    def add_clause(%__MODULE__{clauses: clauses} = rule, def) when is_list(def) do
      %{rule | clauses: clauses ++ [Clause.new(def)]}
    end
  end

  def check_production_chain(rules) when is_map(rules) do
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

  def first_of_rules(rules) when is_map(rules) do
    Enum.reduce(rules, %{}, &firsts_of_rule(&1, &2, rules))
  end

  def firsts_of_rule({name, %Rule{name: name}}, acc, rules) when is_map_key(acc, name) do
    acc
  end

  def firsts_of_rule({name, %Rule{name: name, clauses: clauses}}, acc, rules) do
    Enum.reduce(clauses, acc, &firsts_of_clause(name, &1, &2, rules))
  end

  def firsts_of_clause(rule_name, %Clause{def: [sub_rule_name | _]}, acc, _rules) when is_atom(sub_rule_name) and is_map_key(acc, sub_rule_name) do
    # sub_rule already processed

    sub_rule_firsts = Map.get(acc, sub_rule_name)
    current_rule_firsts = Map.get(acc, rule_name)
    if current_rule_firsts && Enum.any?(sub_rule_firsts, & &1 in current_rule_firsts) do
      raise "Firsts from #{sub_rule_name} already found for rule #{rule_name}"
    end

    Map.update(acc, rule_name, sub_rule_firsts, & &1 ++ sub_rule_firsts)
  end

  def firsts_of_clause(rule_name, %Clause{def: [sub_rule_name | _]} = clause, acc, rules) when is_atom(sub_rule_name) do
    # sub_rule *not* already processed

    sub_rule = Map.get(rules, sub_rule_name)
    acc = firsts_of_rule({sub_rule_name, sub_rule}, acc, rules)
    firsts_of_clause(rule_name, clause, acc, rules)
  end

  def firsts_of_clause(name, %Clause{def: [first | _]}, acc, _rules), do: Map.update(acc, name, [first], & &1 ++ [first])

  # def build_matcher_for(c) when is_binary(c), do: & &1 === c
  # def build_matcher_for(%Regex{} = r), do: & Regex.match?(r, &1)

  defmacro rule({name, _meta, def}, do: _blk) when is_atom(name) do
    quote do
      rules = Map.update(@rules, unquote(name), Rule.new(unquote(name), unquote(def)), & Rule.add_clause(&1, unquote(def)))
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

    # Check for missing rules definition
    #
    case Grammar.check_production_chain(all_rules) do
      [] -> :ok
      errors ->
        errors = Enum.map_join(errors, "\n", fn {name, step} -> "| #{name} misses #{step}" end)
        raise "\n* Production chain errors\n#{errors}"
    end

    # Build firsts map
    #
    firsts_for_rules = first_of_rules(all_rules)

    # productions =
    #   for {name, defs} <- Enum.group_by(all_rules, & &1.name, & &1.def) do
    #     quote do
    #       def unquote(name)(%Tokenizer{} = tokenizer) do
    #         IO.inspect(unquote(name), label: "RULE")
    #         IO.inspect(tokenizer, label: "TOKENIZER")
    #       end
    #     end
    #   end

    # [start | _] = all_rules

    quote do
      def parse(input) do
        tokenizer = Tokenizer.new(input)
        # unquote(start.name)(tokenizer)
      end
    end

    #   unquote_splicing(productions)
    # end
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
