defmodule Tokenizer do
  defstruct tokens: []

  def new(input) when is_binary(input) do
    struct(__MODULE__, tokens: String.split(input, ~r/\s+/, trim: true))
  end

  def current_token(%__MODULE__{tokens: [token | _tail]} = tokenizer) do
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
  defprotocol TokenMatcher do
    @spec match?(t, String.t()) :: boolean()
    def match?(token, string)
  end

  defimpl TokenMatcher, for: BitString do
    def match?(token, string), do: token === string
  end

  defimpl TokenMatcher, for: Regex do
    def match?(regex, token), do: Regex.match?(regex, token)
  end

  defimpl TokenMatcher, for: List do
    def match?(list, token), do: Enum.any?(list, &Grammar.TokenMatcher.match?(&1, token))
  end

  defmodule Clause do
    @enforce_keys [:def, :blk, :epsilon]
    defstruct def: nil, firsts: nil, blk: nil, epsilon: false

    def new(def, blk, epsilon) when is_list(def) and is_atom(epsilon) do
      struct(__MODULE__, def: def, blk: blk, epsilon: epsilon)
    end

    def is_epsilon(%__MODULE__{epsilon: epsilon}), do: epsilon

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

  def firsts_of_rule({name, %Rule{name: name}}, acc, _rules) when is_map_key(acc, name) do
    acc
  end

  def firsts_of_rule({name, %Rule{name: name, clauses: clauses}}, acc, rules) do
    acc = Map.put(acc, name, Rule.new(name))
    Enum.reduce(clauses, acc, &firsts_of_clause(name, &1, &2, rules))
  end

  def firsts_of_clause(rule_name, %Clause{def: [sub_rule_name | _], firsts: nil} = clause, acc, _rules)
      when is_atom(sub_rule_name) and is_map_key(acc, sub_rule_name) do
    # sub_rule already processed

    sub_rule_firsts = acc |> Map.get(sub_rule_name) |> Rule.get_firsts()
    updated_clause = Clause.set_firsts(clause, sub_rule_firsts)
    Map.update!(acc, rule_name, &Rule.add_clause(&1, updated_clause))
  end

  def firsts_of_clause(rule_name, %Clause{def: [sub_rule_name | _], firsts: nil} = clause, acc, rules) when is_atom(sub_rule_name) do
    # sub_rule *not* already processed

    sub_rule = Map.get(rules, sub_rule_name)
    acc = firsts_of_rule({sub_rule_name, sub_rule}, acc, rules)
    firsts_of_clause(rule_name, clause, acc, rules)
  end

  def firsts_of_clause(rule_name, %Clause{def: [first | _]} = clause, acc, _rules) do
    updated_clause = Clause.set_firsts(clause, [first])
    rule = Map.get(acc, rule_name) || Rule.new(rule_name)
    rule = Rule.add_clause(rule, updated_clause)
    Map.put(acc, rule_name, rule)
  end

  def build_production_for_rules(rules) when is_map(rules) do
    Enum.map(rules, &build_production_for_rule(&1))
  end

  def build_production_for_rule({name, %Rule{name: name, clauses: clauses}}) do
    is_epsilon = Enum.any?(clauses, &Clause.is_epsilon/1)
    no_clause = (is_epsilon && epsilon_clause()) || no_clause()
    nested_clauses = Enum.reduce(clauses, no_clause, &build_production_for_clause(&1, &2))

    quote do
      def unquote(name)(%Tokenizer{} = tokenizer) do
        # IO.puts("Enter #{unquote(name)}")
        # IO.puts("| Tokenizer #{inspect(tokenizer)}")
        unquote(nested_clauses)
      end
    end
  end

  def build_production_for_clause(%Clause{firsts: firsts, blk: blk, epsilon: epsilon} = clause, nested_ast) do
    quote do
      case Tokenizer.current_token(tokenizer) do
        {nil, _tokenizer} ->
          if unquote(epsilon) do
            {tokenizer, nil}
          else
            raise "No more token"
          end

        {token, tokenizer} ->
          if TokenMatcher.match?(unquote(firsts), token) do
            fun_in = []

            unquote_splicing(build_production_code_for_clause(clause))

            fun_out =
              (fn var!(params) ->
                 unquote(blk)
               end).(fun_in)

            {tokenizer, fun_out}
          else
            unquote(nested_ast)
          end
      end
    end
  end

  def build_production_code_for_clause(%Clause{def: def}) when is_list(def) do
    Enum.map(def, fn elem ->
      ast = build_production_code_for_clause_elem(elem)

      quote do
        {tokenizer, res} = unquote(ast)
        fun_in = fun_in ++ [res]
      end
    end)
  end

  def build_production_code_for_clause_elem(rule_name) when is_atom(rule_name) do
    quote do
      # IO.puts("Call #{unquote(rule_name)}")
      unquote(rule_name)(tokenizer)
    end
  end

  def build_production_code_for_clause_elem(matcher) do
    quote do
      case Tokenizer.next_token(tokenizer) do
        {nil, _tokenizer} ->
          raise "No more token"

        {token, tokenizer} ->
          if TokenMatcher.match?(unquote(matcher), token) do
            # IO.puts("Consume #{token}")
            {tokenizer, token}
          else
            raise "Unexpected token #{token}"
          end
      end
    end
  end

  def no_clause() do
    quote do
      raise "No clause matched"
    end
  end

  def epsilon_clause() do
    quote do
      {tokenizer, nil}
    end
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

  defmacro rule({name, meta, def}, do: blk) when is_atom(name), do: store_clause(__CALLER__.module, name, meta, def, blk, false)

  defmacro rule!({name, meta, def}, do: blk) when is_atom(name), do: store_clause(__CALLER__.module, name, meta, def, blk, true)

  defmacro __using__(_opts) do
    quote do
      @compile {:inline, []}

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
      [] ->
        :ok

      errors ->
        errors = Enum.map_join(errors, "\n", fn {name, step} -> "| #{name} misses #{step}" end)
        raise "\n* Production chain errors\n#{errors}"
    end

    # Build firsts map
    #
    rules_with_firsts = first_of_rules(all_rules)

    # Check for ambiguous rules
    #
    case Grammar.check_rules_are_not_ambiguous(rules_with_firsts) do
      [] ->
        :ok

      errors ->
        errors =
          Enum.map_join(errors, "\n", fn {name, first, clause_ids} ->
            "| Rule #{name} has conflicts for first #{inspect(first)} on clauses #{inspect(clause_ids)}"
          end)

        raise "\n* Ambiguous rules\n#{errors}"
    end

    # Generate parser functions
    #
    productions = build_production_for_rules(rules_with_firsts)

    start_rule_name = Module.get_attribute(__CALLER__.module, :start_rule_name)
    start = Map.get(rules_with_firsts, start_rule_name)

    # Generate parse/1 function
    #
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
    |> tap(fn ast ->
      ast
      |> Macro.to_string()
      |> IO.puts()
    end)
  end
end

defmodule MyGrammar do
  use Grammar

  rule start(:program) do
    [program] = params
    "#{program}"
  end

  rule program(:assigment, :program_cont) do
    [assigment, cont] = params
    "#{assigment}\n#{cont}"
  end

  rule! program_cont(:assigment, :program_cont) do
    [assigment, cont] = params
    "#{assigment}\n#{cont}"
  end

  rule! assigment(:identifier, "=", :rhv) do
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

  rule! expression_cont("+", :term, :expression_cont) do
    [_, term, cont] = params
    "plus #{term} #{cont}"
  end

  rule! expression_cont("-", :term, :expression_cont) do
    [_, term, cont] = params
    "moins #{term} #{cont}"
  end

  rule term(:factor, :term_cont) do
    [factor, cont] = params
    "#{factor} #{cont}"
  end

  rule! term_cont("*", :factor, :term_cont) do
    [_, factor, cont] = params
    "multiplié par #{factor} #{cont}"
  end

  rule! term_cont("/", :factor, :term_cont) do
    [_, factor, cont] = params
    "divisé par #{factor} #{cont}"
  end

  rule factor(:number) do
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

  rule string(~r/"[\S]*"/) do
    [string] = params
    string
  end
end
