defmodule Grammar do
  @moduledoc """
  This module provides a DSL to define parser of structured inputs. Parsers are defined as a grammar.

  Grammar defined must be LL(1) grammars, i.e. they must be unambiguous and have a single token lookahead.

  The grammar is defined by a set of rules, each rule being a set of clauses. Clauses must be understood as
  disjoinded paths in the rule resolution, or as the `or` operator in the classic notation.

  The tokenization process relies on the [TokenExtractor](`Grammar.TokenExtractor`) protocol, which is used to extract tokens from the input string.
  This protocol is implemented for BitString and Regex, and can be extended to custom token types.

  To declare a parser module, just `use` the Grammar module in your module, and define your rules using the `rule/2` and `rule?/2` macro.
  The newly defined module will expose a `parse/1` function that will parse the input string, and return a tuple with the
  tokenizer in its final state, and result.

  See `rule/2` for a full example.

  ## Spaces and line breaks handling

  By default, the tokenizer will drop spaces and line breaks.
  If you want to keep them, you can pass the `drop_spaces: false` option to the `use Grammar` macro. In this case, you
  are fully responsible for handling spaces and line breaks in your rules.

  ## Options

  - `drop_spaces: true` (default): if set to `false`, the tokenizer will not drop spaces and line breaks.

  #### Example

      defmodule MyModule do
        use Grammar, drop_spaces: false

        # You're now responsible for handling spaces and line breaks in your rules
      end
  """

  defmodule TokenExtractorHelper do
    @moduledoc """
    This module provides helper functions to work with TokenExtractor implementations using Regex.
    """
    @spec normalize_regex(Regex.t()) :: Regex.t()
    def normalize_regex(regex) do
      source = Regex.source(regex)
      opt = Regex.opts(regex)

      case source do
        "^" <> _rest -> regex
        source -> Regex.compile!("^#{source}", opt)
      end
    end

    @spec try_read_from_regex(Regex.t(), String.t()) :: nil | {String.t(), integer()}
    def try_read_from_regex(pattern, input_string) do
      case Regex.run(pattern, input_string) do
        nil -> nil
        [match] -> {match, byte_size(match)}
      end
    end
  end

  defprotocol TokenExtractor do
    @moduledoc """
    This protocol exposes the functions needed to extract tokens from the input string.
    """

    @doc """
    Try to read a token from the input string.

    If the token is found, returns the token and its length in the input string.
    Returns nil otherwise.

    ## Example

        iex> Grammar.TokenExtractor.try_read("hello", "hello world")
        {"hello", 5}

        iex> Grammar.TokenExtractor.try_read("Hello", "hello world")
        nil

        iex> Grammar.TokenExtractor.try_read(~r/[0-9]+/, "013456toto")
        {"013456", 6}

        iex> Grammar.TokenExtractor.try_read(~r/[a-z]+/, "013456toto")
        nil
    """
    @spec try_read(t, String.t()) :: nil | {String.t(), integer()}
    def try_read(token_prototype, input_string)

    @doc """
    Returns true if the token matches the token prototype.

    This function is used orient the parser in the right clause of a rule, by comparing the current token
    to the clause list of first tokens.

    ## Example

        iex> Grammar.TokenExtractor.match?("hello", "hello")
        true

        iex> Grammar.TokenExtractor.match?("hello", "Horld")
        false

        iex> Grammar.TokenExtractor.match?(~r/[0-9]+/, "013456")
        true

        iex> Grammar.TokenExtractor.match?(~r/[0-9]+/, "a013456")
        false
    """
    @spec match?(t, term()) :: boolean()
    def match?(prototype, token)
  end

  defimpl TokenExtractor, for: BitString do
    def try_read(token_prototype, input_string) do
      case input_string do
        <<^token_prototype::binary, _rest::binary>> -> {token_prototype, String.length(token_prototype)}
        _ -> nil
      end
    end

    def match?(token, token) when is_binary(token), do: true
    def match?(_token, _string), do: false
  end

  defimpl TokenExtractor, for: Regex do
    def try_read(token_prototype, input_string) do
      token_prototype
      |> TokenExtractorHelper.normalize_regex()
      |> TokenExtractorHelper.try_read_from_regex(input_string)
    end

    def match?(regex, token) when is_binary(token) do
      [token] == Regex.run(regex, token)
    end

    def match?(_regex, _token), do: false
  end

  defmodule Clause do
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

  defmodule Rule do
    @moduledoc """
    A Rule entity represents a grammar rule, by its name and the list of clauses.
    """
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

  def build_production_for_rule({name, %Rule{name: name, clauses: clauses} = rule}) do
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

  def build_production_for_clause(%Clause{firsts: firsts, blk: blk} = clause, nested_ast) do
    quote do
      if Enum.any?(unquote(firsts), &Grammar.TokenExtractor.match?(&1, current_token)) do
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
      unquote(rule_name)(tokenizer)
    end
  end

  def build_production_code_for_clause_elem(matcher) do
    quote do
      case Tokenizer.next_token(tokenizer, unquote(matcher)) do
        {{nil, cursor}, tokenizer} ->
          throw({cursor, :no_token})

        {{token, cursor}, tokenizer} ->
          {tokenizer, token}
      end
    end
  end

  def no_clause do
    quote do
      throw({cursor, :no_clause_matched})
    end
  end

  def epsilon_clause do
    quote do
      {tokenizer, nil}
    end
  end

  def what_are_you_doing_here_clause do
    quote do
      throw({cursor, :what_are_you_doing_here})
    end
  end

  def build_token_list(rules) when is_map(rules) do
    Enum.flat_map(rules, &build_token_list_for_rule(&1))
  end

  def build_token_list_for_rule({name, %Rule{name: name, clauses: clauses}}) do
    Enum.flat_map(clauses, &build_token_list_for_clause(&1))
  end

  def build_token_list_for_clause(%Clause{def: def}) do
    def |> Enum.reject(&is_atom/1)
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

  @doc """
  Use this macro to define rules of your grammar.

  **The first rule defined will be the entry rule of the grammar**.

  Calls to this macro sharing the same name will be grouped together as they define the same rule,
  each call is a possible path in the rule resolution.

  Lets name a single call to `rule` a clause.
  All clauses must be disjointed, i.e. they must not share the same first token.
  They can be understood as the `or` operator in a rule.

  Each rule of rule clause is defined by
  - a name, which is an atom
  - a definition, which is a list of atoms or token prototypes
  - a block, which is the code to execute when the clause is fully matched

  When executed the code block is provided with a `params` binding, which is a list of the results of the clause steps.

  In the case where a `rule` cannot be matched, a `RuntimeError` is raised (see `rule?/2` for a relaxed version).

  ## Example

      defmodule NumberOfNameListParser do
        use Grammar

        rule start("[", :list_or_empty_list) do
          [_, list] = params
          list || []
        end

        rule? list_or_empty_list(:item, :list_tail, "]") do
          [item, list_tail, _] = params
          [item | (list_tail || [])]
        end

        rule? list_tail(",", :item, :list_tail) do
          [_, item, list_tail] = params
          [item | (list_tail || [])]
        end

        rule item(~r/[0-9]+/) do
          [number] = params
          String.to_integer(number)
        end

        rule item(~r/[a-zA-Z]+/) do
          [string] = params
          string
        end
      end

      NumberOfNameListParser.parse("[1, toto, 23]")
      {%Grammar.Tokenizer{
        input: "",
        current_line: 1,
        current_column: 14
      }, [1, "toto", 23]}
  """
  defmacro rule({name, meta, def}, do: blk) when is_atom(name), do: store_clause(__CALLER__.module, name, meta, def, blk, false)

  @doc """
  Same as `rule/2` but relaxed : if the rule cannot be matched, it will be valued as `nil`.

  Useful for optional or recursive rules.

  See example in `rule/2`.
  """

  defmacro rule?({name, meta, def}, do: blk) when is_atom(name), do: store_clause(__CALLER__.module, name, meta, def, blk, true)

  @allowed_opts [drop_spaces: true]

  defmacro __using__(opts) do
    opts = Keyword.validate!(opts, @allowed_opts)
    drop_spaces? = Keyword.get(opts, :drop_spaces, true)

    quote do
      @compile {:inline, []}

      @drop_spaces? unquote(drop_spaces?)
      @rules %{}

      @before_compile unquote(__MODULE__)

      import unquote(__MODULE__)
    end
  end

  defmacro __before_compile__(_env) do
    all_rules = Module.get_attribute(__CALLER__.module, :rules)
    drop_spaces? = Module.get_attribute(__CALLER__.module, :drop_spaces?)

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
      alias Grammar.Tokenizer

      def rules do
        unquote(Macro.escape(rules_with_firsts))
      end

      @spec parse(binary()) ::
              {:ok, term()} | {:error, {integer(), integer()}, atom()} | {:error, {integer(), integer()}, atom(), term()}
      def parse(input) do
        try do
          tokenizer = Tokenizer.new(input, unquote(drop_spaces?))
          {tokenizer_final, value} = unquote(start.name)(tokenizer)
          {:ok, value}
        catch
          {where, what} = error ->
            {:error, where, what}

          {where, what, data} = error ->
            {:error, where, what, data}
        end
      end

      unquote_splicing(productions)
    end
  end
end
