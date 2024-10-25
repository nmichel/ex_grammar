defmodule Grammar do
  defmodule TokenExtractorHelper do
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
    @spec try_read(t, String.t()) :: nil | {String.t(), integer()}
    def try_read(token_prototype, input_string)

    @spec match?(t, String.t()) :: boolean()
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
        unquote(nested_clauses)
      end
    end
  end

  def build_production_for_clause(%Clause{firsts: firsts, blk: blk, epsilon: epsilon} = clause, nested_ast) do
    quote do
      case Tokenizer.current_token(tokenizer) do
        {{nil, cursor}, tokenizer} ->
          if unquote(epsilon) do
            {tokenizer, nil}
          else
            raise "[#{inspect(cursor)}] : cannot find token"
          end

        {{token, _cursor}, tokenizer} ->
          if Enum.any?(unquote(firsts), &Grammar.TokenExtractor.match?(&1, token)) do
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
      unquote(rule_name)(tokenizer)
    end
  end

  def build_production_code_for_clause_elem(matcher) do
    quote do
      case Tokenizer.next_token(tokenizer) do
        {{nil, cursor}, tokenizer} ->
          raise "[#{inspect(cursor)}] : cannot find token"

        {{token, cursor}, tokenizer} ->
          if TokenExtractor.match?(unquote(matcher), token) do
            {tokenizer, token}
          else
            raise "[#{inspect(cursor)}] : unexpected token #{token}"
          end
      end
    end
  end

  def no_clause() do
    quote do
      raise "[#{tokenizer.current_line} | #{tokenizer.current_column}] No clause matched"
    end
  end

  def epsilon_clause() do
    quote do
      {tokenizer, nil}
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

    # Generate the list of possible tokens from the clause firsts lists
    #
    token_templates = build_token_list(rules_with_firsts)

    tokenizer =
      quote do
        defmodule Tokenizer do
          @enforce_keys [:input]
          defstruct input: "", current_line: 1, current_column: 1

          @newlines ~c[\n]
          @whitespaces ~c[ \t\v\f\r]
          @extractors [unquote_splicing(token_templates)]

          def new(input) when is_binary(input) do
            struct(__MODULE__, input: input)
            |> drop_spaces()
          end

          def current_token(%__MODULE__{input: input} = tokenizer) do
            {token, _} = try_read_token(tokenizer)
            {token, tokenizer}
          end

          def next_token(%__MODULE__{input: input} = tokenizer) do
            case try_read_token(tokenizer) do
              {token, 0} ->
                {token, tokenizer}

              {token, token_length} ->
                tokenizer =
                  tokenizer
                  |> consume_token(token_length)
                  |> drop_spaces()

                {token, tokenizer}
            end
          end

          def try_read_token(%__MODULE__{} = tokenizer) do
            input = tokenizer.input
            cursor = {tokenizer.current_line, tokenizer.current_column}

            @extractors
            |> Enum.reduce({nil, 0}, fn token_template, {current_token, current_length} ->
              case TokenExtractor.try_read(token_template, input) do
                nil -> {current_token, current_length}
                {token, length} when length > current_length -> {token, length}
                _found_tuple -> {current_token, current_length}
              end
            end)
            |> then(fn {token, length} -> {{token, cursor}, length} end)
          end

          def consume_token(%__MODULE__{} = tokenizer, token_length) do
            input = tokenizer.input
            {token_string, rest} = String.split_at(input, token_length)
            tokenizer = update_cursor(tokenizer, token_string)
            %{tokenizer | input: rest}
          end

          def update_cursor(%__MODULE__{} = tokenizer, <<"">>), do: tokenizer

          def update_cursor(%__MODULE__{} = tokenizer, <<c::utf8, tail::binary>>) when c in @newlines do
            update_cursor(%{tokenizer | current_line: tokenizer.current_line + 1, current_column: 0}, tail)
          end

          def update_cursor(%__MODULE__{} = tokenizer, <<c::utf8, tail::binary>>) do
            update_cursor(%{tokenizer | current_line: tokenizer.current_line, current_column: tokenizer.current_column + 1}, tail)
          end

          def drop_spaces(%__MODULE__{input: <<c::utf8, tail::binary>>} = tokenizer) when c in @newlines do
            drop_spaces(%{tokenizer | current_line: tokenizer.current_line + 1, current_column: 1, input: tail})
          end

          def drop_spaces(%__MODULE__{input: <<c::utf8, tail::binary>>} = tokenizer) when c in @whitespaces do
            drop_spaces(%{tokenizer | input: tail, current_column: tokenizer.current_column + 1})
          end

          def drop_spaces(%__MODULE__{} = tokenizer) do
            tokenizer
          end
        end

        defimpl Enumerable, for: __MODULE__.Tokenizer do
          def count(_tokenizer), do: {:error, Tokenizer}
          def member?(_tokenizer, _element), do: {:error, Tokenizer}
          def slice(_tokenizer), do: {:error, Tokenizer}
          def reduce(tokenizer, {:halt, acc}, _fun), do: {:halted, acc}

          def reduce(tokenizer, {:cont, acc}, fun) do
            case Tokenizer.next_token(tokenizer) do
              {{nil, cursor}, _} ->
                {:done, acc}

              {token, tokenizer} ->
                reduce(tokenizer, fun.(token, acc), fun)
            end
          end
        end
      end

    # Generate parser functions
    #
    productions = build_production_for_rules(rules_with_firsts)

    start_rule_name = Module.get_attribute(__CALLER__.module, :start_rule_name)
    start = Map.get(rules_with_firsts, start_rule_name)

    # Generate parse/1 function
    #
    quote do
      unquote(tokenizer)

      def rules do
        unquote(Macro.escape(rules_with_firsts))
      end

      def parse(input) do
        tokenizer = __MODULE__.Tokenizer.new(input)
        unquote(start.name)(tokenizer)
      end

      unquote_splicing(productions)
    end
  end
end
