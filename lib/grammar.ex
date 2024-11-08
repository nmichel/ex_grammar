defmodule Grammar do
  @moduledoc """
  This module provides a DSL to define parser of structured inputs. Parsers are defined as a grammar.

  Grammar defined must be LL(1) grammars, i.e. they must be unambiguous and have a single token lookahead.

  The grammar is defined by a set of rules, each rule being a set of clauses. Clauses must be understood as
  disjoinded paths in the rule resolution, or as the `or` operator in the classic notation.

  The tokenization process relies on the [TokenExtractor](`Grammar.Tokenizer.TokenExtractor`) protocol, which is used to extract tokens from the input string.
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

  In the following `MyModuleKO` module, the `start` rule doesn't handle spaces and line breaks,
  so it will fail if the input contains them.

      iex> defmodule MyModuleKO do
      ...>   use Grammar, drop_spaces: false
      ...>
      ...>   # spaces and linebreaks not handled
      ...>
      ...>   rule start("hello", "world") do
      ...>     [_hello, _world] = params
      ...>     "hello world"
      ...>   end
      ...> end
      iex> MyModuleKO.parse("helloworld")
      {:ok, "hello world"}
      iex> MyModuleKO.parse("hello world")
      {:error, {1, 6}, :no_token}

  But in the `MyModuleOK` module, the `start` explicitly handles spaces and line breaks between "hello" and "world".

  And even more that rule definition requires at least one space between "hello" and "world", parsing fails if no space is found.

      iex> defmodule MyModuleOK do
      ...>   use Grammar, drop_spaces: false
      ...>
      ...>   # spaces and linebreaks not handled
      ...>
      ...>   rule start("hello", ~r/[\\s]+/, "world") do
      ...>     [_hello, _spaces, _world] = params
      ...>     "hello world"
      ...>   end
      ...> end
      iex> MyModuleOK.parse("helloworld")
      {:error, {1, 6}, :no_token}
      iex> MyModuleOK.parse(~s/hello  \\t world/)
      {:ok, "hello world"}
  """

  alias Grammar.CodeGen

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

      iex> defmodule NumberOfNameListParser do
      ...>   use Grammar
      ...>
      ...>   rule start("[", :list_or_empty_list) do
      ...>     [_, list] = params
      ...>     list || []
      ...>   end
      ...>
      ...>   rule? list_or_empty_list(:item, :list_tail, "]") do
      ...>     [item, list_tail, _] = params
      ...>     [item | (list_tail || [])]
      ...>   end
      ...>
      ...>   rule? list_tail(",", :item, :list_tail) do
      ...>     [_, item, list_tail] = params
      ...>     [item | (list_tail || [])]
      ...>   end
      ...>
      ...>   rule item(~r/[0-9]+/) do
      ...>     [number] = params
      ...>     String.to_integer(number)
      ...>   end
      ...>
      ...>   rule item(~r/[a-zA-Z]+/) do
      ...>     [string] = params
      ...>     string
      ...>   end
      ...> end
      iex> GrammarTest.NumberOfNameListParser.parse("[1, toto, 23]")
      {:ok, [1, "toto", 23]}
  """
  defmacro rule({name, meta, def}, do: blk) when is_atom(name),
    do: CodeGen.store_clause(__CALLER__.module, name, meta, def, blk, false)

  @doc """
  Same as `rule/2` but relaxed : if the rule cannot be matched, it will be valued as `nil`.

  Useful for optional or recursive rules.

  See example in `rule/2`.
  """

  defmacro rule?({name, meta, def}, do: blk) when is_atom(name),
    do: CodeGen.store_clause(__CALLER__.module, name, meta, def, blk, true)

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
    case CodeGen.check_all_rules_exist(all_rules) do
      [] ->
        :ok

      errors ->
        errors = Enum.map_join(errors, "\n", fn {name, step} -> "| #{name} misses #{step}" end)
        raise "\n* Production chain errors\n#{errors}"
    end

    # Build firsts map
    #
    rules_with_firsts = CodeGen.first_of_rules(all_rules)

    # Check for ambiguous rules
    #
    case CodeGen.check_rules_are_not_ambiguous(rules_with_firsts) do
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
    productions = CodeGen.build_production_for_rules(rules_with_firsts)

    start_rule_name = Module.get_attribute(__CALLER__.module, :start_rule_name)
    start = Map.get(rules_with_firsts, start_rule_name)

    # Generate parse/1 function
    #
    quote do
      alias Grammar.Tokenizer
      alias Grammar.Tokenizer.TokenExtractor

      def rules do
        unquote(Macro.escape(rules_with_firsts))
      end

      @spec parse(binary()) ::
              {:ok, term()} | {:error, {integer(), integer()}, atom()} | {:error, {integer(), integer()}, atom(), term()}
      def parse(input) do
        # credo:disable-for-next-line Credo.Check.Readability.PreferImplicitTry
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
