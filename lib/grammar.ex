defmodule Grammar do
  defstruct stack: [], rules: %{}, firsts: %{}, heap: []

  @moduledoc ~S"""
  This module exposes functions and macros to create parsers of structured inputs. Parsers are defined as LL(1) grammars.

  A grammar must be LL(1), i.e. must be unambiguous and have a single token lookahead.

  One can create parsers at runtime, using the `Grammar` functions, or define parsers at compile time using the `Grammar` macros.

  The grammar is defined by a set of rules, each rule being a set of clauses. Clauses must be understood as
  disjoinded paths in the rule resolution, or as the `or` operator in the classic notation.

  The tokenization process relies on the [TokenExtractor](`Grammar.Tokenizer.TokenExtractor`) protocol, which is used to extract tokens from the input string.
  This protocol is implemented for BitString and Regex, and can be extended to custom token types.

  ### Spaces and line breaks handling

  By default, the tokenizer will drop spaces and line breaks.

  If you want to keep them, you can pass the `drop_spaces: false` option to the `use Grammar` macro.

  When using the Tokenizer directly, you must pass `false` as second parameter to `Tokenizer.new/2` to keep spaces and line breaks.

  In this case, you are fully responsible for handling spaces and line breaks in your rules.

  # Using the API

  Creating a new grammar is done by calling `Grammar.new/0`, then adding clauses to it using `Grammar.add_clause/5`.

  Once all the rules are defined, the grammar must be prepared using `Grammar.prepare/1`.

  The grammar is then ready to be used for parsing, by calling `Grammar.start/2` with the starting rule name,
  and then `Grammar.loop/2` with the input string wrapped in a `Tokenizer`.

  #### Example

      iex> g = Grammar.new()
      ...> |> Grammar.add_clause(:start, ["hello", :what], fn ["hello", what] -> "hello #{what} !" end)
      ...> |> Grammar.add_clause(:what, [~r/[a-zA-Z]+/], fn [ident] -> ident end)
      ...> |> Grammar.prepare!()
      ...> |> Grammar.start(:start)
      iex> Grammar.loop(g, Grammar.Tokenizer.new("hello world"))
      {:ok, "hello world !"}

  # Using the DSL

  To declare a parser module, just `use` the Grammar module in your module, and define your rules using the `rule/2` and `rule?/2` macro.
  The newly defined module will expose a `parse/1` function that will parse the input string, and return a tuple with the
  tokenizer in its final state, and result.

  See `rule/2` for a full example.

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
      ...>   rule start("hello", ~r/[\s]+/, "world") do
      ...>     [_hello, _spaces, _world] = params
      ...>     "hello world"
      ...>   end
      ...> end
      iex> MyModuleOK.parse("helloworld")
      {:error, {1, 6}, :no_token}
      iex> MyModuleOK.parse(~s/hello  \t world/)
      {:ok, "hello world"}
  """

  alias Grammar.Clause
  alias Grammar.CodeGen
  alias Grammar.Rule
  alias Grammar.RulesChecker
  alias Grammar.Tokenizer
  alias Grammar.Tokenizer.TokenExtractor

  @type first :: term()
  @type rules :: %{Rule.name() => Rule.t()}

  @type t :: %__MODULE__{
          stack: list(),
          rules: rules(),
          firsts: %{Rule.name() => [first()]},
          heap: list()
        }

  @type error_type :: :no_clause_matched | :no_token
  @type error :: {error_type(), term()}

  @doc """
  Create a new empty `Grammar`.
  """
  @spec new() :: t()
  def new, do: %__MODULE__{}

  @doc """
  Add a clause to the grammar.

  A clause is defined by a name, a substitution, a function to execute when the clause is matched, and an epsilon flag.

  Clauses sharing the same name are considered as clauses of a single rule.

  The substitution is a list of terms which declares the steps to match the clause, from left to right, first to last.

  Each term can be either
  - a rule name, which is an atom,
  - a value for which there is a [TokenExtractor](`Grammar.Tokenizer.TokenExtractor`) protocol implementation.

  [TokenExtractor](`Grammar.Tokenizer.TokenExtractor`) implementations are provided for BitString and Regex, and can be extended to custom token types.

  The function is a callback to execute when the clause is fully matched. It is given a list as parameter, each element
  of that list is the value produced by the substitution of each term.

  The epsilon flag indicates if the rule is mandatory or not.

  > ⚠️ the epsilon flag is used only when the *first clause of a rule* is added, even if it is defaulted to `false`.

  ## Example

  The *second* clause of `:start` is marked as epsilon, but it is ignored.

      iex> g = Grammar.new()
      ...> |> Grammar.add_clause(:start, ["hello"], fn [value] -> value end)
      ...> |> Grammar.add_clause(:start, ["world"], fn [value] -> value end, true) # `true` is ignored !
      ...> |> Grammar.prepare!()
      ...> |> Grammar.start(:start)
      ...>
      iex> Grammar.loop(g, Grammar.Tokenizer.new("hello"))
      {:ok, "hello"}
      ...>
      iex> Grammar.loop(g, Grammar.Tokenizer.new("world"))
      {:ok, "world"}
      ...>
      iex> Grammar.loop(g, Grammar.Tokenizer.new("pouet"))
      {:error, {1, 1}, :no_clause_matched}


  The *first* clause of `:start` is marked as epsilon !

      iex> g = Grammar.new()
      ...> |> Grammar.add_clause(:start, ["hello"], fn [value] -> value end, true) # `true` is used !
      ...> |> Grammar.add_clause(:start, ["world"], fn [value] -> value end, false) # `false` is ignored !
      ...> |> Grammar.prepare!()
      ...> |> Grammar.start(:start)
      ...>
      iex> Grammar.loop(g, Grammar.Tokenizer.new("hello"))
      {:ok, "hello"}
      ...>
      iex> Grammar.loop(g, Grammar.Tokenizer.new("world"))
      {:ok, "world"}
      ...>
      iex> Grammar.loop(g, Grammar.Tokenizer.new("pouet"))
      {:ok, nil}


  """
  @spec add_clause(t(), Rule.name(), Clause.substitution(), Clause.callback(), boolean()) :: t()
  def add_clause(%__MODULE__{} = grammar, name, substitution, function, epsilon? \\ false) when substitution != [] do
    rule = Map.get(grammar.rules, name, Rule.new(name, epsilon?))
    clause = Clause.new(substitution, function)
    updated_rule = Rule.add_clause(rule, clause)
    %{grammar | rules: Map.put(grammar.rules, name, updated_rule)}
  end

  @doc """
  Use this function after defining all the rules of the grammar to prepare the grammar for parsing.

  This function will proceed to grammar validation, and returns errors if any are found.

  > ⚠️ A grammar that is not prepared cannot be used for parsing, so `step/2` and `loop/2` will behave unexpectedly.
  """
  @spec prepare(t()) ::
          {:ok, t()}
          | {:error, :missing_rules, [RulesChecker.miss()]}
          | {:error, :cycles_found, [RulesChecker.path()]}
          | {:error, :ambiguities_found, [RulesChecker.ambiguity()]}
  def prepare(%__MODULE__{} = grammar) do
    with :ok <- RulesChecker.check_all_rules_exist(grammar),
         :ok <- RulesChecker.check_rules_are_non_left_recursive(grammar),
         grammar = compute_first(grammar),
         :ok <- RulesChecker.check_rules_are_not_ambiguous(grammar) do
      {:ok, grammar}
    end
  end

  @doc """
  Same as prepare/1 but raises a RuntimeError if an error is found.
  """
  @spec prepare!(t()) :: t()
  def prepare!(grammar) do
    case prepare(grammar) do
      {:ok, grammar} -> grammar
      {:error, error_kind, data} -> raise "\n" <> RulesChecker.stringify_error(error_kind, data)
    end
  end

  @doc false
  @spec done?(t()) :: boolean()
  def done?(%__MODULE__{} = grammar), do: grammar.stack == []

  @doc """
  Reset the grammar to its initial state, and set the starting rule name.

  Usually this function is called once before `loop/2`to process some input.

  Other usage may be to start at different rules within the same grammar, mainly for in dev testing.

  ## Example

      iex> g = Grammar.new()
      ...> |> Grammar.add_clause(:start, ["<", :lists?, ">"], fn [_, ls, _] -> ls || [] end)
      ...> |> Grammar.add_clause(:lists?, [:list, :lists?], fn [l, ls] -> [l | ls || []] end, true)
      ...> |> Grammar.add_clause(:list, ["[", :elements?, "]"], fn [_, es, _] -> es || [] end)
      ...> |> Grammar.add_clause(:elements?, [:element, :elements?], fn [e, es] -> [e | es || []] end, true)
      ...> |> Grammar.add_clause(:element, [~r/[a-z]+/], fn [value] -> value end)
      ...> |> Grammar.prepare!()
      ...>
      iex> g
      ...> |> Grammar.start(:start)
      ...> |> Grammar.loop(Grammar.Tokenizer.new("<[a b c] [dd ee ff]>"))
      {:ok, [["a", "b", "c"], ["dd", "ee", "ff"]]}
      ...>
      iex> g = Grammar.start(g, :element)
      ...> Grammar.loop(g, Grammar.Tokenizer.new("<[a b c] [dd ee ff]>"))
      {:error, {1, 1}, :no_clause_matched}
      iex> Grammar.loop(g, Grammar.Tokenizer.new("ff"))
      {:ok, "ff"}
      iex> g = Grammar.start(g, :list)
      ...> Grammar.loop(g, Grammar.Tokenizer.new("<[a b c] [dd ee ff]>"))
      {:error, {1, 1}, :no_clause_matched}
      iex> Grammar.loop(g, Grammar.Tokenizer.new("[a b c]"))
      {:ok, ["a", "b", "c"]}

  """
  @spec start(t(), Rule.name()) :: t()
  def start(%__MODULE__{rules: rules} = grammar, rule_name) when is_map_key(rules, rule_name) do
    grammar
    |> reset()
    |> struct(stack: [rule_name])
  end

  @doc false
  @spec reset(t()) :: t()
  def reset(%__MODULE__{} = grammar) do
    %{grammar | stack: [], heap: []}
  end

  @doc false
  @spec compute_first(t()) :: t()
  def compute_first(%__MODULE__{} = grammar) do
    grammar.rules
    |> Enum.map(&elem(&1, 1))
    |> Enum.reduce(grammar, &compute_firsts_for_rule(&2, &1))
  end

  @spec compute_firsts_for_rule(t(), Rule.t()) :: t()
  defp compute_firsts_for_rule(%__MODULE__{firsts: firsts} = grammar, %Rule{name: name}) when is_map_key(firsts, name) do
    grammar
  end

  defp compute_firsts_for_rule(%__MODULE__{} = grammar, %Rule{} = rule) do
    {firsts_for_clauses, grammar} = Enum.map_reduce(rule.clauses, grammar, &compute_firsts_for_clause(&2, &1))
    firsts_updated = Map.put(grammar.firsts, rule.name, firsts_for_clauses)
    %{grammar | firsts: firsts_updated}
  end

  @spec compute_firsts_for_clause(t(), Clause.t()) :: {[term()], t()}
  defp compute_firsts_for_clause(%__MODULE__{firsts: firsts} = grammar, %Clause{substitution: [rule_name | _tail]})
       when is_map_key(firsts, rule_name) do
    {Enum.concat(Map.get(firsts, rule_name)), grammar}
  end

  defp compute_firsts_for_clause(%__MODULE__{} = grammar, %Clause{substitution: [rule_name | _tail]} = clause)
       when is_atom(rule_name) do
    grammar
    |> compute_firsts_for_rule(Map.get(grammar.rules, rule_name))
    |> compute_firsts_for_clause(clause)
  end

  defp compute_firsts_for_clause(%__MODULE__{} = grammar, %Clause{substitution: [prototype | _tail]}) do
    {[prototype], grammar}
  end

  @spec firsts(t(), Rule.t()) :: [term()]
  defp firsts(%__MODULE__{} = grammar, %Rule{} = rule), do: Enum.concat(Map.get(grammar.firsts, rule.name))

  @doc """
  Step once in the input Tokenizer, using `grammar`.

  This function returns a tuple which first term is either `:cont` or `:halt`.

  -  `:cont`: the second term is the updated `grammar` and the third term is the updated `tokenizer`.
  -  `:halt`: the second term is an error tuple, the third term is the position of the error in the input string, the fourth term is the `grammar` and the fifth term is the `tokenizer`.

  This function is used internally by `loop/2`, though it can be used to manually step through the parsing process.

  ## Example

  It takes 5 steps to reach the fist element of the input list.

      iex> grammar = Grammar.new()
      ...> |> Grammar.add_clause(:start, [:loop?], &Enum.at(&1, 0, []))
      ...> |> Grammar.add_clause(:loop?, [:element, :loop?], fn [head, tail] -> [head | tail || []] end, true)
      ...> |> Grammar.add_clause(:element, [:ident], &Enum.at(&1, 0))
      ...> |> Grammar.add_clause(:ident, [~r/[a-z]+/], fn [ident] ->  ident end)
      ...> |> Grammar.prepare!()
      ...> |> Grammar.start(:start)
      iex> tokenizer = Grammar.Tokenizer.new("a b c d")
      iex> {:cont, grammar, tokenizer} = Grammar.step(grammar, tokenizer)
      iex> {:cont, grammar, tokenizer} = Grammar.step(grammar, tokenizer)
      iex> {:cont, grammar, tokenizer} = Grammar.step(grammar, tokenizer)
      iex> {:cont, grammar, tokenizer} = Grammar.step(grammar, tokenizer)
      iex> {:cont, grammar, tokenizer} = Grammar.step(grammar, tokenizer)
      iex> [{_callback, 1, ["a"]} | _] = grammar.heap
  """
  @spec step(t(), Tokenizer.t()) ::
          {:cont, t(), Tokenizer.t()}
          | {:halt, :eof, t(), Tokenizer.t()}
          | {:halt, error(), t(), Tokenizer.t()}
  def step(%__MODULE__{stack: []} = grammar, %Tokenizer{} = tokenizer), do: {:halt, :eof, grammar, tokenizer}

  def step(%__MODULE__{stack: [name | stack_bottom], rules: rules} = grammar, %Tokenizer{} = tokenizer)
      when is_atom(name) and is_map_key(rules, name) do
    rule = Map.get(rules, name)

    case Tokenizer.current_token(tokenizer, firsts(grammar, rule)) do
      {{nil, cursor}, tokenizer} ->
        if rule.epsilon? do
          grammar =
            grammar
            |> struct(stack: stack_bottom)
            |> heap_store(nil)

          {:cont, grammar, tokenizer}
        else
          {:halt, {:no_clause_matched, cursor}, grammar, tokenizer}
        end

      {{token, _cursor}, tokenizer} ->
        {:cont, apply_rule(grammar, rule, token), tokenizer}
    end
  end

  def step(%__MODULE__{stack: [name | _stack_bottom]}, %Tokenizer{}) when is_atom(name) do
    raise "Unknown rule: #{name}. Ensure your grammar is Grammar.prepare/1'd."
  end

  def step(%__MODULE__{stack: [token_prototype | stack_bottom]} = grammar, %Tokenizer{} = tokenizer) do
    case Tokenizer.next_token(tokenizer, token_prototype) do
      {{nil, cursor}, tokenizer} ->
        {:halt, {:no_token, cursor}, grammar, tokenizer}

      {{token, _cursor}, tokenizer} ->
        grammar =
          grammar
          |> struct(stack: stack_bottom)
          |> heap_store(token)

        {:cont, grammar, tokenizer}
    end
  end

  @doc """
  Process the input tokenizer, until completion or error.
  """
  @spec loop(t(), Tokenizer.t()) :: {:ok, any()} | {:error, error()}
  def loop(%__MODULE__{} = grammar, %Tokenizer{} = tokenizer) do
    case step(grammar, tokenizer) do
      {:cont, grammar, tokenizer} ->
        loop(grammar, tokenizer)

      {:halt, :eof, grammar, _tokenizer} ->
        {:ok, grammar.heap}

      {:halt, {error, cursor}, _grammar, _tokenizer} ->
        {:error, cursor, error}
    end
  end

  @spec apply_rule(t(), Rule.t(), term()) :: t()
  defp apply_rule(%__MODULE__{stack: [name | stack_bottom]} = grammar, %Rule{name: name} = rule, token) do
    clause_idx =
      grammar.firsts
      |> Map.get(name)
      |> Enum.find_index(fn firsts_of_clause -> Enum.any?(firsts_of_clause, &TokenExtractor.match?(&1, token)) end)

    clause = Enum.at(rule.clauses, clause_idx)

    stack =
      clause.substitution
      |> Kernel.++(stack_bottom)

    grammar
    |> struct(stack: stack)
    |> heap_push(clause.function, length(clause.substitution))
  end

  defp heap_push(grammar, function, slot_count) when slot_count > 0 do
    %{grammar | heap: [{function, slot_count, []} | grammar.heap]}
  end

  defp heap_store(%{heap: []} = grammar, value) do
    %{grammar | heap: value}
  end

  defp heap_store(%{heap: [{function, slot_count, slots} | bottom]} = grammar, value) when slot_count > 0 do
    grammar
    |> struct(heap: [{function, slot_count - 1, slots ++ [value]} | bottom])
    |> heap_unwind()
  end

  defp heap_unwind(%{heap: [{function, 0, slots} | bottom]} = grammar) do
    value = function.(slots)
    heap_store(%{grammar | heap: bottom}, value)
  end

  defp heap_unwind(grammar), do: grammar

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
    do: CodeGen.store_clause(name, meta, def, blk, false)

  @doc """
  Same as `rule/2` but relaxed : if the rule cannot be matched, it will be valued as `nil`.

  Useful for optional or recursive rules.

  See example in `rule/2`.
  """

  defmacro rule?({name, meta, def}, do: blk) when is_atom(name),
    do: CodeGen.store_clause(name, meta, def, blk, true)

  @allowed_opts [drop_spaces: true]

  defmacro __using__(opts) do
    opts = Keyword.validate!(opts, @allowed_opts)
    drop_spaces? = Keyword.get(opts, :drop_spaces, true)

    quote do
      @compile {:inline, []}

      @drop_spaces? unquote(drop_spaces?)
      Module.register_attribute(__MODULE__, :rules, accumulate: true)

      @before_compile unquote(__MODULE__)

      import unquote(__MODULE__)
    end
  end

  defmacro __before_compile__(_env) do
    all_rules = Module.get_attribute(__CALLER__.module, :rules) |> Enum.reverse()

    drop_spaces? = Module.get_attribute(__CALLER__.module, :drop_spaces?)

    grammar_ast = CodeGen.build_grammar(__CALLER__.module, all_rules)

    rule_clause_body_functions = CodeGen.build_rule_body_functions(all_rules)

    quote do
      alias Grammar.Tokenizer
      alias Grammar.Tokenizer.TokenExtractor

      @grammar unquote(grammar_ast)

      def parse(input) do
        tokenizer = Tokenizer.new(input, unquote(drop_spaces?))
        Grammar.loop(@grammar, tokenizer)
      end

      unquote_splicing(rule_clause_body_functions)
    end
  end
end
