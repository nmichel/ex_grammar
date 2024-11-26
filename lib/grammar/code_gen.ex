defmodule Grammar.CodeGen.Clause do
  @moduledoc """
  TODO:
  """

  @enforce_keys [:name, :def, :blk, :epsilon]
  defstruct [:name, :def, :blk, :epsilon]

  @type t :: %__MODULE__{
          name: atom(),
          def: [atom() | binary() | struct()],
          blk: any(),
          epsilon: boolean()
        }

  def new(name, def, blk, epsilon) when is_list(def) and is_atom(epsilon) do
    struct(__MODULE__, name: name, def: def, blk: blk, epsilon: epsilon)
  end
end

defmodule Grammar.CodeGen do
  @moduledoc """
  This module exposes functions required to generate the code for the parser derived from grammar rules.
  """

  alias Grammar.CodeGen.Clause

  def store_clause(rule_name, _meta, def, blk, epsilon) do
    def = Macro.escape(def)
    blk = Macro.escape(blk)

    quote do
      @rules Clause.new(unquote(rule_name), unquote(def), unquote(blk), unquote(epsilon))
    end
  end

  def build_grammar(module, clauses) do
    start_rule_name = hd(clauses).name

    seed = quote do: Grammar.new()

    clauses_decl =
      clauses
      |> Enum.group_by(& &1.name)
      |> Enum.flat_map(fn {_rule_name, clauses} ->
        clauses
        |> Enum.with_index()
      end)
      |> Enum.reduce(seed, fn {%Clause{name: rule_name, def: def, epsilon: epsilon}, index}, acc ->
        quote do
          unquote(acc)
          |> Grammar.add_clause(
            unquote(rule_name),
            unquote(def),
            unquote(build_body_clause_invokation(module, rule_name, index)),
            unquote(epsilon)
          )
        end
      end)

    quote do
      unquote(clauses_decl)
      |> Grammar.prepare!()
      |> Grammar.start(unquote(start_rule_name))
    end
  end

  def build_rule_body_functions(rules) do
    rules
    |> Enum.group_by(& &1.name)
    |> Enum.flat_map(fn {rule_name, clauses} ->
      clauses
      |> Enum.with_index()
      |> Enum.map(fn {%Clause{blk: blk}, index} ->
        function_name = build_body_clause_name(rule_name, index)

        quote do
          def unquote(function_name)(var!(params)) do
            _ = var!(params)
            unquote(blk)
          end
        end
      end)
    end)
  end

  defp build_body_clause_name(rule_name, index) do
    :"clause_#{index}_#{rule_name}"
  end

  defp build_body_clause_invokation(module, rule_name, index) do
    name = build_body_clause_name(rule_name, index)
    remote_mod_ast = quote do: unquote(module).unquote(name)

    quote do
      &(unquote(remote_mod_ast) / 1)
    end
  end
end
