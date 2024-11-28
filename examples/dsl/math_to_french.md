# Math expression to french

The module `MathToFrench` can read simple integer math expressions, like "1+2", "(42-2)*12 / 5",
and rewrite them in ... french.

```
iex> MathToFrench.parse("(42-2)*12 / 5")
{:ok, "(42 moins 2) multiplié par 12 divisé par 5"}
```

Here follows its declaration.

```elixir
defmodule MathToFrench do
  use Grammar

  # This rule wille be the entry point.
  # Note that we could get rid of it, as it add no real value to
  # the output.
  rule start(:expression) do
    [program] = params
    "#{program}"
  end

  # a math expression is a composed of "terms" joined by
  # symbol + or -.
  # This ensure precedence of other operators over the them.
  rule expression(:term, :expression_cont) do
    join_non_nil(params)
  end

  rule? expression_cont("+", :term, :expression_cont) do
    [_, term, cont] = params
    join_non_nil(["plus", term, cont])
  end

  rule? expression_cont("-", :term, :expression_cont) do
    [_, term, cont] = params
    join_non_nil(["moins", term, cont])
  end

  # A term is composed of elements joined by operators * or /.
  # Those elements are reduced by rule `factor`.
  rule term(:factor, :term_cont) do
    join_non_nil(params)
  end

  rule? term_cont("*", :factor, :term_cont) do
    [_, factor, cont] = params
    join_non_nil(["multiplié par", factor, cont])
  end

  rule? term_cont("/", :factor, :term_cont) do
    [_, factor, cont] = params
    join_non_nil(["divisé par", factor, cont])
  end

  # Eventually a factor is either a integer number or
  # a parenthesized expression, as in rule `expression`.
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

  defp join_non_nil(params) when is_list(params) do
    params |> Enum.reject(&is_nil/1) |> Enum.join(" ")
  end
end
```
