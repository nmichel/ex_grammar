# Math expression to function

> ⚠️ Consider reading [Math to french](./math_to_french.md) example first.

The module `MathToFunction` can read simple integer math expressions too,
but this time the parse/1 function returns a function that produce the value of the expression when called.

```
iex> {:ok, calcfn} = MathToFunction.parse("(42-2)*12 / 5")
iex> calcfn.()
96.0
```

Here follows its declaration, a little bit more chalenging.

```elixir
defmodule MathToFunction do
  use Grammar

  rule expression(:term, :expression_cont) do
    [term, cont] = params
    fn -> (cont || & &1).(term.()) end
  end

  rule? expression_cont("+", :term, :expression_cont) do
    [_, term, cont] = params
    fn left -> (cont || & &1).((left + term.())) end
  end

  rule? expression_cont("-", :term, :expression_cont) do
    [_, term, cont] = params
    fn left -> (cont || & &1).((left - term.())) end
  end

  rule term(:factor, :term_cont) do
    [factor, cont] = params
    fn -> (cont || & &1).(factor.()) end
  end

  rule? term_cont("*", :factor, :term_cont) do
    [_, factor, cont] = params
    fn left -> (cont || & &1).((left * factor.())) end
  end

  rule? term_cont("/", :factor, :term_cont) do
    [_, factor, cont] = params
    fn left -> (cont || & &1).((left / factor.())) end
  end

  rule factor(:number) do
    [number] = params
    number
  end

  rule factor("(", :expression, ")") do
    [_, expression, _] = params
    expression
  end

  rule number(~r/[0-9]+/) do
    [number] = params
    number_value = String.to_integer(number)
    fn -> number_value end
  end
end
```
