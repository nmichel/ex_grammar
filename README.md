# Grammar

*One rule to ring them all*

Easily build parsers / transformers for structured data.

## Overview

An Elixir library that helps building parsers / transformers for LL(1) structured data, via two simple macros, and a protocol.

## Usage

### Parse a list of numbers

```elixir
defmodule NumberListReader do
  use Grammar

  rule list("[", :list_tail, "]") do
    [_, list, _] = params
    case list do
      nil -> []
      _ -> list
    end
  end

  rule? list_tail(:number, :list_tail_tail) do
    [number, list] = params
    [number | (list || [])]
  end

  rule? list_tail_tail(",", :number, :list_tail_tail) do
    [_, number, list] = params
    [number | (list || [])]
  end

  rule number(~r/[0-9]+/) do
    [number] = params
    String.to_integer(number)
  end
end

$iex> NumberListReader.parse("[1, 2, 6, 12]")
{%NumberListReader.Tokenizer{input: "", current_line: 1, current_column: 14}, [1, 2, 6, 12]}
```

### Parse simple math expressions

```elixir
defmodule MathExpressionTranslator do
  use Grammar

  rule start(:expression) do
    [program] = params
    "#{program}"
  end

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

$iex> MathExpressionTranslator.parse("1+3*4/(3-1)")
{%MathExpressionTranslator.Tokenizer{
   input: "",
   current_line: 1,
   current_column: 12
 }, "1 plus 3 multiplié par 4 divisé par (3 moins 1)"}
```


### Parse a list of IP addresses

Define a token prototype to catch IP address:

```elixir
defmodule IP do
  defstruct ip: nil
end

defimpl Grammar.Tokenizer.TokenExtractor, for: IP do
  @pattern ~r/^\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}/

  def try_read(_token_prototype, input_string) do
    Grammar.Tokenizer.TokenExtractorHelper.try_read_from_regex(@pattern, input_string)
    |> case do
      nil ->
        nil

      {extracted_string, length} ->
        token =
          extracted_string
          |> String.split(".")
          |> then(&struct(IP, ip: &1))

        {token, length}
    end
  end

  def match?(%IP{}, %IP{}), do: true
  def match?(_prototype, _token), do: false
end
```

Then use this new token to define rules:

```elixir
defmodule IPListReader do
  use Grammar

  rule ips(%IP{}, :ips_tail) do
    [ip, tail] = params
    [ip | (tail || [])]
  end

  rule? ips_tail(",", %IP{}, :ips_tail) do
    [_, ip, tail] = params
    [ip | (tail || [])]
  end
end

$iex> IPListReader.parse("
   1.2.3.4 ,
   3.2.3.4 ,
   4.2.3.4
")

{%IPListReader.Tokenizer{input: "", current_line: 5, current_column: 1},
 [
   %IP{ip: ["1", "2", "3", "4"]},
   %IP{ip: ["3", "2", "3", "4"]},
   %IP{ip: ["4", "2", "3", "4"]}
 ]}
```
