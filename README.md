# Grammar

*One rule to ring them all*

Easily build parsers / transformers for structured data.

## Overview

An Elixir library that helps building parsers / transformers for LL(1) structured data. 

## Usage

> Please have a look at the examples for more advanced use cases

### API 

The following grammar extracts a list of integers from string of comma separated number, and enclosed by square brackets.

```elixir
  Grammar.new()
  |> Grammar.add_clause(:list, ["[", :content, "]"], fn ["[", list, "]"] -> list || [] end)
  |> Grammar.add_clause(:content, [:number, :list_tail], fn [number, list] -> [number | list || []] end, true)
  |> Grammar.add_clause(:list_tail, [",", :number, :list_tail], fn [",", number, list] -> [number | list || []] end, true)
  |> Grammar.add_clause(:number, [~r/[0-9]+/], fn [number] -> String.to_integer(number) end)
  |> Grammar.prepare!()
  |> Grammar.start(:list)
  |> Grammar.loop(Grammar.Tokenizer.new("[1, 2, 6, 12]"))

  {:ok, [1, 2, 6, 12]}
```

### DSL 

`use Grammar` and write "rule" functions that have direct access to parsing results.

You are fully in control of what is returned by each rule function.

Under the hood, the `rule/2` macro uses the regular API to build a `Grammar` at compile time, and bury it in the module you're defining. 

#### Example

This is the same grammar parser as in the [API section](#api), but under the form of a dedicated module.

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
{:ok, [1, 2, 6, 12]}
```
