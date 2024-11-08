# Grammar

*One rule to ring them all*

Easily build parsers / transformers for structured data.

## Overview

An Elixir library that helps building parsers / transformers for LL(1) structured data, via two simple macros, and a protocol.

## Usage

> Please have a look at the examples for more advanced use cases

`use Grammar` and write "rule" functions that have direct access to parsing results.

You are fully in control of what is returned by each rule function.

#### Example

The following module extracts a list of integers from string of comma separated number, and enclosed by square brackets.

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
