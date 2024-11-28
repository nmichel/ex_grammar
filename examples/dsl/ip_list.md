
# List of IP addresses

This example illustrates the use of [TokenExtractor](`Grammar.Tokenizer.TokenExtractor`) protocol for user defined token.

First, we define the token prototype to catch IP address. In our simple case, the field `ip` will contain the list of integer values that compose the address.

```elixir
defmodule IP do
  defstruct ip: nil
end
```

Then we implement the [TokenExtractor](`Grammar.Tokenizer.TokenExtractor`) protocol for that new token type.

Here we are providing a naive implementation, based on a (poor) regular expression and [TokenExtractorHelper](`Grammar.Tokenizer.TokenExtractorHelper`) helper module.

One is free to implement a more robust / efficient version, as long as it respects the protocol.

```elixir
defimpl Grammar.Tokenizer.TokenExtractor, for: IP do
  @pattern ~r/^\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}/

  alias Grammar.Tokenizer.TokenExtractorHelper

  def try_read(_token_prototype, input_string) do
    TokenExtractorHelper.try_read_from_regex(@pattern, input_string)
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

Then use this new token to define rules.

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
```

And we are done !

```elixir
$iex> IPListReader.parse("
   1.2.3.4 ,
   3.2.3.4 ,
   4.2.3.4
")

{:ok,
 [
   %IP{ip: ["1", "2", "3", "4"]},
   %IP{ip: ["3", "2", "3", "4"]},
   %IP{ip: ["4", "2", "3", "4"]}
 ]}
```
