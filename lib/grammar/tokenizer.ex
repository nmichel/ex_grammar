defmodule Grammar.Tokenizer do
  @moduledoc """
  This module extracts the tokens from the input string.

  It is driven by the parser to search for specific tokens only, when required.
  """

  alias Grammar.Tokenizer.TokenExtractor

  @enforce_keys [:input, :drop_spaces?]
  defstruct input: "", current_line: 1, current_column: 1, drop_spaces?: true

  @newlines ~c[\n]
  @whitespaces ~c[ \t\v\f\r]

  def new(input, drop_spaces? \\ true) when is_binary(input) do
    struct(__MODULE__, input: input, drop_spaces?: drop_spaces?)
    |> maybe_drop_spaces()
  end

  def current_token(%__MODULE__{} = tokenizer, token_prototypes) do
    {token, _} = try_read_token(tokenizer, token_prototypes)
    {token, tokenizer}
  end

  def next_token(%__MODULE__{} = tokenizer, token_prototype) do
    case try_read_token(tokenizer, [token_prototype]) do
      {token, 0} ->
        {token, tokenizer}

      {token, token_length} ->
        tokenizer =
          tokenizer
          |> consume_token(token_length)
          |> maybe_drop_spaces()

        {token, tokenizer}
    end
  end

  defp try_read_token(%__MODULE__{} = tokenizer, token_prototypes) do
    input = tokenizer.input
    cursor = {tokenizer.current_line, tokenizer.current_column}

    token_prototypes
    |> Enum.reduce({nil, 0}, fn token_template, {current_token, current_length} ->
      case TokenExtractor.try_read(token_template, input) do
        nil -> {current_token, current_length}
        {token, length} when length > current_length -> {token, length}
        _found_tuple -> {current_token, current_length}
      end
    end)
    |> then(fn {token, length} -> {{token, cursor}, length} end)
  end

  defp consume_token(%__MODULE__{} = tokenizer, token_length) do
    input = tokenizer.input
    {token_string, rest} = String.split_at(input, token_length)
    tokenizer = update_cursor(tokenizer, token_string)
    %{tokenizer | input: rest}
  end

  defp update_cursor(%__MODULE__{} = tokenizer, <<"">>), do: tokenizer

  defp update_cursor(%__MODULE__{} = tokenizer, <<c::utf8, tail::binary>>) when c in @newlines do
    update_cursor(%{tokenizer | current_line: tokenizer.current_line + 1, current_column: 0}, tail)
  end

  defp update_cursor(%__MODULE__{} = tokenizer, <<_c::utf8, tail::binary>>) do
    update_cursor(%{tokenizer | current_line: tokenizer.current_line, current_column: tokenizer.current_column + 1}, tail)
  end

  defp maybe_drop_spaces(%__MODULE__{drop_spaces?: false} = tokenizer), do: tokenizer
  defp maybe_drop_spaces(%__MODULE__{drop_spaces?: true} = tokenizer), do: drop_spaces(tokenizer)

  defp drop_spaces(%__MODULE__{input: <<c::utf8, tail::binary>>} = tokenizer) when c in @newlines do
    drop_spaces(%{tokenizer | current_line: tokenizer.current_line + 1, current_column: 1, input: tail})
  end

  defp drop_spaces(%__MODULE__{input: <<c::utf8, tail::binary>>} = tokenizer) when c in @whitespaces do
    drop_spaces(%{tokenizer | input: tail, current_column: tokenizer.current_column + 1})
  end

  defp drop_spaces(%__MODULE__{} = tokenizer) do
    tokenizer
  end
end
