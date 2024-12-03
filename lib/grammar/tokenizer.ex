defmodule Grammar.Tokenizer do
  @moduledoc """
  This module extracts the tokens from the input string.

  It is driven by the parser to search for specific tokens only, when required.
  """

  alias Grammar.Tokenizer.TokenExtractor

  @enforce_keys [:input, :drop_spaces?]
  defstruct input: "", current_line: 1, current_column: 1, drop_spaces?: true, sub_byte_matching?: false

  @type t :: %__MODULE__{
          input: binary(),
          current_line: integer(),
          current_column: integer(),
          drop_spaces?: boolean(),
          sub_byte_matching?: boolean()
        }

  @newlines ~c[\n]
  @whitespaces ~c[ \t\v\f\r]

  @doc """
  Creates a new tokenizer for a given input.

  ## Parameters

    - input: bitstring from which token will be extracted.
    - drop_spaces?: when set to `false`, the tokenizer will not drop spaces and newlines.

  ## Examples

      iex> Grammar.Tokenizer.new("this is the input")
      %Grammar.Tokenizer{
        input: "this is the input",
        current_line: 1,
        current_column: 1,
        drop_spaces?: true
      }

      iex> Grammar.Tokenizer.new("this is the input", false)
      %Grammar.Tokenizer{
        input: "this is the input",
        current_line: 1,
        current_column: 1,
        drop_spaces?: false
      }
  """
  @spec new(binary(), boolean()) :: t()
  def new(input, drop_spaces? \\ true, sub_byte_matching? \\ false) when is_bitstring(input) do
    struct(__MODULE__, input: input, drop_spaces?: drop_spaces?, sub_byte_matching?: sub_byte_matching?)
    |> maybe_drop_spaces()
  end

  @doc """
  Returns the current token found in the input, if any.
  Expected tokens are passed as a list of token prototypes.

  The token is not consumed, so two succesive calls to `current_token/2` will return the same token.

  ## Examples

      iex> tokenizer = Grammar.Tokenizer.new("hello world")
      %Grammar.Tokenizer{
        input: "hello world",
        current_line: 1,
        current_column: 1,
        drop_spaces?: true
      }
      iex> {{"hello", {1, 1}}, _} = Grammar.Tokenizer.current_token(tokenizer, ["hello"])
      iex> {{"hello", {1, 1}}, _} = Grammar.Tokenizer.current_token(tokenizer, ["hello"])
      iex> {{nil, {1, 1}}, _} = Grammar.Tokenizer.current_token(tokenizer, ["world"])
  """
  @spec current_token(t(), [any()]) :: {any(), t()}
  def current_token(%__MODULE__{} = tokenizer, token_prototypes) do
    {token, _} = try_read_token(tokenizer, token_prototypes)
    {token, tokenizer}
  end

  @doc """
  Returns the current token found in the input, and consumes it.
  The expected token prototype is passed as second parameter.

  ## Examples

      iex> tokenizer = Grammar.Tokenizer.new("hello world")
      iex> {{"hello", {1, 1}}, tokenizer} = Grammar.Tokenizer.next_token(tokenizer, "hello")
      iex> {{"world", {1, 7}}, _} = Grammar.Tokenizer.next_token(tokenizer, "world")
  """
  @spec next_token(t(), any()) :: {any(), t()}
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

  @spec try_read_token(t(), [any()]) :: {any(), integer()}
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

  @spec consume_token(t(), integer()) :: t()
  defp consume_token(%__MODULE__{sub_byte_matching?: true} = tokenizer, token_length) do
    <<_token::size(token_length), rest::bitstring>> = tokenizer.input
    %{tokenizer | input: rest, current_column: tokenizer.current_column + token_length}
  end

  defp consume_token(%__MODULE__{} = tokenizer, token_length) do
    input = tokenizer.input
    {token_string, rest} = String.split_at(input, token_length)
    tokenizer = update_cursor(tokenizer, token_string)
    %{tokenizer | input: rest}
  end

  @spec update_cursor(t(), binary()) :: t()
  defp update_cursor(%__MODULE__{} = tokenizer, <<"">>), do: tokenizer

  defp update_cursor(%__MODULE__{} = tokenizer, <<c::utf8, tail::binary>>) when c in @newlines do
    update_cursor(%{tokenizer | current_line: tokenizer.current_line + 1, current_column: 0}, tail)
  end

  defp update_cursor(%__MODULE__{} = tokenizer, <<_c::utf8, tail::binary>>) do
    update_cursor(%{tokenizer | current_line: tokenizer.current_line, current_column: tokenizer.current_column + 1}, tail)
  end

  @spec maybe_drop_spaces(t()) :: t()
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
