defprotocol Grammar.Tokenizer.TokenExtractor do
  @moduledoc """
  This protocol exposes the functions needed to extract tokens from the input string.
  """

  @doc """
  Try to read a token from the input string.

  If the token is found, returns the token and its length in the input string.
  Returns nil otherwise.

  ## Example

      iex> Grammar.Tokenizer.TokenExtractor.try_read("hello", "hello world")
      {"hello", 5}

      iex> Grammar.Tokenizer.TokenExtractor.try_read("Hello", "hello world")
      nil

      iex> Grammar.Tokenizer.TokenExtractor.try_read(~r/[0-9]+/, "013456toto")
      {"013456", 6}

      iex> Grammar.Tokenizer.TokenExtractor.try_read(~r/[a-z]+/, "013456toto")
      nil
  """
  @spec try_read(t, String.t()) :: nil | {String.t(), integer()}
  def try_read(token_prototype, input_string)

  @doc """
  Returns true if the token matches the token prototype.

  This function is used orient the parser in the right clause of a rule, by comparing the current token
  to the clause list of first tokens.

  ## Example

      iex> Grammar.Tokenizer.TokenExtractor.match?("hello", "hello")
      true

      iex> Grammar.Tokenizer.TokenExtractor.match?("hello", "Horld")
      false

      iex> Grammar.Tokenizer.TokenExtractor.match?(~r/[0-9]+/, "013456")
      true

      iex> Grammar.Tokenizer.TokenExtractor.match?(~r/[0-9]+/, "a013456")
      false
  """
  @spec match?(t, term()) :: boolean()
  def match?(prototype, token)
end

defimpl Grammar.Tokenizer.TokenExtractor, for: BitString do
  def try_read(token_prototype, input_string) do
    case input_string do
      <<^token_prototype::binary, _rest::binary>> -> {token_prototype, String.length(token_prototype)}
      _ -> nil
    end
  end

  def match?(token, token) when is_binary(token), do: true
  def match?(_token, _string), do: false
end

defimpl Grammar.Tokenizer.TokenExtractor, for: Regex do
  alias Grammar.Tokenizer.TokenExtractorHelper

  def try_read(token_prototype, input_string) do
    token_prototype
    |> TokenExtractorHelper.normalize_regex()
    |> TokenExtractorHelper.try_read_from_regex(input_string)
  end

  def match?(regex, token) when is_binary(token) do
    [token] == Regex.run(regex, token)
  end

  def match?(_regex, _token), do: false
end
