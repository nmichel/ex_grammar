defmodule SpecialTokens do
  defmodule IP do
    defstruct ip: nil
  end

  defimpl Grammar.TokenMatcher, for: IP do
    def match?(%IP{}, %IP{}), do: true
    def match?(_prototype, _token), do: false
  end

  defimpl Grammar.TokenExtractor, for: IP do
    @pattern ~r/^\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}/

    def try_read(_token_prototype, input_string) do
      Grammar.TokenExtractorHelper.try_read_from_regex(@pattern, input_string)
      |> case do
        nil -> nil
        {extracted_string, length} ->
          token =
            extracted_string
            |> String.split(".")
            |> then(&struct(IP, ip: &1))
          {token, length}
      end
    end
  end

  defmodule Number do
    defstruct number: nil
  end

  defimpl Grammar.TokenMatcher, for: Number do
    def match?(%Number{}, %Number{}), do: true
    def match?(_prototype, _token), do: false
  end

  defimpl Grammar.TokenExtractor, for: Number do
    @pattern ~r/^[0-9]+/

    def try_read(_token_prototype, input_string) do
      Grammar.TokenExtractorHelper.try_read_from_regex(@pattern, input_string)
      |> case do
        nil -> nil
        {extracted_string, length} ->
          token =
            extracted_string
            |> String.to_integer()
            |> then(&struct(Number, number: &1))
          {token, length}
      end
    end
  end

  defmodule QuotedString do
    defstruct string: nil
  end

  defimpl Grammar.TokenMatcher, for: QuotedString do
    def match?(%QuotedString{}, %QuotedString{}), do: true
    def match?(_prototype, _token), do: false
  end

  defimpl Grammar.TokenExtractor, for: QuotedString do
    @pattern ~r/^"[\S\s]*"/

    def try_read(_token_prototype, input_string) do
      Grammar.TokenExtractorHelper.try_read_from_regex(@pattern, input_string)
      |> case do
        nil -> nil
        {extracted_string, length} ->
          token = struct(QuotedString, string: extracted_string)
          {token, length}
      end
    end
  end

  defmodule Identifier do
    defstruct string: nil
  end

  defimpl Grammar.TokenMatcher, for: Identifier do
    def match?(%Identifier{}, %Identifier{}), do: true
    def match?(_prototype, _token), do: false
  end

  defimpl Grammar.TokenExtractor, for: Identifier do
    @pattern ~r/^[a-zA-Z]+[a-zA-Z0-9_]*/

    def try_read(_token_prototype, input_string) do
      Grammar.TokenExtractorHelper.try_read_from_regex(@pattern, input_string)
      |> case do
        nil -> nil
        {extracted_string, length} ->
          token = struct(Identifier, string: extracted_string)
          {token, length}
      end
    end
  end
end
