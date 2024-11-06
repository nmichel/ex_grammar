defmodule SpecialTokens do
  @moduledoc """
  This module defines special tokens that are not included in the standard library, primarily for testing purposes.
  """
  defmodule IP do
    @moduledoc false
    defstruct ip: nil
  end

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

  defimpl String.Chars, for: IP do
    def to_string(%IP{ip: [a, b, c, d]}) do
      "#{a}.#{b}.#{c}.#{d}"
    end
  end

  defmodule Number do
    @moduledoc false
    defstruct number: nil
  end

  defimpl Grammar.Tokenizer.TokenExtractor, for: Number do
    @pattern ~r/^[0-9]+/

    alias Grammar.Tokenizer.TokenExtractorHelper

    def try_read(_token_prototype, input_string) do
      TokenExtractorHelper.try_read_from_regex(@pattern, input_string)
      |> case do
        nil ->
          nil

        {extracted_string, length} ->
          token =
            extracted_string
            |> String.to_integer()
            |> then(&struct(Number, number: &1))

          {token, length}
      end
    end

    def match?(%Number{}, %Number{}), do: true
    def match?(_prototype, _token), do: false
  end

  defimpl String.Chars, for: Number do
    def to_string(%Number{number: number}) do
      "#{number}"
    end
  end

  defmodule QuotedString do
    @moduledoc false
    defstruct string: nil
  end

  defimpl Grammar.Tokenizer.TokenExtractor, for: QuotedString do
    @pattern ~r/^"[^"]*"/

    alias Grammar.Tokenizer.TokenExtractorHelper

    def try_read(_token_prototype, input_string) do
      TokenExtractorHelper.try_read_from_regex(@pattern, input_string)
      |> case do
        nil ->
          nil

        {extracted_string, length} ->
          token = struct(QuotedString, string: extracted_string)
          {token, length}
      end
    end

    def match?(%QuotedString{}, %QuotedString{}), do: true
    def match?(_prototype, _token), do: false
  end

  defimpl String.Chars, for: QuotedString do
    def to_string(%QuotedString{string: string}) do
      "#{string}"
    end
  end

  defmodule Identifier do
    @moduledoc false
    defstruct string: nil
  end

  defimpl Grammar.Tokenizer.TokenExtractor, for: Identifier do
    @pattern ~r/^[a-zA-Z]+[a-zA-Z0-9_]*/

    alias Grammar.Tokenizer.TokenExtractorHelper

    def try_read(_token_prototype, input_string) do
      TokenExtractorHelper.try_read_from_regex(@pattern, input_string)
      |> case do
        nil ->
          nil

        {extracted_string, length} ->
          token = struct(Identifier, string: extracted_string)
          {token, length}
      end
    end

    def match?(%Identifier{}, %Identifier{}), do: true
    def match?(_prototype, _token), do: false
  end

  defimpl String.Chars, for: Identifier do
    def to_string(%Identifier{string: string}) do
      "#{string}"
    end
  end
end
