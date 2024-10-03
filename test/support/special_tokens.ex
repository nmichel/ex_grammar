defmodule SpecialTokens do
  defmodule IP do
    defstruct ip: nil
  end

  defimpl Grammar.TokenMatcher, for: IP do
    def match?(_ip, token) do
      r = ~r/^\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}/
      case Regex.run(r, token) do
        [match] when byte_size(match) == byte_size(token) -> true
        _ -> false
      end
    end
  end

  defimpl Grammar.TokenExtractor, for: IP do
    def pattern(_token), do: ~r/^\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}/
  end

  defmodule Number do
    defstruct number: nil
  end

  defimpl Grammar.TokenMatcher, for: Number do
    def match?(_ip, token) do
      r = ~r/^[0-9]+/
      case Regex.run(r, token) do
        [match] when byte_size(match) == byte_size(token) -> true
        _ -> false
      end
    end
  end

  defimpl Grammar.TokenExtractor, for: Number do
    def pattern(_token), do: ~r/^[0-9]+/
  end

  defmodule QuotedString do
    defstruct string: nil
  end

  defimpl Grammar.TokenMatcher, for: QuotedString do
    def match?(_ip, token) do
      r = ~r/^"[\S\s]*"/
      case Regex.run(r, token) do
        [match] when byte_size(match) == byte_size(token) -> true
        _ -> false
      end
    end
  end

  defimpl Grammar.TokenExtractor, for: QuotedString do
    def pattern(_token), do: ~r/^"[\S\s]*"/
  end

  defmodule Identifier do
    defstruct string: nil
  end

  defimpl Grammar.TokenMatcher, for: Identifier do
    def match?(_ip, token) do
      r = ~r/^[a-zA-Z]+[a-zA-Z0-9_]*/
      case Regex.run(r, token) do
        [match] when byte_size(match) == byte_size(token) -> true
        _ -> false
      end
    end
  end

  defimpl Grammar.TokenExtractor, for: Identifier do
    def pattern(_token), do: ~r/^[a-zA-Z]+[a-zA-Z0-9_]*/
  end
end
