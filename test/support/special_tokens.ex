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
    @pattern ~r/^\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}/

    def try_read(_token_prototype, input_string) do
      Grammar.TokenExtractorHelper.try_read_from_regex(@pattern, input_string)
    end
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
    @pattern ~r/^[0-9]+/

    def try_read(_token_prototype, input_string) do
      Grammar.TokenExtractorHelper.try_read_from_regex(@pattern, input_string)
    end
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
    @pattern ~r/^"[\S\s]*"/

    def try_read(_token_prototype, input_string) do
      Grammar.TokenExtractorHelper.try_read_from_regex(@pattern, input_string)
    end
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
    @pattern ~r/^[a-zA-Z]+[a-zA-Z0-9_]*/

    def try_read(_token_prototype, input_string) do
      Grammar.TokenExtractorHelper.try_read_from_regex(@pattern, input_string)
    end
  end
end
