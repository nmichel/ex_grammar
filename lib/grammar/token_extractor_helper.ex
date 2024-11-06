defmodule Grammar.Tokenizer.TokenExtractorHelper do
  @moduledoc """
  This module provides helper functions to work with TokenExtractor implementations using Regex.
  """
  @spec normalize_regex(Regex.t()) :: Regex.t()
  def normalize_regex(regex) do
    source = Regex.source(regex)
    opt = Regex.opts(regex)

    case source do
      "^" <> _rest -> regex
      source -> Regex.compile!("^#{source}", opt)
    end
  end

  @spec try_read_from_regex(Regex.t(), String.t()) :: nil | {String.t(), integer()}
  def try_read_from_regex(pattern, input_string) do
    case Regex.run(pattern, input_string) do
      nil -> nil
      [match] -> {match, byte_size(match)}
    end
  end
end
