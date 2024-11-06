defmodule TokenizerTest do
  alias SpecialTokens.QuotedString
  alias Code.Identifier
  alias SpecialTokens.Number
  use ExUnit.Case

  doctest Grammar.TokenExtractor

  defmodule MyGrammar do
    use Grammar

    rule start(:number) do
      [number] = params
      number
    end

    rule start(:identifier) do
      [identifier] = params
      identifier
    end

    rule start(:string) do
      [string] = params
      string
    end

    rule start(:multi_line_comment) do
      [comment] = params
      comment
    end

    rule number(~r/[0-9]+/) do
      [number] = params
      number
    end

    rule identifier(~r/[a-zA-Z]+[a-zA-Z0-9]*/) do
      [string] = params
      string
    end

    rule string(~r/"[\S\s]*"/) do
      [string] = params
      string
    end

    rule multi_line_comment(~r/""".*?"""/s) do
      [string] = params
      string
    end
  end

  test "test tokenizer" do
    alias MyGrammar.Tokenizer

    alias SpecialTokens.{
      QuotedString,
      Number,
      Identifier
    }

    tokenizer = Tokenizer.new(~S/  coucou
      1234
        "blablabla coucou 1234
        bla  bla
       "
       re 12
    /)

    token_prototypes = [%Number{}, "coucou", %Identifier{}, %QuotedString{}]

    token_and_metas = [
      {{"coucou", {1, 3}}, "coucou"},
      {{%Number{number: 1234}, {2, 7}}, %Number{}},
      {{%QuotedString{string: "\"blablabla coucou 1234\n        bla  bla\n       \""}, {3, 9}}, %QuotedString{}},
      {{%Identifier{string: "re"}, {6, 8}}, %Identifier{}},
      {{%Number{number: 12}, {6, 11}}, %Number{}}
    ]

    Enum.reduce(token_and_metas, tokenizer, fn {token_and_meta_expected, token_prototype}, tokenizer ->
      {token_and_meta_found, tokenizer} = Tokenizer.current_token(tokenizer, token_prototypes)
      ^token_and_meta_expected = token_and_meta_found
      {^token_and_meta_expected, tokenizer} = Tokenizer.next_token(tokenizer, token_prototype)
      tokenizer
    end)
  end
end
