defmodule Grammar.TokenizerTest do
  use ExUnit.Case

  doctest Grammar.Tokenizer.TokenExtractor

  alias Grammar.SpecialTokens.Identifier
  alias Grammar.SpecialTokens.Number
  alias Grammar.SpecialTokens.QuotedString
  alias Grammar.Tokenizer

  test "test tokenizer" do
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

  test "spaces and linebreaks cuts Bitstring tokens" do
    tokenizer = Tokenizer.new("cou
    cou
    ")

    assert {{nil, {1, 1}}, _} = Tokenizer.next_token(tokenizer, "coucou")
    assert {{"cou", {1, 1}}, _} = Tokenizer.next_token(tokenizer, "cou")
  end
end
