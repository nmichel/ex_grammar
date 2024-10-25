defmodule GrammarSpecialTokensTest do
  use ExUnit.Case

  defmodule MyGrammar do
    use Grammar

    alias SpecialTokens.IP
    alias SpecialTokens.Number
    alias SpecialTokens.QuotedString
    alias SpecialTokens.Identifier

    rule program(:assignment) do
      [assignment] = params
      "#{assignment}"
    end

    rule assignment(:identifier, "=", :rhv) do
      [identifier, _, rhv] = params
      "assigne #{rhv} à #{identifier}"
    end

    rule identifier(%Identifier{}) do
      [string] = params
      "#{string}"
    end

    rule rhv(:ip) do
      [ip] = params
      "l'adresse IP #{ip}"
    end

    rule rhv(:number) do
      [number] = params
      "le nombre #{number}"
    end

    rule rhv(:string) do
      [string] = params
      "la chaîne #{string}"
    end

    rule ip(%IP{}) do
      [ip] = params
      ip
    end

    rule number(%Number{}) do
      [ip] = params
      ip
    end

    rule string(%QuotedString{}) do
      [ip] = params
      ip
    end
  end

  test "grammar with special tokens" do
    assert {_, "assigne le nombre 12 à un_nombre"} = GrammarSpecialTokensTest.MyGrammar.parse("un_nombre = 12")

    assert {_, "assigne la chaîne \"ceci est une chaîne\" à une_chaine"} =
             GrammarSpecialTokensTest.MyGrammar.parse("une_chaine = \"ceci est une chaîne\"")

    assert {_, "assigne l'adresse IP 12.12.3.4 à une_ip"} = GrammarSpecialTokensTest.MyGrammar.parse("une_ip = 12.12.3.4")
  end
end
