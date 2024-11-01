defmodule TokenizerTest do
  use ExUnit.Case

  test "test tokenizer" do
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

    tokenizer = MyGrammar.Tokenizer.new(~S/  coucou
      1234
        "blablabla coucou 12234"
    """
        bla  bla
       """
       re 12
    /)

    assert [
             {"12", {7, 11}},
             {"re", {7, 8}},
             {"\"blablabla coucou 12234\"\n    \"\"\"\n        bla  bla\n       \"\"\"", {3, 9}},
             {"1234", {2, 7}},
             {"coucou", {1, 3}}
           ] = Enum.reduce(tokenizer, [], fn e, a -> [e | a] end)
  end
end
