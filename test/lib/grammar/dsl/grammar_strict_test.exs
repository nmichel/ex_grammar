defmodule GrammarStrictTest do
  use ExUnit.Case

  describe "with drop_spaces mode on" do
    defmodule HelloWorldStrict do
      use Grammar

      rule start("hello", :world) do
        [_, world] = params
        "hello #{world}"
      end

      rule world("world", "!"), do: "world !"
    end

    test "all tokens are required" do
      assert {:ok, "hello world !"} = HelloWorldStrict.parse("hello world !")
      assert {:error, {1, 6}, :no_clause_matched} = HelloWorldStrict.parse("     ")
      assert {:error, {1, 6}, :no_clause_matched} = HelloWorldStrict.parse("hello")
      assert {:error, {1, 12}, :no_token} = HelloWorldStrict.parse("hello world")
    end

    test "spaces and linebreaks do not matter" do
      assert {:ok, "hello world !"} = HelloWorldStrict.parse("\n\nhello    \t world\n !\n\n")
    end
  end

  describe "with drop_spaces mode off" do
    defmodule HelloWorldStrictDropSpaceOff do
      use Grammar, drop_spaces: false

      rule start("hello", :world) do
        [_, world] = params
        "hello #{world}"
      end

      rule world("world", :spaces, "!"), do: "world !"

      rule spaces(~r/[\s]+/), do: nil
    end

    test "all tokens are required" do
      assert {:ok, "hello world !"} = HelloWorldStrictDropSpaceOff.parse("helloworld !")
      assert {:ok, "hello world !"} = HelloWorldStrictDropSpaceOff.parse("helloworld\n\n!")
    end

    test "unhandled spacing is forbidden" do
      assert {:error, {1, 6}, :no_clause_matched} = HelloWorldStrictDropSpaceOff.parse("hello world !")
      assert {:error, {1, 1}, :no_clause_matched} = HelloWorldStrictDropSpaceOff.parse(" helloworld !")
    end
  end
end
