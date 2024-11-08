defmodule GrammarRelaxedTest do
  use ExUnit.Case

  describe "with drop_spaces mode on" do
    defmodule HelloWorldRelaxed do
      use Grammar

      rule start("hello", :world, :bang) do
        [_, world, bang] = params
        "hello #{world || "Dolly"} #{bang}"
      end

      rule? world("world"), do: "world"

      rule bang("!"), do: "!"

      rule bang("?"), do: "?"
    end

    test "all required tokens are required" do
      assert {:ok, "hello world !"} = HelloWorldRelaxed.parse("hello world !")
      assert {:ok, "hello world ?"} = HelloWorldRelaxed.parse("hello world ?")
      assert {:ok, "hello Dolly ?"} = HelloWorldRelaxed.parse("hello ?")
      assert {:ok, "hello Dolly !"} = HelloWorldRelaxed.parse("hello !")
      assert {:error, {1, 6}, :no_clause_matched} = HelloWorldRelaxed.parse("     ")
      assert {:error, {1, 6}, :no_clause_matched} = HelloWorldRelaxed.parse("hello")
      assert {:error, {1, 12}, :no_clause_matched} = HelloWorldRelaxed.parse("hello world")
      assert {:error, {1, 7}, :no_clause_matched} = HelloWorldRelaxed.parse("hello home")
    end

    test "optional rules are not mandatory" do
      assert {:ok, "hello Dolly !"} = HelloWorldRelaxed.parse("hello !")
      assert {:ok, "hello Dolly ?"} = HelloWorldRelaxed.parse("hello ?")
      assert {:ok, "hello world !"} = HelloWorldRelaxed.parse("hello world !")
      assert {:ok, "hello world ?"} = HelloWorldRelaxed.parse("hello world ?")
    end

    test "optional rules do not consume token when not matched" do
      # "home" is not consumed by the optional rule world, and so appears to be the next token,
      # where the bang rule is expected to match.
      assert {:error, {1, 7}, :no_clause_matched} = HelloWorldRelaxed.parse("hello home !")
    end

    test "spaces and linebreaks do not matter" do
      assert {:ok, "hello world !"} = HelloWorldRelaxed.parse("\n\nhello    \t world\n !\n\n")
      assert {:ok, "hello Dolly ?"} = HelloWorldRelaxed.parse("\n\n\thello\n\n?\n\n")
    end
  end

  describe "with drop_spaces mode off" do
    defmodule HelloWorldRelaxedDropSpacesOff do
      use Grammar, drop_spaces: false

      rule start("hello", :spaces, :world, :bang) do
        [_, _spaces, world, bang] = params
        "hello #{world || "Dolly"} #{bang}"
      end

      rule? world("world"), do: "world"

      rule bang("!"), do: "!"

      rule bang("?"), do: "?"

      rule spaces(~r/[\s]+/), do: nil
    end

    test "all required tokens are required, including spaces" do
      assert {:ok, "hello world !"} = HelloWorldRelaxedDropSpacesOff.parse("hello world!")
      assert {:ok, "hello world ?"} = HelloWorldRelaxedDropSpacesOff.parse("hello world?")
      assert {:ok, "hello Dolly ?"} = HelloWorldRelaxedDropSpacesOff.parse("hello ?")
      assert {:ok, "hello Dolly !"} = HelloWorldRelaxedDropSpacesOff.parse("hello !")
      assert {:error, {1, 1}, :no_clause_matched} = HelloWorldRelaxedDropSpacesOff.parse("     ")
      assert {:error, {1, 6}, :no_clause_matched} = HelloWorldRelaxedDropSpacesOff.parse("hello")
      assert {:error, {1, 12}, :no_clause_matched} = HelloWorldRelaxedDropSpacesOff.parse("hello world")
      assert {:error, {1, 7}, :no_clause_matched} = HelloWorldRelaxedDropSpacesOff.parse("hello home")
    end

    test "handle spaces and linebreaks as described" do
      assert {:ok, "hello Dolly ?"} = HelloWorldRelaxedDropSpacesOff.parse("hello\n\n?")
      assert {:ok, "hello world ?"} = HelloWorldRelaxedDropSpacesOff.parse("hello\t\t world?")
    end

    test "unhandled spaces and linebreaks are forbidden" do
      assert {:error, {1, 1}, :no_clause_matched} = HelloWorldRelaxedDropSpacesOff.parse("\n\nhello    \t world\n !\n\n")
      assert {:error, {1, 14}, :no_clause_matched} = HelloWorldRelaxedDropSpacesOff.parse("hello\t\t world  ?")
    end
  end
end
