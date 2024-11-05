defmodule Grammar.CSVTest do
  use ExUnit.Case

  defmodule CSVParser do
    use Grammar, drop_spaces: false

    rule start(:csv) do
      [csv] = params
      csv
    end

    rule csv(:line, :csv_cont) do
      [line, cont] = params
      [line | cont || []]
    end

    rule? csv_cont("\n", :line, :csv_cont) do
      [_, line, cont] = params
      [line | cont || []]
    end

    rule line(:cell, :line_cont) do
      [cell, cont] = params
      [cell | cont || []]
    end

    rule? line_cont(",", :cell, :line_cont) do
      [_, cell, cont] = params
      [cell | cont || []]
    end

    rule cell(~r/[^,\n]+/) do
      [cell] = params
      cell
    end
  end

  test "csv inputs are parsed and rewriten" do
    assert {:ok, [["a", "b", "c"], ["      d", "e", "f"], ["      g", "  h", " i"]]} =
             CSVParser.parse("a,b,c
      d,e,f
      g,  h, i")

    assert {:ok, [["\"a\"", "b", "c"], ["      d", "e", "f"], ["      g", "  h", " i"]]} =
             CSVParser.parse("\"a\",b,c
      d,e,f
      g,  h, i")
  end
end
