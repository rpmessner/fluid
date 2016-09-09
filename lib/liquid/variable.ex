defmodule Liquid.Variable do
  @moduledoc """
    Module to create and lookup for Variables

  """
  defstruct name: nil, literal: nil, filters: [], parts: []
  alias Liquid.{Filters, Variable, Context}

  @doc """
    resolves data from `Liquid.Variable.parse/1` and creates a variable struct
  """
  def create(markup) when is_binary(markup) do
    [name|filters] = markup |> parse
    name = name |> String.trim
    variable = %Liquid.Variable{name: name, filters: filters}
    parsed = Liquid.Appointer.parse_name(name)
    Map.merge(variable, parsed)
  end

  @doc """
  Assigns context to variable and than applies all filters
  """
  def lookup(%Variable{}=v, %Context{}=context) do
    { ret, filters, context } = Liquid.Appointer.assign(v, context)
    try do
      ret = filters |> Filters.filter(ret)
      { ret, context }
    rescue
      e in UndefinedFunctionError -> { e.message, context}
      e in ArgumentError -> { e.message, context}
      e in ArithmeticError -> { "Liquid error: #{e.message}", context}
    end
  end


  @doc """
  Parses the markup to a list of filters
  """
  def parse(markup) when is_binary(markup) do
    [name|filters] = if markup != "" do
      Liquid.filter_parser
        |> Regex.scan(markup)
        |> List.flatten
        |> Enum.filter(&(&1 != "|"))
        |> Enum.map(&String.strip/1)
      else
        [""]
      end
    filters = for markup <- filters do
      [_, filter] = ~r/\s*(\w+)/ |> Regex.scan(markup) |> hd
      args = Liquid.filter_arguments
        |> Regex.scan(markup)
        |> List.flatten
        |> Liquid.List.even_elements

      [String.to_atom(filter), args]
    end
    [name|filters]
  end

end
