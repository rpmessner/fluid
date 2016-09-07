defmodule Liquid.Variable do
  @moduledoc """
    Module to create and lookup for Variables

  """
  defstruct name: nil, literal: nil, filters: [], parts: []
  alias Liquid.{Filters, Variable, Context}

  defp literals, do: %{"nil" => nil, "null" => nil, "" => nil,
                      "true" => true, "false" => false,
                      "blank" => :blank?, "empty" => :empty?}

  def integer, do: ~r/^(-?\d+)$/
  def float, do: ~r/^(-?\d[\d\.]+)$/
  def quoted_string, do: ~r/#{Liquid.quoted_string}/

  @doc """
    resolves data from `Liquid.Variable.parse/1` and creates a variable struct
  """
  def create(markup) when is_binary(markup) do
    [name|filters] = markup |> parse
    name = name |> String.trim
    variable = %Liquid.Variable{name: name, filters: filters}
    cond do
      literals      |> Map.has_key?(name) ->
        value = literals |> Map.get(name)
        %{variable | literal: value }
      integer       |> Regex.match?(name) ->
        value = name |> String.to_integer
        %{variable | literal: value }
      float         |> Regex.match?(name) ->
        value = name |> String.to_float
        %{variable | literal: value }
      quoted_string |> Regex.match?(name) ->
        unquoted_name = Liquid.quote_matcher |> Regex.replace(name, "")
        %{ variable | literal: unquoted_name }
      true ->
        name = name |> String.split(" ", parts: 2) |> hd
        parts = Regex.scan(Liquid.variable_parser, name) |> List.flatten
        %{variable | parts: parts}
    end
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
