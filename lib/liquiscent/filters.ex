defmodule Liquescent.Filters do

  defmodule Functions do
    def split(<<string::binary>>, <<separator::binary>>) do
      String.split(string, separator)
    end

    def capitalize(<<string::binary>>) do
      String.capitalize(string)
    end
  end

  def parse(<<markup::binary>>) do
    [name|filters] = Regex.scan(Liquescent.filter_parser, markup)
      |> List.flatten
      |> Enum.filter(&(&1 != "|"))
      |> Enum.map(&String.strip/1)
    filters = Enum.map(filters, fn(markup) ->
      [[_, filter]|_] = Regex.scan(~r/\s*(\w+)/, markup)
      args = Liquescent.filter_arguments
        |> Regex.scan(markup)
        |> List.flatten
        |> Liquescent.List.even_elements

      [String.to_atom(filter), args]
    end)
    [name|filters]
  end

  def filter([], value), do: value
  def filter([filter|rest], value) do
    [name, args] = filter
    args = Enum.map(args, fn(arg) ->
      Regex.replace(Liquescent.quote_matcher, arg, "")
    end)
    ret  = apply(Functions, name, [value|args])
    filter(rest, ret)
  end

end
