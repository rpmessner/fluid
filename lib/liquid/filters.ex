defmodule Liquid.Filters do

  defmodule Functions do
    def size(input) when is_binary(input) do
      String.length(input)
    end

    def size(input) when is_list(input) do
      length(input)
    end

    def size(input) when is_tuple(input) do
      tuple_size(input)
    end

    def size(input), do: 0

    def downcase(input) do
      input |> to_string |> String.downcase
    end

    def upcase(input) do
      input |> to_string |> String.upcase
    end

    def capitalize(input) do
      input |> to_string |> String.capitalize
    end

    

    def split(<<string::binary>>, <<separator::binary>>) do
      String.split(string, separator)
    end


  end

  def parse(<<markup::binary>>) do
    [name|filters] = Regex.scan(Liquid.filter_parser, markup)
      |> List.flatten
      |> Enum.filter(&(&1 != "|"))
      |> Enum.map(&String.strip/1)
    filters = Enum.map(filters, fn(markup) ->
      [[_, filter]|_] = Regex.scan(~r/\s*(\w+)/, markup)
      args = Liquid.filter_arguments
        |> Regex.scan(markup)
        |> List.flatten
        |> Liquid.List.even_elements

      [String.to_atom(filter), args]
    end)
    [name|filters]
  end

  def filter([], value), do: value
  def filter([filter|rest], value) do
    [name, args] = filter
    args = Enum.map(args, fn(arg) ->
      Regex.replace(Liquid.quote_matcher, arg, "")
    end)
    ret  = apply(Functions, name, [value|args])
    filter(rest, ret)
  end

end
