defmodule Fluid.Filters do
  defmodule Functions do
    def split(<<string::binary>>, <<separator::binary>>) do
      String.split(string, separator)
    end

    def capitalize(<<string::binary>>) do
      String.capitalize(string)
    end
  end

  alias Fluid.Variables, as: Variables

  def parse(<<markup::binary>>) do
    filters = Regex.scan(Fluid.filter_parser, markup)
    filters = Enum.filter(filters, fn(x) -> x != "|" end)
    [name|filters] = Enum.map(filters, function(String, :strip, 1))
    filters = Enum.map(filters, fn(markup) ->
      [[filter]|rest] = Regex.scan(%r/\s*(\w+)/, markup)
      args = Fluid.filter_arguments |> Regex.scan(markup) |> List.flatten
      [binary_to_atom(filter, :utf8), args]
    end)
    [name|filters]
  end

  def filter([], value), do: value
  def filter([filter|rest], value) do
    [name, args] = filter
    args = Enum.map(args, fn(arg) ->
      Regex.replace(Fluid.quote_matcher, arg, "")
    end)
    ret  = apply(Functions, name, [value|args])
    filter(rest, ret)
  end
end