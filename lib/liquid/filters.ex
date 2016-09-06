defmodule Liquid.Filters do

  defmodule Functions do
    def split(<<string::binary>>, <<separator::binary>>) do
      String.split(string, separator)
    end

    def capitalize(<<string::binary>>) do
      String.capitalize(string)
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

  @doc """
  Recursively pass through all of the input filters applying them
  """
  def filter([], value), do: value
  def filter([filter|rest], value) do
    [name, args] = filter
    args = for arg <- args do
      Liquid.quote_matcher |> Regex.replace(arg, "")
    end

    functions = Functions.__info__(:functions)
    custom_filters = Application.get_env(:liquid, :custom_filters)

    ret = case {name, functions[name], custom_filters[name]} do
      # pass value in case of no filters
      {nil, _, _} -> value
      # pass non-existend filter
      {_, nil, nil} -> value
      # Fallback to custom if no standard
      {_, nil, _} -> apply_function custom_filters[name], name, [value|args]
      _ -> apply_function Functions, name, [value|args]
    end
    filter(rest, ret)
  end


  @doc """
  Add filter modules mentioned in extra_filters_module env variable
  """
  def add_filter_modules do
    for filter_module <- Application.get_env(:liquid, :extra_filters_module) || [] do
      filter_module |> add_filters
    end
  end

  @doc """
  Fetches the current custom filters and extends with the functions from passed module
  NB: you can't override the standard filters though
  """
  def add_filters(module) do
    custom_filters = Application.get_env(:liquid, :custom_filters) || %{}

    module_functions = module.__info__(:functions)
      |> Enum.into(%{}, fn {key,_} -> {key, module} end)

    custom_filters = module_functions |> Map.merge(custom_filters)
    Application.put_env(:liquid, :custom_filters, custom_filters)
  end

  defp apply_function(module, name, args) do
    try do
      apply module, name, args
    rescue
      e in UndefinedFunctionError ->
        functions = module.__info__(:functions)
        raise ArgumentError, message: "Liquid error: wrong number of arguments (#{e.arity} for #{functions[name]})"
    end
  end


end
