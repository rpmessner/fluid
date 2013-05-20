defmodule Fluid.Variables do
  alias Fluid.Filters, as: Filters

  defp literals, do: [nil: nil, null: nil, "": nil,
                      true: true, false: false,
                      blank: :blank?, empty: :empty?]

  def integer, do: %r/^(-?\d+)$/
  def float, do: %r/^(-?\d[\d\.]+)$/
  def quoted_string, do: %r/#{Fluid.quoted_string}/

  @doc """
    matches for [] access
  """
  def create(<<markup::binary>>) do
    [name|filters] = Filters.parse(markup)
    key = name |> String.strip |> binary_to_atom(:utf8)
    variable = Fluid.Variable[name: name, filters: filters]
    cond do
      literals      |> Dict.has_key?(key) -> literals |> Dict.get(key) |> variable.literal
      integer       |> Regex.match?(name) -> name |> binary_to_integer |> variable.literal
      float         |> Regex.match?(name) -> name |> binary_to_float |> variable.literal
      quoted_string |> Regex.match?(name) -> Fluid.quote_matcher |> Regex.replace(name, "") |> variable.literal
      true ->
        [name|_] = String.split(name, " ")
        parts = Regex.scan(Fluid.variable_parser, name)
        variable.parts(parts)
    end
  end

  def lookup(Fluid.Variable[name: name, filters: filters]=v, assigns, presets) do
    { ret, assigns } = case v do
      Fluid.Variable[literal: literal, parts: []] ->
        { literal, assigns }
      Fluid.Variable[literal: nil, parts: parts] ->
        resolve(parts, assigns, assigns, presets)
    end
    ret = Filters.filter(filters, ret)
    { ret, assigns }
  end

  defp resolve([<<?[,rest::binary>>|parts], current, assigns, presets) when is_list(assigns) do
    [index, _] = String.split(rest, "]")
    index = binary_to_integer(index)
    resolve(parts, current |> Enum.at!(index), assigns, presets)
  end

  defp resolve([<<name::binary>>|parts], current, assigns, presets) do
    { into, assigns } = resolve(name, current, assigns, presets)
    ret = resolve(parts, into, assigns, presets)
    ret
  end

  defp resolve([], current, assigns, presets) do
    { current, assigns }
  end

  defp resolve(<<name::binary>>, current, assigns, presets) when !is_list(current), do: { nil, assigns }
  defp resolve(<<name::binary>>, current, assigns, presets) when is_list(current) do
    key = binary_to_atom(name, :utf8)
    assign = Dict.get(current, key)
    preset = Dict.get(presets, key)
    cond do
      is_function(assign) ->
        assign = assign.()
        { assign, Dict.put(assigns, key, assign) }
      true -> { assign || preset, assigns }
    end
  end
end