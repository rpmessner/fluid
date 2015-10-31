defmodule Fluid.Variables do
  defstruct name: nil, literal: nil, filters: [], parts: []

  alias Fluid.Filters, as: Filters
  alias Fluid.Variable, as: Variable
  alias Fluid.Variables, as: Variables
  alias Fluid.Context, as: Context

  defp literals, do: [nil: nil, null: nil, "": nil,
                      true: true, false: false,
                      blank: :blank?, empty: :empty?]

  def integer, do: ~r/^(-?\d+)$/
  def float, do: ~r/^(-?\d[\d\.]+)$/
  def quoted_string, do: ~r/#{Fluid.quoted_string}/

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
        parts = Regex.scan(Fluid.variable_parser, name) |> List.flatten
        variable.parts(parts)
    end
  end

  def lookup(%Variable{filters: filters}=v, %Context{}=context) do
    { ret, context } = case v do
      Variable[literal: literal, parts: []] ->
        { literal, context }
      Variable[literal: nil, parts: parts] ->
        resolve(parts, context, context)
    end
    ret = Filters.filter(filters, ret)
    { ret, context }
  end

  defp resolve([<<name::binary>>|_]=parts, %Context{}=current, %Context{}=context) do
    key = name |> binary_to_atom(:utf8)
    cond do
      current.assigns |> Dict.has_key?(key) ->
        resolve(parts, current.assigns, context)
      current.presets |> Dict.has_key?(key) ->
        resolve(parts, current.presets, context)
      true -> { nil, context }
    end
  end

  defp resolve([], current, %Context{}=context), do: { current, context }
  defp resolve([<<?[,index::binary>>|parts], current, %Context{}=context) do
    [index, _] = String.split(index, "]")
    index = binary_to_integer(index)
    resolve(parts, current |> Enum.fetch!(index), context)
  end

  defp resolve(["size"|_], current, %Context{}=context) when is_list(current) do
    { current |> Enum.count, context }
  end

  defp resolve([<<name::binary>>|parts], current, %Context{}=context) do
    { current, context } = resolve(name, current, context)
    resolve(parts, current, context)
  end

  defp resolve(<<_::binary>>, current, %Context{}=context) when !is_list(current), do: { nil, context }
  defp resolve(<<name::binary>>, current, %Context{}=context) when is_list(current) do
    key    = binary_to_atom(name, :utf8)
    return = Dict.get(current, key)
    cond do
      is_function(return) ->
        return = return.()
        { return, context.assigns |> Dict.put(key, return) |> context.assigns }
      true -> { return, context }
    end
  end
end
