defmodule Fluid.Variables do
  defstruct name: nil, literal: nil, filters: [], parts: []

  alias Fluid.Filters, as: Filters
  alias Fluid.Variables, as: Variables
  alias Fluid.Variables, as: Variables
  alias Fluid.Contexts, as: Contexts

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
    key = name |> String.strip |> String.to_atom
    variable = Fluid.Variables[name: name, filters: filters]
    cond do
      literals      |> Dict.has_key?(key) -> literals |> Dict.get(key) |> variable.literal
      integer       |> Regex.match?(name) -> name |> String.to_integer  |> variable.literal
      float         |> Regex.match?(name) -> name |> String.to_float |> variable.literal
      quoted_string |> Regex.match?(name) -> Fluid.quote_matcher |> Regex.replace(name, "") |> variable.literal
      true ->
        [name|_] = String.split(name, " ")
        parts = Regex.scan(Fluid.variable_parser, name) |> List.flatten
        variable.parts(parts)
    end
  end

  def lookup(%Variables{filters: filters}=v, %Contexts{}=context) do
    { ret, context } = case v do
      %Variables{literal: literal, parts: []} ->
        { literal, context }
      %Variables{literal: nil, parts: parts} ->
        resolve(parts, context, context)
    end
    ret = Filters.filter(filters, ret)
    { ret, context }
  end

  defp resolve([<<name::binary>>|_]=parts, %Contexts{}=current, %Contexts{}=context) do
    key = name |> String.to_atom
    cond do
      current.assigns |> Dict.has_key?(key) ->
        resolve(parts, current.assigns, context)
      current.presets |> Dict.has_key?(key) ->
        resolve(parts, current.presets, context)
      true -> { nil, context }
    end
  end

  defp resolve([], current, %Contexts{}=context), do: { current, context }
  defp resolve([<<?[,index::binary>>|parts], current, %Contexts{}=context) do
    [index, _] = String.split(index, "]")
    index = String.to_integer(index)
    resolve(parts, current |> Enum.fetch!(index), context)
  end

  defp resolve(["size"|_], current, %Contexts{}=context) when is_list(current) do
    { current |> Enum.count, context }
  end

  defp resolve([<<name::binary>>|parts], current, %Contexts{}=context) do
    { current, context } = resolve(name, current, context)
    resolve(parts, current, context)
  end

  defp resolve(<<_::binary>>, current, %Contexts{}=context) when is_list(current), do: { nil, context } # !is_list(current)
  defp resolve(<<name::binary>>, current, %Contexts{}=context) when is_list(current) do
    key    = String.to_atom(name)
    return = Dict.get(current, key)
    cond do
      is_function(return) ->
        return = return.()
        { return, context.assigns |> Dict.put(key, return) |> context.assigns }
      true -> { return, context }
    end
  end
end
