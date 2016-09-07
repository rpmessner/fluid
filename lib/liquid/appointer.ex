defmodule Liquid.Appointer do
  @moduledoc "A module to assign context for `Liquid.Variable`"
  alias Liquid.{Variable, Context}

  defp literals, do: %{"nil" => nil, "null" => nil, "" => nil,
                      "true" => true, "false" => false,
                      "blank" => :blank?, "empty" => :empty?}

  def integer, do: ~r/^(-?\d+)$/
  def float, do: ~r/^(-?\d[\d\.]+)$/
  def quoted_string, do: ~r/#{Liquid.quoted_string}/


  @doc "Assigns context for Variable and filters"
  def assign(%Variable{literal: literal, parts: [],filters: filters}, context) do
    { literal, filters |> assign_context(context.assigns), context }
  end

  def assign(%Variable{literal: nil, parts: parts, filters: filters}, context) do
    {resolve(parts, context), filters |> assign_context(context.assigns), context}
  end

  @doc """
  Makes Variable.parts or literals from the given markup
  """
  @spec parse_name(String.t) :: String.t
  def parse_name(name) do
    value = cond do
      literals      |> Map.has_key?(name) ->
        literals |> Map.get(name)
      integer       |> Regex.match?(name) ->
        name |> String.to_integer
      float         |> Regex.match?(name) ->
        name |> String.to_float
      quoted_string |> Regex.match?(name) ->
        Liquid.quote_matcher |> Regex.replace(name, "")
      true ->
        name = name |> String.split(" ", parts: 2) |> hd
        Regex.scan(Liquid.variable_parser, name) |> List.flatten
    end
    if is_list(value), do: %{parts: value}, else: %{literal: value }
  end


  defp resolve([<<key::binary>>|_]=parts, %Context{}=current) do
    cond do
      !(is_nil(Map.get(current.assigns, key |> String.to_atom))) ->
        resolve(parts, current.assigns)
      !(is_nil(Map.get(current.presets, key |> String.to_atom))) ->
        resolve(parts, current.presets)
      current.assigns |> Map.has_key?(key) ->
        resolve(parts, current.assigns)
      current.presets |> Map.has_key?(key) ->
        resolve(parts, current.presets)
      true -> nil
    end
  end

  defp resolve([], current), do: current

  defp resolve([<<?[,index::binary>>|parts], current) do
    index = String.split(index, "]") |> hd |> String.to_integer
    resolve(parts, current |> Enum.fetch!(index))
  end

  defp resolve(["size"|_], current) when is_list(current), do: current |> Enum.count

  defp resolve(["size"|_], current) when is_map(current), do: current |> map_size

  defp resolve([name|parts], current) when is_binary(name) do
    current = resolve(name, current)
    resolve(parts, current)
  end

  defp resolve(key, current) when is_map(current) and is_binary(key) do
    key = if Map.has_key?(current, :__struct__), do: key |> String.to_atom, else: key
    Map.get(current, key)
  end

  defp resolve(key, _current) when is_binary(key), do: nil # !is_list(current)


  defp assign_context(filters, assigns) when assigns == %{}, do: filters

  defp assign_context([], _), do: []

  defp assign_context([head|tail], assigns) do
    [name, args] = head
    args = for arg <- args do
      parsed = arg |> parse_name
      if !(parsed |> Map.has_key?(:parts) and length(parsed.parts) > 1) do
        if Map.has_key?(assigns, :__struct__) do
          key = arg |> String.to_atom
          if assigns |> Map.has_key?(key), do: "#{Map.get(assigns,key)}", else: arg
        else
          if assigns |> Map.has_key?(arg), do: "#{assigns[arg]}", else: arg
        end
      else
        resolve(parsed.parts, %Context{assigns: assigns}) |> to_string
      end

    end

    [[name, args] | assign_context(tail,assigns)]
  end

end
