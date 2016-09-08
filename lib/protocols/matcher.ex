defprotocol Matcher do
  @fallback_to_any true
  @doc "Assigns context to values"
  def match(_,_)
end

defimpl Matcher, for: Liquid.Context do
  @doc """
  `Matcher` protocol implementation for `Liquid.Context`
  """

  def match(current, []), do: current

  def match(current, [key|_]=parts) when is_binary(key) do
    current = cond do
      current.assigns |> Map.has_key?(key) -> current.assigns
      current.presets |> Map.has_key?(key) -> current.presets
      !(is_nil(Map.get(current.assigns, key |> String.to_atom))) -> current.assigns
      !(is_nil(Map.get(current.presets, key |> String.to_atom))) -> current.presets
      true -> nil
    end
    Matcher.match(current, parts)
  end
end

defimpl Matcher, for: Map do

  def match(current, []), do: current

  def match(current, ["size"|_]), do: current |> map_size

  def match(current, [name|parts]) when is_binary(name) do
    current |> Matcher.match(name) |> Matcher.match(parts)
  end

  def match(current, key) when is_binary(key), do: current[key]
end


defimpl Matcher, for: List do

  def match(current, []), do: current

  def match(current, ["size"|_]), do: current |> Enum.count

  def match(current, [<<?[,index::binary>>|parts]) do
    index = index |> String.split("]") |> hd |> String.to_integer
    current |> Enum.fetch!(index) |> Matcher.match(parts)
  end

end

defimpl Matcher, for: Any do

  def match(nil,_), do: nil

  def match(current, []), do: current

  def match(true, _), do: nil


  @doc """
  Match functions for structs:
  """
  def match(current, [name|parts]) when is_map(current) and is_binary(name) do
    current |> Matcher.match(name) |> Matcher.match(parts)
  end

  def match(current, key) when is_map(current) and is_binary(key) do
    key = if Map.has_key?(current, :__struct__), do: key |> String.to_atom, else: key
    current |> Map.get(key)
  end

  @doc """
  Matches all remaining cases
  """
  def match(_current, key) when is_binary(key), do: nil # !is_list(current)
end
