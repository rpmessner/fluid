defprotocol Blank do
  @doc "Returns true when blank"
  def blank?(data)
end

defimpl Blank, for: List do
  alias Liquid.Block
  alias Liquid.Tag

  def blank?([]), do: true
  def blank?(list) do
    !(list
      |> Enum.map(fn
          (x) when is_binary(x) ->
            cond do
              Regex.match?(~r/\A\s*\z/, x) -> true
              true -> false
            end
          (%Block{blank: true}) -> true
          (%Tag{blank: true}) -> true
          (_) -> false
        end)
      |> Enum.uniq
      |> Enum.any?(fn(x) -> x == false end))
  end
end
