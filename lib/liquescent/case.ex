defmodule Liquescent.Case do

  alias Liquescent.Tags
  alias Liquescent.Blocks
  alias Liquescent.Templates
  alias Liquescent.Variables
  alias Liquescent.Condition

  def syntax, do: ~r/(#{Liquescent.quoted_fragment})/
  def when_syntax, do: ~r/(#{Liquescent.quoted_fragment})(?:(?:\s+or\s+|\s*\,\s*)(#{Liquescent.quoted_fragment}.*))?/

  def parse(%Blocks{markup: markup}=b, %Templates{}=t) do
    [[_, name]] = syntax |> Regex.scan(markup)
    { split(name |> Variables.create, b.nodelist), t }
  end

  defp split(%Variables{}, []), do: []
  defp split(%Variables{}=v, [<<_::binary>>|t]), do: split(v, t)
  defp split(%Variables{}=_, [%Liquescent.Tags{name: :else}|t]), do: t
  defp split(%Variables{}=v, [%Liquescent.Tags{name: :when, markup: markup}|t]) do
    { nodelist, t } = Blocks.split(t, [:when, :else])
    condition = parse_condition(v, markup)
    %Blocks{name: :if, nodelist: nodelist, condition: condition, elselist: split(v, t)}
  end

  defp parse_condition(%Variables{}=v, <<markup::binary>>) do
    { h, t } = parse_when(markup)

    parse_condition(v, Condition.create({v, "==", h}), t)
  end

  defp parse_condition(%Variables{}=_, %Condition{}=condition, []), do: condition
  defp parse_condition(%Variables{}=v, %Condition{}=condition, [<<markup::binary>>]) do
    { h, t } = parse_when(markup)
    parse_condition(v, Condition.join(:or, condition, {v, "==", h}), t)
  end

  defp parse_when(markup) do
    [[_,h|t]|m] = when_syntax |> Regex.scan(markup)
    m = m |> List.flatten |> Liquescent.List.even_elements
    t = [t|m]  |> Enum.join(" ")
    t = if t == "", do: [], else: [t]
    { h, t }
  end
end

defmodule Liquescent.When do
  alias Liquescent.Tags, as: Tags
  alias Liquescent.Templates, as: Templates

  def parse(%Tags{}=tag, %Templates{}=t), do: { tag, t }
end
