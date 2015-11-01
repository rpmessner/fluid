defmodule Fluid.Case do
  require IEx
  alias Fluid.Tags, as: Tags
  alias Fluid.Blocks, as: Blocks
  alias Fluid.Contexts, as: Contexts
  alias Fluid.Templates, as: Templates
  alias Fluid.Variables, as: Variables
  alias Fluid.Conditions, as: Conditions

  def syntax, do: ~r/(#{Fluid.quoted_fragment})/
  def when_syntax, do: ~r/(#{Fluid.quoted_fragment})(?:(?:\s+or\s+|\s*\,\s*)(#{Fluid.quoted_fragment}.*))?/

  def parse(%Blocks{markup: markup}=b, %Templates{}=t) do
    [[_, name]] = syntax |> Regex.scan(markup)
    { split(name |> Variables.create, b.nodelist), t }
  end

  defp split(%Variables{}, []), do: []
  defp split(%Variables{}=v, [<<_::binary>>|t]), do: split(v, t)
  defp split(%Variables{}=_, [%Fluid.Tags{name: :else}|t]), do: t
  defp split(%Variables{}=v, [%Fluid.Tags{name: :when, markup: markup}|t]) do
    { nodelist, t } = Blocks.split(t, [:when, :else])
    condition = parse_condition(v, markup)
    %Blocks{name: :if, nodelist: nodelist, condition: condition, elselist: split(v, t)}
  end

  defp parse_condition(%Variables{}=v, <<markup::binary>>) do
    { h, t } = parse_when(markup)

    parse_condition(v, Conditions.create({v, "==", h}), t)
  end

  defp parse_condition(%Variables{}=_, %Conditions{}=condition, []), do: condition
  defp parse_condition(%Variables{}=v, %Conditions{}=condition, [<<markup::binary>>]) do
    { h, t } = parse_when(markup)
    parse_condition(v, Conditions.join(:or, condition, {v, "==", h}), t)
  end

  defp parse_when(markup) do
    [[_,h|t]|m] = when_syntax |> Regex.scan(markup)
    m = m |> List.flatten |> Fluid.List.even_elements
    t = [t|m]  |> Enum.join(" ")
    t = if t == "", do: [], else: [t]
    { h, t }
  end
end

defmodule Fluid.When do
  alias Fluid.Tags, as: Tags
  alias Fluid.Templates, as: Templates

  def parse(%Tags{}=tag, %Templates{}=t), do: { tag, t }
end
