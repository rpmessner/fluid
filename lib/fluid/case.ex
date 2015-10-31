defmodule Fluid.Case do
  alias Fluid.Tag, as: Tag
  alias Fluid.Block, as: Block
  alias Fluid.Blocks, as: Blocks
  alias Fluid.Context, as: Context
  alias Fluid.Template, as: Template
  alias Fluid.Variable, as: Variable
  alias Fluid.Variables, as: Variables
  alias Fluid.Condition, as: Condition
  alias Fluid.Conditions, as: Conditions

  def syntax, do: ~r/(#{Fluid.quoted_fragment})/
  def when_syntax, do: ~r/(#{Fluid.quoted_fragment})(?:(?:\s+or\s+|\s*\,\s*)(#{Fluid.quoted_fragment}.*))?/g

  def parse(%Block{markup: markup}=b, %Template{}=t) do
    [[_, name]] = syntax |> Regex.scan(markup)
    { split(name |> Variables.create, b.nodelist), t }
  end

  defp split(%Variable{}, []), do: []
  defp split(%Variable{}=v, [<<_::binary>>|t]), do: split(v, t)
  defp split(%Variable{}=_, [Fluid.Tag[name: :else]|t]), do: t
  defp split(%Variable{}=v, [Fluid.Tag[name: :when, markup: markup]|t]) do
    { nodelist, t } = Blocks.split(t, [:when, :else])
    condition = parse_condition(v, markup)
    Block[name: :if, nodelist: nodelist, condition: condition, elselist: split(v, t)]
  end

  defp parse_condition(%Variable{}=v, <<markup::binary>>) do
    { h, t } = parse_when(markup)
    parse_condition(v, Conditions.create({v, "==", h}), t)
  end

  defp parse_condition(%Variable{}=_, %Condition{}=condition, []), do: condition
  defp parse_condition(%Variable{}=v, %Condition{}=condition, [<<markup::binary>>]) do
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
  alias Fluid.Tag, as: Tag
  alias Fluid.Context, as: Context
  alias Fluid.Template, as: Template

  def parse(%Tag{}=tag, %Template{}=t), do: { tag, t }
end
