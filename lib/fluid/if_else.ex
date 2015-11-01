defmodule Fluid.ElseIf do
  def parse(%Fluid.Tags{}=tag, %Fluid.Templates{}=t), do: { tag, t }
  def render(_, _, _, _), do: raise "should never get here"
end

defmodule Fluid.Else do
  def parse(%Fluid.Tags{}=tag, %Fluid.Templates{}=t), do: { tag, t }
  def render(_, _, _, _), do: raise "should never get here"
end

defmodule Fluid.IfElse do
  alias Fluid.Conditions, as: Conditions
  alias Fluid.Render, as: Render
  alias Fluid.Blocks, as: Blocks

  def syntax, do: ~r/(#{Fluid.quoted_fragment})\s*([=!<>a-z_]+)?\s*(#{Fluid.quoted_fragment})?/
  def expressions_and_operators do
    ~r/(?:\b(?:\s?and\s?|\s?or\s?)\b|(?:\s*(?!\b(?:\s?and\s?|\s?or\s?)\b)(?:#{Fluid.quoted_fragment}|\S+)\s*)+)/
  end

  def parse(%Fluid.Blocks{}=block, %Fluid.Templates{}=t) do
    block = parse_conditions(block)
    case Blocks.split(block, [:else, :elsif]) do
      { true_block, [%Fluid.Tags{name: :elsif, markup: markup}|elsif_block] } ->
        { elseif, t } = %Fluid.Blocks{name: :if, markup: markup, nodelist: elsif_block} |> parse(t)
        { block.nodelist(true_block).elselist([elseif]), t }
      { true_block, [%Fluid.Tags{name: :else}|false_block] } ->
        { block.nodelist(true_block).elselist(false_block), t }
      { _, [] } ->
        { block, t }
    end
  end

  def render(output, %Fluid.Tags{}, context) do
    { output, context }
  end

  def render(output, %Fluid.Blocks{condition: condition, nodelist: nodelist, elselist: elselist}, context) do
    condition = Conditions.evaluate(condition, context)
    conditionlist = if condition, do: nodelist, else: elselist
    Render.render(output, conditionlist, context)
  end

  defp split_conditions(expressions) do
    expressions |> List.flatten |> Enum.map(&String.strip/1) |> Enum.map(fn(x) ->
      case syntax |> Regex.scan(x) do
        [[_, left, operator, right]] -> { left, operator, right }
        [[_, x]] -> x
      end
    end)
  end

  defp parse_conditions(%Fluid.Blocks{markup: markup}=block) do
    expressions = Regex.scan(expressions_and_operators, markup)
    expressions = expressions |> split_conditions |> Enum.reverse
    condition   = Conditions.create(expressions)
    %{block | condition: condition }
  end
end
