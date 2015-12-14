defmodule Liquescent.ElseIf do
  def parse(%Liquescent.Tags{}=tag, %Liquescent.Template{}=t), do: { tag, t }
  def render(_, _, _, _), do: raise "should never get here"
end

defmodule Liquescent.Else do
  def parse(%Liquescent.Tags{}=tag, %Liquescent.Template{}=t), do: { tag, t }
  def render(_, _, _, _), do: raise "should never get here"
end

defmodule Liquescent.IfElse do
  alias Liquescent.Condition, as: Condition
  alias Liquescent.Render, as: Render
  alias Liquescent.Blocks, as: Blocks

  def syntax, do: ~r/(#{Liquescent.quoted_fragment})\s*([=!<>a-z_]+)?\s*(#{Liquescent.quoted_fragment})?/
  def expressions_and_operators do
    ~r/(?:\b(?:\s?and\s?|\s?or\s?)\b|(?:\s*(?!\b(?:\s?and\s?|\s?or\s?)\b)(?:#{Liquescent.quoted_fragment}|\S+)\s*)+)/
  end

  def parse(%Liquescent.Blocks{}=block, %Liquescent.Template{}=t) do
    block = parse_conditions(block)
    case Blocks.split(block, [:else, :elsif]) do
      { true_block, [%Liquescent.Tags{name: :elsif, markup: markup}|elsif_block] } ->
        { elseif, t } = %Liquescent.Blocks{name: :if, markup: markup, nodelist: elsif_block} |> parse(t)
        { %{block | nodelist: true_block, elselist: [elseif] }, t }
      { true_block, [%Liquescent.Tags{name: :else}|false_block] } ->
        { %{block | nodelist: true_block, elselist: false_block}, t }
      { _, [] } ->
        { block, t }
    end
  end

  def render(output, %Liquescent.Tags{}, context) do
    { output, context }
  end

  def render(output, %Liquescent.Blocks{condition: condition, nodelist: nodelist, elselist: elselist}, context) do
    condition = Condition.evaluate(condition, context)
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

  defp parse_conditions(%Liquescent.Blocks{markup: markup}=block) do
    expressions = Regex.scan(expressions_and_operators, markup)
    expressions = expressions |> split_conditions |> Enum.reverse
    condition   = Condition.create(expressions)
    %{block | condition: condition }
  end
end
