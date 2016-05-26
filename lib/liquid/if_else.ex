defmodule Liquid.ElseIf do
  def parse(%Liquid.Tag{}=tag, %Liquid.Template{}=t), do: { tag, t }
  def render(_, _, _, _), do: raise "should never get here"
end

defmodule Liquid.Else do
  def parse(%Liquid.Tag{}=tag, %Liquid.Template{}=t), do: { tag, t }
  def render(_, _, _, _), do: raise "should never get here"
end

defmodule Liquid.IfElse do
  alias Liquid.Condition
  alias Liquid.Render
  alias Liquid.Block
  alias Liquid.Tag
  alias Liquid.Template

  def syntax, do: ~r/(#{Liquid.quoted_fragment})\s*([=!<>a-z_]+)?\s*(#{Liquid.quoted_fragment})?/
  def expressions_and_operators do
    ~r/(?:\b(?:\s?and\s?|\s?or\s?)\b|(?:\s*(?!\b(?:\s?and\s?|\s?or\s?)\b)(?:#{Liquid.quoted_fragment}|\S+)\s*)+)/
  end

  def parse(%Block{}=block, %Template{}=t) do
    block = parse_conditions(block)
    case Block.split(block, [:else, :elsif]) do
      { true_block, [%Tag{name: :elsif, markup: markup}|elsif_block] } ->
        is_blank = Blank.blank?(elsif_block)
        { elseif, t } = %Block{name: :if, markup: markup, nodelist: elsif_block, blank: is_blank} |> parse(t)
        is_blank = Blank.blank?(true_block)
        { %{block | nodelist: true_block, elselist: [elseif], blank: is_blank }, t }
      { true_block, [%Tag{name: :else}|false_block] } ->
        is_blank = Blank.blank?(true_block) && Blank.blank?(false_block)
        { %{block | nodelist: true_block, elselist: false_block, blank: is_blank}, t }
      { _, [] } ->
        is_blank = Blank.blank?(block.nodelist)
        { %{block | blank: is_blank}, t }
    end
  end

  def render(output, %Tag{}, context) do
    { output, context }
  end

  def render(output, %Block{blank: true} = block, context) do
    {_, context} = render(output, %{block | blank: false }, context)
    {output, context}
  end

  def render(output, %Block{condition: condition, nodelist: nodelist, elselist: elselist, blank: false}, context) do
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

  defp parse_conditions(%Block{markup: markup}=block) do
    expressions = Regex.scan(expressions_and_operators, markup)
    expressions = expressions |> split_conditions |> Enum.reverse
    condition   = Condition.create(expressions)
    %{block | condition: condition }
  end
end
