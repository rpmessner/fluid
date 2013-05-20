# defmodule Fluid.ElseIf do
#   def render(_, _, _, _), do: raise "should never get here"
# end

# defmodule Fluid.Else do
#   def render(_, _, _, _), do: raise "should never get here"
# end

defmodule Fluid.IfElse do
  alias Fluid.Conditions, as: Condition
  alias Fluid.Render, as: Render

  def syntax, do: %r/(#{Fluid.quoted_fragment})\s*([=!<>a-z_]+)?\s*(#{Fluid.quoted_fragment})?/
  def expressions_and_operators do
    %r/(?:\b(?:\s?and\s?|\s?or\s?)\b|(?:\s*(?!\b(?:\s?and\s?|\s?or\s?)\b)(?:#{Fluid.quoted_fragment}|\S+)\s*)+)/
  end

  def render(output, Fluid.Tag[]=tag, assigns, presets) do
    { output, assigns }
  end

  defp split_conditions(expressions) do
    expressions |> Enum.map(function(String, :strip, 1)) |> Enum.map(fn(x) ->
      case syntax |> Regex.scan(x) do
        [[left, operator, right]] -> { left, operator, right }
        [[x]] -> x
      end
    end)
  end

  defp parse_conditions(Fluid.Block[markup: markup]=block) do
    expressions = Regex.scan(expressions_and_operators, markup)
    expressions = expressions |> split_conditions |> Enum.reverse
    condition   = Condition.create(expressions)
    block       = block.condition(condition)
  end

  def parse(Fluid.Block[nodelist: nodelist]=block, presets) do
    block  = parse_conditions(block)
    blocks = Enum.split_while(nodelist, fn(x) ->
      !(is_record(x, Fluid.Tag) and x.name in [:else, :elsif])
    end)
    case blocks do
      { true_block, [Fluid.Tag[name: :elsif, markup: markup]|elsif_block] } ->
        { elseif, presets } = Fluid.Block[name: :if, markup: markup, nodelist: elsif_block] |> parse(presets)
        { block.nodelist(true_block).elselist([elseif]), presets }
      { true_block, [Fluid.Tag[name: :else]|false_block] } ->
        { block.nodelist(true_block).elselist(false_block), presets }
      { _, [] } ->
        { block, presets }
    end
  end

  def render(output, Fluid.Block[condition: condition, nodelist: nodelist, elselist: elselist]=block, assigns, presets) do
    evaled = Condition.evaluate(condition, assigns, presets)
    conditionlist = if evaled, do: nodelist, else: elselist
    Render.render(output, conditionlist, assigns, presets)
  end
end