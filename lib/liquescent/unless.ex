defmodule Liquescent.Unless do
  alias Liquescent.IfElse
  alias Liquescent.Blocks
  alias Liquescent.Template
  alias Liquescent.Condition
  alias Liquescent.Tags
  alias Liquescent.Render

  def parse(%Blocks{}=block, %Template{}=t) do
    IfElse.parse(block, t)
  end

  def render(output, %Tags{}, context) do
    { output, context }
  end

  def render(output, %Blocks{condition: condition, nodelist: nodelist, elselist: elselist}, context) do
    condition = Condition.evaluate(condition, context)
    conditionlist = unless condition, do: nodelist, else: elselist
    Render.render(output, conditionlist, context)
  end
end
