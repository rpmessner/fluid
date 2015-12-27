defmodule Liquescent.Unless do
  alias Liquescent.IfElse
  alias Liquescent.Block
  alias Liquescent.Template
  alias Liquescent.Condition
  alias Liquescent.Tags
  alias Liquescent.Render

  def parse(%Block{}=block, %Template{}=t) do
    IfElse.parse(block, t)
  end

  def render(output, %Tags{}, context) do
    { output, context }
  end

  def render(output, %Block{condition: condition, nodelist: nodelist, elselist: elselist}, context) do
    condition = Condition.evaluate(condition, context)
    conditionlist = if condition, do: elselist, else: nodelist
    Render.render(output, conditionlist, context)
  end
end
