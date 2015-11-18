defmodule Fluid.Unless do
  alias Fluid.IfElse
  alias Fluid.Blocks
  alias Fluid.Templates
  alias Fluid.Conditions
  alias Fluid.Tags
  alias Fluid.Render

  def parse(%Blocks{}=block, %Templates{}=t) do
    IfElse.parse(block, t)
  end

  def render(output, %Tags{}, context) do
    { output, context }
  end

  def render(output, %Blocks{condition: condition, nodelist: nodelist, elselist: elselist}, context) do
    condition = Conditions.evaluate(condition, context)
    conditionlist = unless condition, do: nodelist, else: elselist
    Render.render(output, conditionlist, context)
  end
end
