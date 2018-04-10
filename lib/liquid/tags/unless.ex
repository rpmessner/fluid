defmodule Liquid.Unless do
  @moduledoc """
  Is a conditional just like 'if' but works on the inverse logic.
  Input:
  ```
       {% unless x < 0 %} x is greater than zero {% endunless %}
  ```
  Output:
  ```
  x is greater than zero
  ```
  """
  alias Liquid.IfElse
  alias Liquid.Block
  alias Liquid.Template
  alias Liquid.Condition
  alias Liquid.Tag
  alias Liquid.Render

  @doc """
  Implementation of 'Unless' parse operations
  """
  @spec parse(block :: %Block{}, t :: %Template{}) :: %{}
  def parse(%Block{} = block, %Template{} = t) do
    IfElse.parse(block, t)
  end

  @doc """
  Implementation of 'Unless' render operations
  """
  @spec render(List, %Tag{}, List) :: {List, List}
  def render(output, %Tag{}, context) do
    {output, context}
  end

  def render(
        output,
        %Block{condition: condition, nodelist: nodelist, elselist: elselist},
        context
      ) do
    condition = Condition.evaluate(condition, context)
    conditionlist = if condition, do: elselist, else: nodelist
    Render.render(output, conditionlist, context)
  end
end
