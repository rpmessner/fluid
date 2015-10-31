defmodule Fluid.Comment do
  def parse(%Fluid.Blocks{}=block, %Fluid.Templates{}=template), do: { block, template }
  def render(output, %Fluid.Blocks{}, context), do: { output, context }
end
