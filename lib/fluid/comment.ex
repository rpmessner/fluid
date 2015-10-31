defmodule Fluid.Comment do
  def parse(%Fluid.Block{}=block, %Fluid.Template{}=template), do: { block, template }
  def render(output, %Fluid.Block{}, context), do: { output, context }
end
