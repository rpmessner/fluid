defmodule Fluid.Comment do
  def parse(Fluid.Block[]=block, presets), do: { block, presets }
  def render(output, Fluid.Block[], context), do: { output, context }
end
