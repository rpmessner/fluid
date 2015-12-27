defmodule Liquid.Comment do
  def parse(%Liquid.Block{}=block, %Liquid.Template{}=template), do: { block, template }
  def render(output, %Liquid.Block{}, context), do: { output, context }
end
