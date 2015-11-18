defmodule Liquescent.Comment do
  def parse(%Liquescent.Blocks{}=block, %Liquescent.Templates{}=template), do: { block, template }
  def render(output, %Liquescent.Blocks{}, context), do: { output, context }
end
