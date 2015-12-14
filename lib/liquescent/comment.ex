defmodule Liquescent.Comment do
  def parse(%Liquescent.Block{}=block, %Liquescent.Template{}=template), do: { block, template }
  def render(output, %Liquescent.Block{}, context), do: { output, context }
end
